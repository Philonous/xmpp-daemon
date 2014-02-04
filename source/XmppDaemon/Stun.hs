{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module XmppDaemon.Stun where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.IP as IP
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable)
import           Data.XML.Pickle
import           Data.XML.Types
import           Network
import           Network.DNS
import           Network.Socket
import           Network.Stun
import           Network.Xmpp
import           System.Log.Logger

data GetStunError = GetStunError String
                    deriving (Show, Typeable)

instance Ex.Exception GetStunError

getReflAddr :: HostName -> Maybe PortNumber -> IO (Either Text.Text Text.Text)
getReflAddr stunServer mbPn = do
    rs <- makeResolvSeed defaultResolvConf
    r <- withResolver rs $ \resolver -> do
        let domain = Text.encodeUtf8 $ Text.pack stunServer
            srvDomain = "_stun._udp." <> domain <> "."
        srv' <- fromEither =<< lookupSRV resolver srvDomain
        srv <- if (null srv')
               then do
                   infoM "Pontarius.Xmpp.Daemon.Stun" $
                       "Did not find any SRV entries for " ++ show srvDomain
                   return [(fromMaybe 3478 mbPn, domain)]
               else
                 return $ (\(_,_,p,dm) -> (fromIntegral p,dm)) <$> srv'
        tryAll srv $ \(p, dom) -> do
            infoM "Pontarius.Xmpp.Daemon.Stun" $
                       "Looking up " ++ show dom
            as <- fromEither =<< lookupA resolver dom
            aaaas <- fromEither =<< lookupAAAA resolver dom

            let addrs = (toSockAddr p <$> as) ++ (toSockAddr6 p <$> aaaas)
            case addrs of
                [] -> return $ Left (GetStunError "Neither A nor AAAA record \
                                          \returned a result")
                addrs' -> tryAll addrs' $ \addr -> do
                    infoM "Pontarius.Xmpp.Daemon.Stun" $
                       "Sending STUN request to " ++ show addr
                    findMappedAddress addr 0 []
    return $ mapLeft (Text.pack . show) $ for r $ \x -> Text.pack $ case fst x of
        SockAddrInet{} -> takeWhile (/= ':') $ show r
        SockAddrInet6{} -> tail . takeWhile (/= ']') $ show r
        r' -> show r'
  where
    tryAll :: Show e => [a] -> (a -> IO (Either e b)) -> IO (Either GetStunError b)
    tryAll []     f = return . Left . GetStunError
                        $ "Error: Could not connect to host " ++ stunServer
    tryAll (x:xs) f = do
        res <- f x
        case res of
            Right r -> return $ Right r
            Left e -> do
                errorM "Pontarius.Xmpp.Daemon.Stun" $ "error: " ++ (show e)
                tryAll xs f
    fromEither (Left e) = do
        errorM "Pontarius.Xmpp.Daemon.Stun" $ "DNS error: " ++ (show e)
        return []
    fromEither (Right r) = return r
    toSockAddr p ipv4 = SockAddrInet p (IP.toHostAddress ipv4)
    toSockAddr6 p ipv6 = SockAddrInet6 p 0 (IP.toHostAddress6 ipv6) 0
    for = flip fmap
    mapLeft f (Left l) = Left $ f l
    mapLeft _ (Right r) = Right r

getAddrE :: Element
getAddrE = Element "{org.pontarius.xmpp.daemon}get-refl-addr" [] []

xpAddrString :: PU Element (Either Text.Text Text.Text)
xpAddrString = xpRoot . xpUnliftElems $
    xpElemNodes "{org.pontarius.xmpp.daemon}refl-addr"
                  (xpEither (xpElemNodes "{org.pontarius.xmpp.daemon}error"
                                         (xpContent xpText))
                            (xpElemNodes "{org.pontarius.xmpp.daemon}addr"
                                         (xpContent xpText)))

stunHandler :: HostName
            -> Maybe PortNumber
            -> Session
            -> (Jid -> IO Bool)
            -> IO ()
stunHandler host mbPort session policy = do
    eitherChan <- listenIQChan Get "org.pontarius.xmpp.daemon" session
    case eitherChan of
        Left _ -> return ()
        Right chan -> forever $ do
            request <- atomically $ readTChan chan
            pol <- case iqRequestFrom $ iqRequestBody request of
                Nothing -> return False
                Just j -> policy j
            case pol of
                True -> do
                    mbReflAddr <- getReflAddr host mbPort
                    answerIQ request . Right . Just
                        $ pickle xpAddrString mbReflAddr
                    return ()
                False -> do
                    answerIQ request . Left $ mkStanzaError ServiceUnavailable
                    return ()

getAddr :: Session -> Jid -> IO (Either Text.Text Text.Text)
getAddr session recipient = do
    res <- sendIQ' (Just (5*10^6)) (Just recipient) Get Nothing getAddrE session
    case res of
        Left (IQSendError e) -> return . Left $ "Could not send request "
                                             `Text.append` Text.pack (show e)
        Left IQTimeOut -> return $ Left "Request timed out"
        Right (IQResponseError e) -> return . Left $ "Received XMPP error"
                                       `Text.append` Text.pack (show e)
        Right (IQResponseResult r) -> case iqResultPayload r of
            Nothing -> return . Left $ "Response did not contain data"
            Just r -> case unpickle xpAddrString r of
                Left _ -> return . Left $ "Response data is invalid"
                Right r -> return r
