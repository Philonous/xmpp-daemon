{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module XmppDaemon.Stun where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Data.XML.Pickle
import           Data.XML.Types
import           Network.Socket
import           Network.Stun
import           Network.Xmpp
import           System.Log.Logger

data GetStunError = GetStunError String
                    deriving (Show, Typeable)

instance Ex.Exception GetStunError

stunError :: String -> IO b
stunError x = do
    errorM "Pontarius.Xmpp.Daemon.Stun" x
    Ex.throwIO $ GetStunError x

getReflAddr :: HostName -> IO (Either Text.Text Text.Text)
getReflAddr stunServer =
    Ex.handle (\(GetStunError str) -> return . Left $ Text.pack str)
    . fmap (Right . Text.pack) $
    getAddrInfo Nothing (Just stunServer) (Just "stun") >>= \case
        [] -> stunError $ "Error: Could not find server " ++ stunServer
        (h:_) -> do
            findMappedAddress (addrAddress h) 0 [] >>= \case
                Left e -> do
                    stunError $ "Error: stun returned error " ++ show e
                Right (r, _) -> case r of
                    SockAddrInet{} -> return . takeWhile (/= ':') $ show r
                    SockAddrInet6{} -> return . tail . takeWhile (/= ']') $ show r
                    _ -> return $ show r

getAddrE :: Element
getAddrE = Element "{org.pontarius.xmpp.daemon}get-refl-addr" [] []

xpAddrString :: PU Element (Either Text.Text Text.Text)
xpAddrString = xpRoot . xpUnliftElems $
    xpElemNodes "{org.pontarius.xmpp.daemon}refl-addr"
                  (xpEither (xpElemNodes "{org.pontarius.xmpp.daemon}error"
                                         (xpContent xpText))
                            (xpElemNodes "{org.pontarius.xmpp.daemon}addr"
                                         (xpContent xpText)))

stunHandler :: HostName -> Session -> (Jid -> IO Bool) -> IO ()
stunHandler host session policy = do
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
                    mbReflAddr <- getReflAddr host
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
