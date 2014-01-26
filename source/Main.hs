{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
module Main where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Reader
import           DBus
import           DBus (MethodDescription(..))
import           DBus.MessageBus
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Network
import           Network.Xmpp (def)
import qualified Network.Xmpp as Xmpp
import qualified Network.Xmpp.IM as Xmpp
import           System.Directory
import           System.FilePath
import           System.Log.Logger

import           XmppDaemon.Stun

config det = Xmpp.def{Xmpp.sessionStreamConfiguration
                          = Xmpp.def{Xmpp.connectionDetails = det}}

instance DBus.Representable Xmpp.Jid where
    type RepType Xmpp.Jid = 'DBus.DBusSimpleType DBus.TypeString
    toRep = DBus.DBVString . Xmpp.jidToText
    fromRep (DBus.DBVString s) = Xmpp.jidFromText s


throwLeft (Left e) = Ex.throwIO e
throwLeft (Right r) = return $ r

sendMessage :: Xmpp.Session -> Xmpp.Jid -> Text.Text -> IO ()
sendMessage con recipient msg =
    throwLeft =<< Xmpp.sendMessage (Xmpp.simpleIM recipient msg) con

sendMethod :: Xmpp.Session -> DBus.Method
sendMethod con = DBus.Method (DBus.repMethod $ sendMessage con)
                             "sendMessage"
                             ("recipient" :-> "message" :-> Result "")

getAddrMethod con ref = DBus.Method (DBus.repMethod $ findAddr con ref)
                                "getAddr"
                                ("peer" :-> Result "addr answer")

xmppInterface con ref  = Interface "pontarius.xmpp" [ sendMethod con
                                                    , getAddrMethod con ref
                                                    ] []

conObject con ref =
    Object { objectObjectPath = objectPath "pontarius/xmpp/connection"
           , objectInterfaces = [xmppInterface con ref]
           , objectSubObjects = []
           }

root con ref = Object { objectObjectPath = objectPath "/"
                      , objectInterfaces = []
                      , objectSubObjects = [conObject con ref]
                      }

-- | Load the configuration files
loadConfig :: IO Conf.Config
loadConfig = do
    appData <- getAppUserDataDirectory "xmpp-daemon"
    home <- getHomeDirectory
    Conf.load [ Conf.Optional $ appData </> "xmpp-daemon.conf"
              , Conf.Optional $ home </> ".xmpp-daemon.conf"
              ]

jidCompatible :: Xmpp.Jid -> Xmpp.Jid -> Bool
jidCompatible = (==) `on` (Xmpp.localpart &&& Xmpp.domainpart)

findPeer :: Xmpp.Session -> Text.Text -> IO (Maybe Xmpp.Jid)
findPeer session name = do
    is <- Map.elems . Xmpp.items <$> Xmpp.getRoster session
    return $ Xmpp.riJid <$> List.find (\x -> Just name == Xmpp.riName x) is

findFullJid :: Xmpp.Session
            -> Text.Text
            -> TVar (Set.Set Xmpp.Jid)
            -> IO (Maybe Xmpp.Jid)
findFullJid session str ref = do
    mbJ <- case Xmpp.jidFromText str of
        Nothing -> findPeer session str
        Just j -> return $ Just j
    case mbJ of
        Nothing -> return Nothing
        Just j -> if Xmpp.isFull j then return (Just j)
                  else do
                      online <- atomically $ readTVar ref
                      return $ List.find (jidCompatible j) (Set.toList online)

findAddr session ref name = do
    mbFJid <- findFullJid session name ref
    case mbFJid of
        Nothing -> return $ Left "Could not find peer with this JID"
        Just fJid -> getAddr session fJid

handlePresence :: Xmpp.Session
               -> TVar (Set.Set Xmpp.Jid)
               -> (Xmpp.Jid -> IO Bool)
               -> IO b
handlePresence session ref policy = forever $ do
    pres <- Xmpp.pullPresence session
    case pres of
        Right p | Just f <- Xmpp.presenceFrom p
            -> case Xmpp.presenceType p of
                Xmpp.Available -> do atomically $ modifyTVar ref (Set.insert f)
                Xmpp.Unavailable -> do atomically $ modifyTVar ref (Set.delete f)
                Xmpp.Subscribe -> do
                    pol <- policy f
                    when pol . void $ Xmpp.sendPresence (Xmpp.presenceSubscribed f)
                                                 session

        _ -> return ()

instance Conf.Configured [Xmpp.Jid] where
    convert (Conf.List xs) = mapM (Xmpp.jidFromText <=< Conf.convert) xs

main = do
    conf <- loadConfig
    realm <- Conf.require conf "xmpp.realm"
    server <- Conf.lookup conf "xmpp.server"
    port <- Conf.lookup conf "xmpp.port" :: IO (Maybe Integer)
    let conDetails = case server of
            Nothing -> Xmpp.UseRealm
            Just srv -> case port of
                Nothing -> Xmpp.UseSrv srv
                Just p -> Xmpp.UseHost srv (PortNumber $ fromIntegral p)
    uname <- Conf.require conf "xmpp.user"
    pwd <- Conf.require conf "xmpp.password"
    stunServer <- Conf.require conf "stun.server"
    peers <- Conf.require conf "peers"
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel ERROR
    updateGlobalLogger "DBus" $ setLevel ERROR
    let policy j = any (jidCompatible j) peers
    -- handler <- streamHandler stderr DEBUG >>= \h ->
    --     return $ setFormatter h (simpleLogFormatter "$loggername: $msg")
    -- updateGlobalLogger "Pontarius.Xmpp" (addHandler handler)
    sess' <- Xmpp.session realm (Xmpp.simpleAuth uname pwd) (config conDetails)
    sess <- case sess' of
        Left err -> error $ "Error connection to XMPP server: " ++ show err
        Right sess -> return sess
    presRef <- newTVarIO (Set.empty)
    forkIO $ stunHandler stunServer sess (return . policy)
    forkIO $ handlePresence sess presRef (return . policy)
    Xmpp.sendPresence Xmpp.presenceOnline sess
    infoM "Pontarius.Xmpp" "Done connecting to XMPP server, connecting to DBUS"
    con <- connectBus Session
           (\con header bdy -> do
                 print header
                 objectRoot (addIntrospectable $ root sess presRef) con header bdy
           ) (\ _ _ _ -> return ())
    infoM "Pontarius.Xmpp" "Requesting name"
    requestName "xmpp.daemon" def con
    infoM "Pontarius.Xmpp" "Connected to DBus. Ready."
    putStrLn "Running."
    waitFor con
