{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Network.Xmpp (jid, Jid)
import           Data.Text as Text

import           DBus
import           DBus.Message
import           DBus.MessageBus
import           DBus.Types
import qualified Data.Text as Text
import           System.Environment
import           System.Exit
import           System.IO

getIP peer = do
    con <- connectBus Session (\_ _ _ -> return ()) (\_ _ _ -> return ())
    eitherIP <- callMethod' "xmpp.daemon" (objectPath "/pontarius/xmpp/connection")
                            "pontarius.xmpp" "getAddr"
                            [DBV . DBVString $ Text.pack peer]
                            [] con
    case eitherIP of
        Right ip -> putStrLn $ Text.unpack ip
        Left error -> hPutStrLn stderr $ Text.unpack error

addPeer peer = do
    con <- connectBus Session (\_ _ _ -> return ()) (\_ _ _ -> return ())
    () <- callMethod' "xmpp.daemon" (objectPath "/pontarius/xmpp/connection")
                            "pontarius.xmpp" "addPeer"
                            [DBV . DBVString $ Text.pack peer]
                            [] con
    return ()

printUsage = do
    putStr "usage: xmpp-client command args "
    putStr "Commands: "
    putStr "  getip   peer (jid)"
    putStr "  addpeer peer (jid)"
    exitFailure


main = getArgs >>= \case
        [] -> printUsage
        (cmd:args) -> case cmd of
            "getip" -> case args of
                [p] -> getIP p
                _ -> printUsage
            "addpeer" ->  case args of
                [p] -> addPeer p
                _ -> printUsage
            _ -> printUsage
