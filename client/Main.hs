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

main = do
    peer <- getArgs >>= \case
        [p] -> return p
        _ -> do hPutStrLn stderr "usage: xmpp-client peer"
                exitFailure
    con <- connectBus Session (\_ _ _ -> return ()) (\_ _ _ -> return ())
    eitherIP <- callMethod' "xmpp.daemon" (objectPath "/pontarius/xmpp/connection")
                            "pontarius.xmpp" "getAddr"
                            [DBV . DBVString $ Text.pack peer]
                            [] con
    case eitherIP of
        Right ip -> putStrLn $ Text.unpack ip
        Left error -> hPutStrLn stderr $ Text.unpack error
