{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XmppDaemon.State where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Maybe (fromJust)
import           Data.SafeCopy
import qualified Data.Set as Set
import           Data.Typeable
import           Network.Xmpp
import           System.Directory
import           System.FilePath

instance SafeCopy Jid where
    getCopy = contain $ fromJust . jidFromText <$> safeGet
    putCopy j = contain . safePut $ jidToText j
    errorTypeName _ = "Jid"

deriving instance Typeable Jid

data DaemonState = DState { _peers :: Set.Set Jid
                          } deriving (Show)

makeLenses ''DaemonState
deriveSafeCopy 1 'base ''DaemonState


addPeer' :: Jid -> Update DaemonState ()
addPeer' p = peers %= Set.insert (toBare p)

rmPeer' :: Jid -> Update DaemonState ()
rmPeer' p = peers %= Set.delete (toBare p)

hasPeer' :: Jid -> Query DaemonState Bool
hasPeer' p = views peers $ Set.member (toBare p)

getPeers' :: Query DaemonState (Set.Set Jid)
getPeers' = view peers

makeAcidic ''DaemonState ['addPeer', 'rmPeer', 'hasPeer', 'getPeers']

addPeer :: AcidState (EventState AddPeer') -> Jid -> IO ()
addPeer  s = update s . AddPeer'

rmPeer :: AcidState (EventState RmPeer') -> Jid -> IO ()
rmPeer   s = update s . RmPeer'

hasPeer :: AcidState (EventState HasPeer') -> Jid -> IO Bool
hasPeer  s = query s . HasPeer'

getPeers :: AcidState (EventState GetPeers') -> IO (Set.Set Jid)
getPeers s = query s GetPeers'

loadState :: IO (AcidState DaemonState)
loadState = do
    appData <- getAppUserDataDirectory "xmpp-daemon"
    openLocalStateFrom (appData </> "state") (DState Set.empty)
