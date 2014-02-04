{-# LANGUAGE OverloadedStrings #-}
module Obex.MessageAccess
    (
    -- * Session Handling
      BluetoothAddr, MapSession
    , newSession, reuseSession, closeSession

    -- * Interaction with Session
    , Obex.MessageAccess.setFolder
    , Obex.MessageAccess.listFolders
    , Obex.MessageAccess.listMessages
    , Obex.MessageAccess.updateInbox

    -- * Filters
    , Filter(..), TypeFilter(..)

    -- * Messages
    , Message(..), LinkedMessage
    , extractMessage
    , getFolder, getDeleted, getPriority, getProtected, getSent, getRead, setRead
    ) where

import Data.Maybe
import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.Text as TS

import DBus (toVariant, fromVariant)
import DBus.Client

import Control.Monad

import Obex.MessageAccess.Internal

-- | Represents a Bluetooth adapter address. Should be in a format recognized by
-- bluez, for example @01:23:45:67:89@.
type BluetoothAddr = TS.Text

-- | Creates a new Message-Access-Protocol session. This will create a
-- connection to the D-Bus session bus and ask bluez bluetoothd for a new
-- connection to the specified Bluetooth address.
newSession :: BluetoothAddr -> IO MapSession
newSession bt = do
    client <- connectSession
    path   <- createMapSession client bt
    return $ MapSession client path True

-- | Like 'newSession' but reuses an existing D-Bus client.
reuseSession :: Client -> BluetoothAddr -> IO MapSession
reuseSession client bt = do
    path <- createMapSession client bt
    return $ MapSession client path False

-- | Closes a Message-Access-Protocol session. This will also disconnect the
-- D-Bus client unless the session was created with 'reuseSession'.
closeSession :: MapSession -> IO ()
closeSession (MapSession client path closable) = do
    removeSession client path
    when closable $ disconnect client

-- | Changes the current working directory.
setFolder :: MapSession -> TS.Text -> IO ()
setFolder (MapSession c p _) = Obex.MessageAccess.Internal.setFolder c p

-- | Lists all folders in the current working directory. Only folders matching
-- the given filters are considered.
listFolders :: MapSession -> [Filter] -> IO [TS.Text]
listFolders (MapSession c p _) fs = do
    let dict = filtersToDict fs
    dict' <- Obex.MessageAccess.Internal.listFolders c p dict
    return $ mapMaybe (fromVariant <=< M.lookup "Name") dict'

-- | Lists all messages matching the given filters.
listMessages :: MapSession -> TS.Text -> [Filter] -> IO [LinkedMessage]
listMessages (MapSession c p _) folder fs = do
    let dict = filtersToDict fs
    dict' <- Obex.MessageAccess.Internal.listMessages c p folder dict
    return . mapMaybe dictToLinkedMessage $ M.toList dict'

-- | Asks the device to update the inbox.
updateInbox :: MapSession -> IO ()
updateInbox (MapSession c p _) = Obex.MessageAccess.Internal.updateInbox c p

-- | Returns the message payload.
extractMessage :: LinkedMessage -> Message
extractMessage (LinkedMessage _ m) = m

getFolder :: MapSession -> LinkedMessage -> IO TS.Text
getFolder (MapSession c _ _) (LinkedMessage p _)
    = fromJust . fromVariant <$> getMessageProperty c p "Folder"

getDeleted :: MapSession -> LinkedMessage -> IO Bool
getDeleted (MapSession c _ _) (LinkedMessage p _)
    = fromJust . fromVariant <$> getMessageProperty c p "Deleted"

getPriority :: MapSession -> LinkedMessage -> IO Bool
getPriority (MapSession c _ _) (LinkedMessage p _)
    = fromJust . fromVariant <$> getMessageProperty c p "Priority"

getSent :: MapSession -> LinkedMessage -> IO Bool
getSent (MapSession c _ _) (LinkedMessage p _)
    = fromJust . fromVariant <$> getMessageProperty c p "Sent"

getProtected :: MapSession -> LinkedMessage -> IO Bool
getProtected (MapSession c _ _) (LinkedMessage p _)
    = fromJust . fromVariant <$> getMessageProperty c p "Protected"

getRead :: MapSession -> LinkedMessage -> IO Bool
getRead (MapSession c _ _) (LinkedMessage p _)
    = fromJust . fromVariant <$> getMessageProperty c p "Read"

setRead :: MapSession -> LinkedMessage -> Bool -> IO ()
setRead (MapSession c _ _) (LinkedMessage p _) v
    = setMessageProperty c p "Read" $ toVariant v

-- vim: set ts=4 sts=4 sw=4 et:
