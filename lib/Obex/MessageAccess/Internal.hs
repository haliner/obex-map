{-# LANGUAGE OverloadedStrings #-}
module Obex.MessageAccess.Internal where

import Control.Monad

import Data.Default
import Data.Functor
import Data.Time.Format
import Data.Time.LocalTime
import Data.Word
import qualified Data.Map.Strict as M
import qualified Data.Text as TS

import DBus hiding (Message)
import DBus.Client

-- * Low Level D-Bus Interface

type ObjectPathDict = M.Map ObjectPath
type TextDict = M.Map TS.Text
type Dict = TextDict Variant

wellKnownName :: BusName
wellKnownName = "org.bluez.obex"

obexObject :: ObjectPath
obexObject = "/org/bluez/obex"

clientInterface :: InterfaceName
clientInterface = "org.bluez.obex.Client1"

messageInterface :: InterfaceName
messageInterface = "org.bluez.obex.Message1"

messageAccessInterface :: InterfaceName
messageAccessInterface = "org.bluez.obex.MessageAccess1"

propertyInterface :: InterfaceName
propertyInterface = "org.freedesktop.DBus.Properties"

createMapSession :: Client -> TS.Text -> IO ObjectPath
createMapSession c btaddr = do
    [v] <- methodReturnBody <$> call_ c m
    let Just dict = fromVariant v
    return dict
  where
    args = M.singleton ("Target" :: TS.Text) (toVariant ("map" :: TS.Text))
    m = (methodCall obexObject clientInterface "CreateSession")
            { methodCallDestination = Just wellKnownName
            , methodCallBody = [ toVariant btaddr
                               , toVariant args
                               ]
            }

removeSession :: Client -> ObjectPath -> IO ()
removeSession c p = do
    [] <- methodReturnBody <$> call_ c m
    return ()
  where
    m = (methodCall obexObject clientInterface "RemoveSession")
            { methodCallDestination = Just wellKnownName
            , methodCallBody = [ toVariant p ]
            }

setFolder :: Client -> ObjectPath -> TS.Text -> IO ()
setFolder c p folder = do
    [] <- methodReturnBody <$> call_ c m
    return ()
  where
    m = (methodCall p messageAccessInterface "SetFolder")
            { methodCallDestination = Just wellKnownName
            , methodCallBody = [ toVariant folder ]
            }

listFolders :: Client -> ObjectPath -> Dict -> IO [Dict]
listFolders c p filters = do
    [v] <- methodReturnBody <$> call_ c m
    let Just dict = fromVariant v
    return dict
  where
    m = (methodCall p messageAccessInterface "ListFolders")
            { methodCallDestination = Just wellKnownName
            , methodCallBody = [ toVariant filters ]
            }

listMessages :: Client -> ObjectPath -> TS.Text -> Dict -> IO (ObjectPathDict Dict)
listMessages c p folder filters = do
    [v] <- methodReturnBody <$> call_ c m
    let Just outerDict = fromVariant v
    return outerDict
  where
    m = (methodCall p messageAccessInterface "ListMessages")
            { methodCallDestination = Just wellKnownName
            , methodCallBody = [ toVariant folder
                               , toVariant filters
                               ]
           }

updateInbox :: Client -> ObjectPath -> IO ()
updateInbox c p = callNoReply c m
  where
    m = (methodCall p messageAccessInterface "UpdateInbox")
            { methodCallDestination = Just wellKnownName
            }

getMessageProperty :: Client -> ObjectPath -> TS.Text -> IO Variant
getMessageProperty c p n = do
    [v] <- methodReturnBody <$> call_ c m
    let Just v' = fromVariant v
    return v'
  where
    m = (methodCall p propertyInterface "Get")
            { methodCallDestination = Just wellKnownName
            , methodCallBody = [ toVariant messageInterface
                               , toVariant n
                               ]
            }

setMessageProperty :: Client -> ObjectPath -> TS.Text -> Variant -> IO ()
setMessageProperty c p n v = do
    [] <- methodReturnBody <$> call_ c m
    return ()
  where
    m = (methodCall p propertyInterface "Set")
            { methodCallDestination = Just wellKnownName
            , methodCallBody = [ toVariant messageInterface
                               , toVariant n
                               , toVariant v
                               ]
            }

-- * High Level D-Bus Interface

-- | Opaque type to represent an open Message-Access-Protocol session. Use
-- 'Obex.MessageAccess.newSession' and 'Obex.MessageAccess.reuseSession' to
-- create a new session.
data MapSession = MapSession Client ObjectPath Bool

-- * Filters

data TypeFilter = SMSType | MMSType | EMailType deriving (Read, Show)

data Filter
    = OffsetFilter     Int
    | CountFilter      Int
    | LengthFilter     Int
    | BeginFilter      LocalTime
    | EndFilter        LocalTime
    | ReadFilter       Bool
    | ReceipientFilter TS.Text
    | SenderFilter     TS.Text
    | PriorityFilter   Bool
    deriving (Read, Show)

filterName :: Filter -> TS.Text
filterName f = case f of
    OffsetFilter     _ -> "Offset"
    CountFilter      _ -> "MaxCount"
    LengthFilter     _ -> "SubjectLength"
    BeginFilter      _ -> "PeriodBegin"
    EndFilter        _ -> "PeriodEnd"
    ReadFilter       _ -> "Read"
    ReceipientFilter _ -> "Receipient"
    SenderFilter     _ -> "Sender"
    PriorityFilter   _ -> "Priority"

filterValueToVariant :: Filter -> Variant
filterValueToVariant f = case f of
    OffsetFilter     i -> toVariant (fromIntegral i :: Word16)
    CountFilter      i -> toVariant (fromIntegral i :: Word16)
    LengthFilter     i -> toVariant (fromIntegral i :: Word8)
    BeginFilter      t -> toVariant (timeToText t)
    EndFilter        t -> toVariant (timeToText t)
    ReadFilter       b -> toVariant b
    ReceipientFilter s -> toVariant s
    SenderFilter     s -> toVariant s
    PriorityFilter   b -> toVariant b

timeFormat :: String
timeFormat = "%Y%m%dT%H%M%S"

timeToText :: LocalTime -> TS.Text
timeToText = TS.pack . formatTime def timeFormat

textToTime :: TS.Text -> Maybe LocalTime
textToTime = parseTime def timeFormat . TS.unpack

filtersToDict :: [Filter] -> M.Map TS.Text Variant
filtersToDict = M.fromList . map (\f -> (filterName f, filterValueToVariant f))

-- * Messages

-- | Please note that 'MMS' and 'EMail' are currently not implemented.
data Message
    = SMS
        { sender        :: TS.Text   -- ^ Name of sender.
        , senderTel     :: TS.Text   -- ^ Phone number of sender.
        , recipient     :: TS.Text   -- ^ Name of receipient.
        , recipientTel  :: TS.Text   -- ^ Phone number of receipient.
        , timestamp     :: LocalTime -- ^ Timestamp of message.
        , content       :: TS.Text   -- ^ Message content.
        }
    | MMS
    | EMail
    deriving (Read, Show)

-- | Represents a message which is linked to a real message on the remote
-- device. Use 'Obex.MessageAccess.extractMessage' to get the message payload.
data LinkedMessage = LinkedMessage ObjectPath Message

variantLookup :: (Ord k, IsVariant v) => k -> M.Map k Variant -> Maybe v
variantLookup k = fromVariant <=< M.lookup k

dictToLinkedMessage :: (ObjectPath, Dict) -> Maybe LinkedMessage
dictToLinkedMessage (p, d) = LinkedMessage p <$> dictToMessage d

dictToMessage :: Dict -> Maybe Message
dictToMessage d = do
    msgType <- variantLookup "Type" d :: Maybe TS.Text
    case msgType of
        "email"    -> Just EMail
        "mms"      -> Just MMS
        "sms-gsm"  -> smsMessage d
        "sms-cdma" -> smsMessage d
        _          -> Nothing

smsMessage :: Dict -> Maybe Message
smsMessage d = do
    sender'       <- variantLookup "Sender" d
    senderTel'    <- variantLookup "SenderAddress" d
    recipient'    <- variantLookup "Recipient" d
    recipientTel' <- variantLookup "RecipientAddress" d
    timestamp'    <- textToTime =<< variantLookup "Timestamp" d
    content'      <- variantLookup "Subject" d
    Just $ SMS sender' senderTel' recipient' recipientTel' timestamp' content'

-- vim: set ts=4 sts=4 sw=4 et:
