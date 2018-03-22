-- TODO: Could this be renamed to ‘Contact’?
module Signal.Types.Conversation
  ( Conversation(..)
  , compareTitle
  , getNumber
  , getTimestamp
  , getTitle
  , isActive
  , readConversation
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readInt, readNumber, readString)
import Data.Foreign.Index ((!))
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Record.ShowRecord (showRecord)
import Data.Traversable (traverse)

import I18n.PhoneNumbers.PhoneNumber as PN
import Signal.Foreign (optional)
import Signal.Types.ArrayBuffer (ArrayBuffer, readArrayBuffer)
import Signal.Types.Avatar (Avatar, readAvatar)
import Signal.Types.Timestamp (Timestamp, readTimestamp)
import Signal.Types.VerifiedStatus (VerifiedStatus)
import Signal.Types.VerifiedStatus as VerifiedStatus


type RegionCode = String

data Conversation
  = Private
    { active_at     :: Maybe Number -- Not set if never active
    , avatar        :: Maybe Avatar -- How does this relate to `profileAvatar`?
    , color         :: Maybe String -- ?
    , id            :: String       -- PhoneNumber
    , lastMessage   :: Maybe String
    , name          :: Maybe String -- Consolidate with `profileName`?
    , profileAvatar :: Maybe Avatar -- How does this relate to `avatar`?
    , profileKey    :: Maybe ArrayBuffer
    , profileName   :: Maybe String -- Consolidate with `name`?
    , timestamp     :: Maybe Timestamp
    , tokens        :: Array String -- Used for search?
    , unreadCount   :: Int
    , verified      :: VerifiedStatus
    }
  | Group
    { active_at      :: Maybe Number -- Not set if never active
    , avatar         :: Maybe Avatar -- How does this relate to `profileAvatar`?
    , expireTimer    :: Maybe Number -- Seconds?
    , groupId        :: Maybe String -- Consolidate with `id`?
    , id             :: String       -- Consolidate with `groupId`?
    , lastMessage    :: Maybe String
    , left           :: Boolean
    , members        :: Array String -- NonEmptyList PhoneNumber
    , name           :: Maybe String
    , profileSharing :: Boolean
    , timestamp      :: Maybe Timestamp
    , tokens         :: Array String -- Used for search?
    , unreadCount    :: Int
    -- No group concept of `verified`
    }

derive instance eqConversation :: Eq Conversation

foreign import compareTitleImpl :: Fn5 Ordering Ordering Ordering String String Ordering

compareTitle :: String -> String -> Ordering
compareTitle = runFn5 compareTitleImpl EQ LT GT

instance showConversation :: Show Conversation where
  show (Private o) = "(Private " <> showRecord o <> ")"
  show (Group o)   = "(Group "   <> showRecord o <> ")"

getTimestamp :: Conversation -> Maybe Timestamp
getTimestamp (Private o) = o.timestamp
getTimestamp (Group o)   = o.timestamp

getTitle :: Maybe RegionCode -> Conversation -> String
getTitle rc p@(Private o) = fromMaybe (getNumber rc p) o.name
getTitle rc g@(Group o)   = fromMaybe "Unknown group" o.name

getNumber :: Maybe RegionCode -> Conversation -> String
getNumber rc (Private o) = maybe rawNumber (PN.format format) phoneNumber
  where
  format | regionCode == rc = PN.National
         | otherwise        = PN.International
  regionCode                = phoneNumber >>= PN.regionCodeForNumber
  phoneNumber               = PN.parse rawNumber
  rawNumber                 = o.id

getNumber _ _            = ""

--                          Private
readPrivate :: Foreign -> F Conversation
readPrivate value = do
  active_at     <- value ! "active_at"     >>= optional readNumber
  avatar        <- value ! "avatar"        >>= optional readAvatar
  color         <- value ! "color"         >>= optional readString
  id_           <- value ! "id"            >>= readString
  lastMessage   <- value ! "lastMessage"   >>= optional readString
  name          <- value ! "name"          >>= optional readString
  profileAvatar <- value ! "profileAvatar" >>= optional readAvatar
  profileKey    <- value ! "profileKey"    >>= optional readArrayBuffer
  profileName   <- value ! "profileName"   >>= optional readString
  timestamp     <- value ! "timestamp"     >>= optional readTimestamp
  tokens        <- value ! "tokens"        >>= readArray >>= traverse readString
  unreadCount   <- value ! "unreadCount"   >>= optional readInt
  verified      <- value ! "verified"      >>= optional readInt
  pure $ Private
    { active_at
    , avatar
    , color
    , id            : id_
    , lastMessage
    , name
    , profileAvatar
    , profileKey
    , profileName
    , timestamp
    , tokens
    , unreadCount   : fromMaybe 0 unreadCount
    , verified      : VerifiedStatus.fromIntWithDefault verified
    }

--                        Group
readGroup :: Foreign -> F Conversation
readGroup value = do
  active_at      <- value ! "active_at"      >>= optional readNumber
  avatar         <- value ! "avatar"         >>= optional readAvatar
  expireTimer    <- value ! "expireTimer"    >>= optional readNumber
  groupId        <- value ! "groupId"        >>= optional readString
  id_            <- value ! "id"             >>= readString
  lastMessage    <- value ! "lastMessage"    >>= optional readString
  left           <- value ! "left"           >>= optional readBoolean
  members        <- value ! "members"        >>= readArray >>= traverse readString
  name           <- value ! "name"           >>= optional readString
  profileSharing <- value ! "profileSharing" >>= optional readBoolean
  timestamp      <- value ! "timestamp"      >>= optional readTimestamp
  tokens         <- value ! "tokens"         >>= readArray >>= traverse readString
  unreadCount    <- value ! "unreadCount"    >>= optional readInt
  pure $ Group
    { active_at
    , avatar
    , expireTimer
    , groupId
    , id             : id_
    , lastMessage
    , left           : fromMaybe false left
    , members
    , name
    , profileSharing : fromMaybe false profileSharing
    , timestamp
    , tokens
    , unreadCount    : fromMaybe 0 unreadCount
    }

readConversation :: Foreign -> F Conversation
readConversation value = do
  type_ <- value ! "type" >>= optional readString
  case type_ of
    Just "private" -> readPrivate value
    Just "group"   -> readGroup value
    Just t         -> fail $ ForeignError $ "Unknown message type: '" <> t <> "'"
    _              -> fail $ ForeignError $ "Missing message type"

isActive :: Conversation -> Boolean
isActive (Group o)   = isJust o.active_at
isActive (Private o) = isJust o.active_at
