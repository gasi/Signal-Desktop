-- TODO: Could this be renamed to ‘Contact’?
module Signal.Types.Conversation
  ( Conversation(..)
  , readConversation
  , Avatar
  , isActive
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readInt, readNullOrUndefined, readNumber, readString)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Record.ShowRecord (showRecord)
import Data.Traversable (traverse)
import Signal.Types.ArrayBuffer (ArrayBuffer, readArrayBuffer)
import Signal.Types.Timestamp (Timestamp, readTimestamp)
import Signal.Types.VerifiedStatus (VerifiedStatus)
import Signal.Types.VerifiedStatus as VerifiedStatus

newtype Avatar = Avatar
  { contentType :: String -- MIME
  , data        :: ArrayBuffer
  , size        :: Int
  }

instance showAvatar :: Show Avatar where
  show (Avatar o) =
    "(Avatar" <>
    " contentType: " <> show o.contentType <> "," <>
    " size: " <> show o.size <>
    ")"

readAvatar :: Foreign -> F Avatar
readAvatar value = do
    contentType <- value ! "contentType" >>= readString
    data_       <- value ! "data"        >>= readArrayBuffer
    size        <- parseSize value
    pure $ Avatar
      { contentType
      , data : data_
      , size
      }
  where
    parseSize :: Foreign -> F Int
    parseSize o = do
      size    <- value ! "size"   >>= optional readInt
      length_ <- value ! "length" >>= optional readInt
      case (size <|> length_) of
        Just n  -> pure n
        Nothing ->
          fail $ ForeignError "`Avatar::size` or `Avatar::length` is required"

derive instance eqAvatar :: Eq Avatar

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

instance showConversation :: Show Conversation where
  show (Private o) = "(Private " <> showRecord o <> ")"
  show (Group o)   = "(Group "   <> showRecord o <> ")"

optional :: forall a. (Foreign -> F a) -> Foreign -> F (Maybe a)
optional f x = readNullOrUndefined x >>= traverse f

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
    { active_at      : active_at
    , avatar         : avatar
    , expireTimer    : expireTimer
    , groupId        : groupId
    , id             : id_
    , lastMessage    : lastMessage
    , left           : fromMaybe false left
    , members        : members
    , name           : name
    , profileSharing : fromMaybe false profileSharing
    , timestamp      : timestamp
    , tokens         : tokens
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
