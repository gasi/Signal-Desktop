-- TODO: Could this be renamed to ‘Contact’?
module Signal.Types.Conversation
  ( Conversation
  , readConversation
  ) where

import Prelude

import Data.Maybe                   (Maybe, fromMaybe)
import Data.Foreign                 (F, Foreign, ForeignError(..), fail,
                                    readArray, readInt, readNullOrUndefined,
                                    readNumber, readString)
import Data.Foreign.Index           ((!))
import Data.Record.ShowRecord       (showRecord)
import Data.Traversable             (traverse)

import Signal.Types.VerifiedStatus  as VerifiedStatus
import Signal.Types.VerifiedStatus  (VerifiedStatus)
import Signal.Types.ArrayBuffer     (ArrayBuffer, readArrayBuffer)

newtype Avatar = Avatar
  { contentType :: String -- MIME
  , data        :: ArrayBuffer
  , size        :: Number -- Int
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
  size        <- value ! "size"        >>= readNumber
  pure $ Avatar
    { contentType : contentType
    , data        : data_
    , size        : size
    }

data Conversation
  = Private
    { active_at     :: Maybe Number -- Not set if never active
    , avatar        :: Maybe Avatar -- How does this relate to `profileAvatar`?
    , color         :: Maybe String -- ?
    , id            :: String       -- PhoneNumber
    , lastMessage   :: Maybe String
    , profileAvatar :: Maybe Avatar -- How does this relate to `avatar`?
    , profileKey    :: Maybe ArrayBuffer
    , profileName   :: Maybe String
    , timestamp     :: Maybe Number
    , tokens        :: Array String -- Used for search?
    , unreadCount   :: Int
    , verified      :: VerifiedStatus
    }
  | Group
    { active_at      :: Maybe Number -- Not set if never active
    , avatar         :: Maybe Avatar -- How does this relate to `profileAvatar`?
    , expireTimer    :: Maybe Number -- Seconds?
    , groupId        :: String       -- Consolidate with `id`?
    , id             :: String       -- Consolidate with `groupId`?
    , lastMessage    :: Maybe String
    , left           :: Boolean
    , members        :: Array String -- NonEmptyList PhoneNumber
    , name           :: Maybe String
    , profileSharing :: Boolean
    , timestamp      :: Maybe Number
    , tokens         :: Array String -- Used for search?
    , unreadCount    :: Int
    -- No group concept of `verified`
    }

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
  profileAvatar <- value ! "profileAvatar" >>= optional readAvatar
  profileKey    <- value ! "profileKey"    >>= optional readArrayBuffer
  profileName   <- value ! "profileName"   >>= optional readString
  timestamp     <- value ! "timestamp"     >>= optional readNumber
  tokens        <- value ! "tokens"        >>= readArray >>= traverse readString
  unreadCount   <- value ! "unreadCount"   >>= optional readInt
  verified      <- value ! "verified"      >>= optional readInt
  pure $ Private
    { active_at     : active_at
    , avatar        : avatar
    , color         : color
    , id            : id_
    , lastMessage   : lastMessage
    , profileAvatar : profileAvatar
    , profileKey    : profileKey
    , profileName   : profileName
    , timestamp     : timestamp
    , tokens        : tokens
    , unreadCount   : fromMaybe 0 unreadCount
    , verified      : VerifiedStatus.fromIntWithDefault verified
    }

readConversation :: Foreign -> F Conversation
readConversation value = do
  type_ <- value ! "type" >>= readString
  case type_ of
    "private" -> readPrivate value
    -- "group"   -> readGroup value
    _         -> fail $ ForeignError $ "Unknown message type: '" <> type_ <> "'"
