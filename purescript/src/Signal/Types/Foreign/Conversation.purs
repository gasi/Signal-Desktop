module Signal.Types.Foreign.Conversation
  ( toForeign
  ) where

import Prelude

import Data.Foreign                (Foreign)
import Data.Foreign                as F
import Data.Nullable               (Nullable, toNullable)

import Signal.Types.Conversation   as C
import Signal.Types.VerifiedStatus as VS


type Group =
  { type           :: String
  , active_at      :: Nullable Number
  , avatar         :: Nullable C.Avatar
  , expireTimer    :: Nullable Number
  , groupId        :: String
  , id             :: String
  , lastMessage    :: Nullable String
  , left           :: Boolean
  , members        :: Array String
  , name           :: Nullable String
  , profileSharing :: Boolean
  , timestamp      :: Nullable Number
  , tokens         :: Array String
  , unreadCount    :: Int
  }

type Private =
  { type           :: String
  , active_at      :: Nullable Number
  , avatar         :: Nullable C.Avatar
  , color          :: Nullable String
  , id             :: String
  , lastMessage    :: Nullable String
  , timestamp      :: Nullable Number
  , tokens         :: Array String
  , unreadCount    :: Int
  , verified       :: Int
  }

toForeign :: C.Conversation -> Foreign
toForeign (C.Group o)  = F.toForeign $
  { type           : "group"
  , active_at      : toNullable o.active_at
  , avatar         : toNullable o.avatar
  , expireTimer    : toNullable o.expireTimer
  , groupId        : o.groupId
  , id             : o.id
  , lastMessage    : toNullable o.lastMessage
  , left           : o.left
  , members        : o.members
  , name           : toNullable o.name
  , profileSharing : o.profileSharing
  , timestamp      : toNullable o.timestamp
  , tokens         : o.tokens
  , unreadCount    : o.unreadCount
  } :: Group
toForeign (C.Private o) = F.toForeign $
  { type           : "private"
  , active_at      : toNullable o.active_at
  , avatar         : toNullable o.avatar
  , color          : toNullable o.color
  , id             : o.id
  , lastMessage    : toNullable o.lastMessage
  , timestamp      : toNullable o.timestamp
  , tokens         : o.tokens
  , unreadCount    : o.unreadCount
  , verified       : VS.toInt o.verified
  } :: Private
