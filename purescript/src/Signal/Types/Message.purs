module Signal.Types.Message
  ( Message
  , readMessage
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readInt,
                    readNullOrUndefined, readNumber, readString)
import Data.Foreign.Index ((!))
import Data.Record.ShowRecord (showRecord)
import Data.Traversable (traverse)

import Signal.Types.Attachment (Attachment, readAttachment)

data Message
  = Incoming
    { attachments    :: Array Attachment
    , body           :: Maybe String
    , conversationId :: String
    , decrypted_at   :: Maybe Number -- TODO: Make `Int`?
    , errors         :: Maybe (Array Unit) -- TODO: ?
    , flags          :: Maybe Int
    , id             :: String
    , received_at    :: Number -- TODO: Make `Int`?
    , sent_at        :: Number -- TODO: Make `Int`?
    , source         :: Maybe String -- `PhoneNumber`
    , sourceDevice   :: Maybe Int
    , timestamp      :: Number -- TODO: Make `Int`?
    }
  | Outgoing
    { attachments              :: Array Attachment
    , body                     :: Maybe String
    , conversationId           :: String
    , dataMessage              :: Maybe {} -- TODO: ?
    , delivered                :: Int
    , delivered_to             :: Array String -- `PhoneNumber`
    , destination              :: String -- `PhoneNumber`
    , expirationStartTimestamp :: Number -- TODO: Make`Int`?
    , expireTimer              :: Maybe Int -- `Duration`
    , expires_at               :: Maybe Number
    , id                       :: String
    , received_at              :: Number -- TODO: Make `Int`?
    , recipients               :: Maybe (Array String)
    , sent                     :: Boolean
    , sent_at                  :: Number -- TODO: Make `Int`?
    , sent_to                  :: Array String -- `PhoneNumber`
    , synced                   :: Boolean
    , timestamp                :: Number -- TODO: Make `Int`?
    }
  | KeyChange
    { id              :: String
    , conversationId  :: String
    , key_changed     :: String -- `PhoneNumber`
    , received_at     :: Number
    , sent_at         :: Maybe Number
    , timestamp       :: Number
    , unread          :: Int
    }
  | VerifiedChange
    { id              :: String
    , verifiedChanged :: String -- `PhoneNumber`
    , conversationId  :: String
    , local           :: Boolean
    , timestamp       :: Number
    , sent_at         :: Number
    , received_at     :: Number
    , unread          :: Int
    , verified        :: Boolean
    }

instance showMessage :: Show Message where
  show (Incoming o)  = "(Incoming " <> showRecord o <> ")"
  show (KeyChange o) = "(KeyChange " <> showRecord o <> ")"
  show _             = "(Message)"

--                           Incoming
readIncoming :: Foreign -> F Message
readIncoming value = do
  attachments    <- value ! "attachments" >>= readArray >>= traverse readAttachment
  body           <- value ! "body" >>= readNullOrUndefined >>= traverse readString
  conversationId <- value ! "conversationId" >>= readString
  decrypted_at   <- value ! "decrypted_at" >>= readNullOrUndefined >>= traverse readNumber
  flags          <- value ! "flags" >>= readNullOrUndefined >>= traverse readInt
  -- errors         <- value ! "errors" >>= readArray >>= traverse ...
  id             <- value ! "id" >>= readString
  received_at    <- value ! "received_at" >>= readNumber
  sent_at        <- value ! "sent_at" >>= readNumber
  source         <- value ! "source" >>= readNullOrUndefined >>= traverse readString
  sourceDevice   <- value ! "sourceDevice" >>= readNullOrUndefined >>= traverse readInt
  timestamp      <- value ! "timestamp" >>= readNumber
  pure $ Incoming
    { attachments: attachments
    , body: body
    , conversationId: conversationId
    , decrypted_at: decrypted_at
    , errors: Nothing
    , flags: flags
    , id: id
    , received_at: received_at
    , sent_at: sent_at
    , source: source
    , sourceDevice: sourceDevice
    , timestamp: timestamp
    }

readKeyChange :: Foreign -> F Message
readKeyChange value = do
  conversationId <- value ! "conversationId" >>= readString
  id             <- value ! "id" >>= readString
  key_changed    <- value ! "key_changed" >>= readString
  received_at    <- value ! "received_at" >>= readNumber
  sent_at        <- value ! "sent_at" >>= readNullOrUndefined >>= traverse readNumber
  timestamp      <- value ! "timestamp" >>= readNumber
  unread         <- value ! "unread" >>= readInt
  pure $ KeyChange
    { conversationId: conversationId
    , id: id
    , key_changed: key_changed
    , received_at: received_at
    , sent_at: sent_at
    , timestamp: timestamp
    , unread: unread
    }

readMessage :: Foreign -> F Message
readMessage value = do
  type_ <- value ! "type" >>= readString
  case type_ of
    "incoming"  -> readIncoming value
    "keychange" -> readKeyChange value
    _           -> fail $ ForeignError ("Unknown type: " <> type_)
