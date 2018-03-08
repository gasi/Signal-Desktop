module Signal.Types.Message
  ( Message
  , readMessage
  ) where

import Prelude

import Data.Foreign (F, Foreign, readArray, readBoolean, readInt,
                    readNullOrUndefined, readNumber, readString)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe)
import Data.Record.ShowRecord (showRecord)
import Data.Traversable (traverse)

import Signal.Types.Attachment (Attachment, readAttachment)

newtype Message = Message
  { attachments :: Array Attachment
  , body :: String
  , conversationId :: String
  -- TODO: `decrypted_at` should be `Int` but we get type mismatch error:
  , decrypted_at :: Maybe Number
  , flags :: Maybe Int
  , id :: String
  -- TODO: `received_at` should be `Int` but we get type mismatch error:
  , received_at :: Number
  , recipients :: Maybe (Array String)
  , sent :: Maybe Boolean
  -- TODO: `sent_at` should be `Int` but we get type mismatch error:
  , sent_at :: Number
  , source :: Maybe String
  , sourceDevice :: Maybe Int
  -- TODO: `timestamp` should be `Int` but we get type mismatch error:
  , timestamp :: Number
  , type :: String -- 'outgoing' | 'incoming' | 'keychange'
  }

instance showMessage :: Show Message where
  show (Message o) = showRecord o

readMessage :: Foreign -> F Message
readMessage value = do
  attachments    <- value ! "attachments" >>= readArray >>= traverse readAttachment
  body           <- value ! "body" >>= readString
  conversationId <- value ! "conversationId" >>= readString
  decrypted_at   <- value ! "decrypted_at" >>= readNullOrUndefined >>= traverse readNumber
  flags          <- value ! "flags" >>= readNullOrUndefined >>= traverse readInt
  id             <- value ! "id" >>= readString
  received_at    <- value ! "received_at" >>= readNumber
  recipients     <- value ! "recipients" >>= readNullOrUndefined >>=
                              traverse readArray >>= (traverse >>> traverse) readString
  sent           <- value ! "sent" >>= readNullOrUndefined >>= traverse readBoolean
  sent_at        <- value ! "sent_at" >>= readNumber
  source         <- value ! "source" >>= readNullOrUndefined >>= traverse readString
  sourceDevice   <- value ! "sourceDevice" >>= readNullOrUndefined >>= traverse readInt
  timestamp      <- value ! "timestamp" >>= readNumber
  type_          <- value ! "type" >>= readString
  pure $ Message
    { attachments: attachments
    , body: body
    , conversationId: conversationId
    , decrypted_at: decrypted_at
    , flags: flags
    , id: id
    , received_at: received_at
    , recipients: recipients
    , sent: sent
    , sent_at: sent_at
    , source: source
    , sourceDevice: sourceDevice
    , timestamp: timestamp
    , type: type_
    }
