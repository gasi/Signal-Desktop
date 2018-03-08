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


newtype Message = Message
  -- { attachments :: Array Attachment
  -- , body :: String
  { body :: String
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
