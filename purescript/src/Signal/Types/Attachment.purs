module Signal.Types.Attachment
  ( Attachment
  , readAttachment
  , replaceUnicodeOrderOverrides
  , replaceUnicodeOrderOverridesSync
  )
  where

import Prelude

import Control.Monad.Promise (PurePromise, resolve)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Foreign (F, Foreign, readInt, readNullOrUndefined, readString, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe)
import Data.Record.ShowRecord (showRecord)
import Data.String (Pattern(..), replaceAll, Replacement(..))
import Data.Traversable (traverse)

newtype AttachmentData = AttachmentData ArrayBuffer

newtype Attachment = Attachment
  { contentType :: String
  , data :: AttachmentData
  , fileName :: String
  , schemaVersion :: Maybe Int
  , size :: Int
  }

instance showAttachment :: Show Attachment where
  show (Attachment o) = showRecord o

instance showAttachmentData :: Show AttachmentData where
  show (AttachmentData x) = "(AttachmentData { length: " <> show (byteLength x) <> " })"

-- Parsing
readAttachment :: Foreign -> F Attachment
readAttachment value = do
  contentType   <- value ! "contentType" >>= readString
  data_         <- value ! "data" >>= unsafeReadTagged "ArrayBuffer"
  fileName      <- value ! "fileName" >>= readString
  size          <- value ! "size" >>= readInt
  schemaVersion <- value ! "schemaVersion" >>= readNullOrUndefined >>= traverse readInt
  pure $ Attachment
    { contentType: contentType
    , data: AttachmentData data_
    , fileName: fileName
    , size: size
    , schemaVersion: schemaVersion
    }

-- Schema
leftToRightOverride :: Pattern
leftToRightOverride = Pattern "\x202D"

rightToLeftOverride :: Pattern
rightToLeftOverride = Pattern "\x202E"

overrideReplacement :: Replacement
overrideReplacement = Replacement "\xFFFD"

replaceUnicodeOrderOverridesSync :: Attachment -> Attachment
replaceUnicodeOrderOverridesSync (Attachment attachment) =
    Attachment $ attachment { fileName = replaceOverrides attachment.fileName }
  where
    replaceOverrides = replaceLTRO >>> replaceRTLO
    replaceLTRO = replaceAll leftToRightOverride overrideReplacement
    replaceRTLO = replaceAll rightToLeftOverride overrideReplacement

replaceUnicodeOrderOverrides :: Attachment -> PurePromise Attachment
replaceUnicodeOrderOverrides attachment =
  resolve (replaceUnicodeOrderOverridesSync attachment)
