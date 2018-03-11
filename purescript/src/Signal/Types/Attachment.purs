module Signal.Types.Attachment
  ( Attachment
  , readAttachment
  , replaceUnicodeOrderOverrides
  , replaceUnicodeOrderOverridesSync
  )
  where

import Prelude

import Control.Monad.Promise        (PurePromise, resolve)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.Foreign                 (F, Foreign, readInt, readNullOrUndefined,
                                     readString)
import Data.Foreign.Index           ((!))
import Data.Maybe                   (Maybe)
import Data.Record.ShowRecord       (showRecord)
import Data.String                  (Pattern(..), replaceAll, Replacement(..))
import Data.Traversable             (traverse)

import Signal.Types.ArrayBuffer     (ArrayBuffer, readArrayBuffer)

newtype Attachment = Attachment
  -- Required
  { fileName      :: String
  , contentType   :: String
  , size          :: Int
  , data          :: ArrayBuffer
  -- Optional
  , schemaVersion :: Maybe Int
  , id            :: Maybe String
  , width         :: Maybe Int
  , height        :: Maybe Int
  , thumbnail     :: Maybe ArrayBuffer
  , key           :: Maybe ArrayBuffer
  , digest        :: Maybe ArrayBuffer
  , flags         :: Maybe Int
  }

instance showAttachment :: Show Attachment where
  show (Attachment o) = showRecord o

-- Parsing
readAttachment :: Foreign -> F Attachment
readAttachment value = do
    -- Required
    fileName      <- value ! "fileName" >>= readString
    contentType   <- value ! "contentType" >>= readString
    size          <- value ! "size" >>= readInt
    data_         <- value ! "data" >>= readArrayBuffer
    -- Optional
    schemaVersion <- value ! "schemaVersion" >>= readNullOrUndefined >>= traverse readInt
    id_           <- value ! "id" >>= readNullOrUndefined >>= traverse readString
    width         <- value ! "width" >>= readNullOrUndefined >>= traverse readInt
    height        <- value ! "height" >>= readNullOrUndefined >>= traverse readInt
    thumbnail     <- value ! "thumbnail" >>= readNullOrUndefined >>=
                                traverse readArrayBuffer
    key           <- value ! "key" >>= readNullOrUndefined >>= traverse readArrayBuffer
    digest        <- value ! "digest" >>= readNullOrUndefined >>= traverse readArrayBuffer
    flags         <- value ! "flags" >>= readNullOrUndefined >>= traverse readInt
    pure $ Attachment
      -- Required
      { fileName: fileName
      , contentType: contentType
      , size: size
      , data: data_
      -- Optional
      , schemaVersion: schemaVersion
      , id: id_
      , width: width
      , height: height
      , thumbnail: thumbnail
      , key: key
      , digest: digest
      , flags: flags
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
