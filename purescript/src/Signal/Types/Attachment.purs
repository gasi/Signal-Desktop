module Signal.Types.Attachment where

import Prelude

import Control.Monad.Promise (PurePromise, resolve)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.String (Pattern(..), replaceAll, Replacement(..))

type Attachment =
  { contentType :: String
  , data :: ArrayBuffer
  , fileName :: String
  , size :: Number
  , schemaVersion :: Number
  }

leftToRightOverride :: Pattern
leftToRightOverride = Pattern "\x202D"

rightToLeftOverride :: Pattern
rightToLeftOverride = Pattern "\x202E"

overrideReplacement :: Replacement
overrideReplacement = Replacement "\xFFFD"

replaceUnicodeOrderOverridesSync :: Attachment -> Attachment
replaceUnicodeOrderOverridesSync attachment =
    attachment { fileName = replaceOverrides attachment.fileName }
  where
    replaceOverrides = replaceLTRO >>> replaceRTLO
    replaceLTRO = replaceAll leftToRightOverride overrideReplacement
    replaceRTLO = replaceAll rightToLeftOverride overrideReplacement

replaceUnicodeOrderOverrides :: Attachment -> PurePromise Attachment
replaceUnicodeOrderOverrides attachment =
  resolve (replaceUnicodeOrderOverridesSync attachment)
