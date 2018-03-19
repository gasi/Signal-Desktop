module Signal.Types.Timestamp
  ( Timestamp(..)
  , readTimestamp
  ) where

import Prelude

import Data.Foreign (F, Foreign, ForeignError(..), fail, readNumber, typeOf)

import Signal.Types.Long (Long, readLong)


data Timestamp = TSLong Long | TSNumber Number

derive instance eqTimestamp :: Eq Timestamp
derive instance ordTimestamp :: Ord Timestamp

instance showTimestamp :: Show Timestamp where
  show (TSLong ts)   = "(TSLong " <> show ts <> ")"
  show (TSNumber ts) = "(TSNumber " <> show ts <> ")"

readTimestamp :: Foreign -> F Timestamp
readTimestamp value
  | typeOf value == "object" = map TSLong $ readLong value
  | typeOf value == "number" = map TSNumber $ readNumber value
  | otherwise                = fail $ ForeignError "Invalid timestamp"
