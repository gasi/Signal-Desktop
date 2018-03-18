module Signal.Types.Long
  ( Long(Long)
  , readLong
  ) where

import Prelude

import Data.Foreign (F, Foreign, readBoolean, readNumber)
import Data.Foreign.Index ((!))


data Long = Long
  { unsigned :: Boolean
  , high     :: Number
  , low      :: Number
  }

derive instance eqLong :: Eq Long

instance showLong :: Show Long where
  show (Long o) =
    "(Long" <>
    " unsigned: " <> show o.unsigned <> "," <>
    " high: " <> show o.high <> "," <>
    " low: " <> show o.low <>
    ")"

readLong :: Foreign -> F Long
readLong value = do
  unsigned <- value ! "unsigned" >>= readBoolean
  high     <- value ! "high"     >>= readNumber
  low      <- value ! "low"      >>= readNumber
  pure $ Long
    { unsigned
    , high
    , low
    }
