module Signal.Types.Foreign.Timestamp
  ( toForeign
  ) where

import Data.Foreign as F
import Data.Foreign (Foreign)

import Signal.Types.Long (Long(Long))
import Signal.Types.Timestamp as T


type TSNumber = Number
type TSLong =
  { unsigned :: Boolean
  , high     :: Number
  , low      :: Number
  }

toForeign :: T.Timestamp -> Foreign
toForeign (T.TSNumber o) = F.toForeign (o :: Number)
toForeign (T.TSLong (Long o)) = F.toForeign (o :: TSLong)
