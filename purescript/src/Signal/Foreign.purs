module Signal.Foreign
  ( optional
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Foreign (F, Foreign,readNullOrUndefined)
import Data.Traversable (traverse)


optional :: forall a. (Foreign -> F a) -> Foreign -> F (Maybe a)
optional f x = readNullOrUndefined x >>= traverse f
