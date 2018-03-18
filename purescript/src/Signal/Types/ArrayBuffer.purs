module Signal.Types.ArrayBuffer
    ( ArrayBuffer
    , readArrayBuffer
    ) where

import Prelude

import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Types       as A
import Data.Foreign                 (F, Foreign, unsafeReadTagged)


newtype ArrayBuffer = ArrayBuffer A.ArrayBuffer

foreign import eqImpl :: ArrayBuffer -> ArrayBuffer -> Boolean

instance eqArrayBuffer :: Eq ArrayBuffer
  where eq = eqImpl

instance showArrayBuffer :: Show ArrayBuffer where
  show (ArrayBuffer o) =
    "(ArrayBuffer { length: " <> show (byteLength o) <> " })"

readArrayBuffer :: Foreign -> F ArrayBuffer
readArrayBuffer = unsafeReadTagged "ArrayBuffer"
