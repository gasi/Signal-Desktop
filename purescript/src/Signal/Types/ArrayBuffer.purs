module Signal.Types.ArrayBuffer
    ( ArrayBuffer
    , readArrayBuffer
    , toBlob
    , toFile
    ) where

import Prelude

import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Types       as A
import Data.Foreign                 (F, Foreign, unsafeReadTagged)
import DOM.File.Types (Blob, File)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)


newtype ArrayBuffer = ArrayBuffer A.ArrayBuffer

foreign import eqImpl :: ArrayBuffer -> ArrayBuffer -> Boolean

instance eqArrayBuffer :: Eq ArrayBuffer
  where eq = eqImpl

instance showArrayBuffer :: Show ArrayBuffer where
  show (ArrayBuffer o) =
    "(ArrayBuffer { length: " <> show (byteLength o) <> " })"

readArrayBuffer :: Foreign -> F ArrayBuffer
readArrayBuffer = unsafeReadTagged "ArrayBuffer"

type MediaType = String
foreign import toBlobImpl :: Fn2 ArrayBuffer MediaType Blob
foreign import blobToFileImpl :: Fn1 Blob File

toBlob :: ArrayBuffer -> MediaType -> Blob
toBlob = runFn2 toBlobImpl

toFile :: ArrayBuffer -> MediaType -> File
toFile ab mt = runFn1 blobToFileImpl (toBlob ab mt)
