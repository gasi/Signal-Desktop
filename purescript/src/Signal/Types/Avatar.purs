module Signal.Types.Avatar
  ( Avatar(..)
  , readAvatar
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readInt, readString)
import Data.Foreign.Index ((!))
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))

import Signal.Foreign (optional)
import Signal.Types.ArrayBuffer (ArrayBuffer, readArrayBuffer)

newtype Avatar = Avatar
  { contentType :: String -- MIME
  , data        :: ArrayBuffer
  , size        :: Int
  }

colors :: Array String
colors =
  [ "red"
  , "pink"
  , "purple"
  , "deep_purple"
  , "indigo"
  , "blue"
  , "light_blue"
  , "cyan"
  , "teal"
  , "green"
  , "light_green"
  , "orange"
  , "deep_orange"
  , "amber"
  , "blue_grey"
  ]

instance showAvatar :: Show Avatar where
  show (Avatar o) =
    "(Avatar" <>
    " contentType: " <> show o.contentType <> "," <>
    " size: " <> show o.size <>
    ")"

readAvatar :: Foreign -> F Avatar
readAvatar value = do
    contentType <- value ! "contentType" >>= readString
    data_       <- value ! "data"        >>= readArrayBuffer
    size        <- parseSize value
    pure $ Avatar
      { contentType
      , data : data_
      , size
      }
  where
    parseSize :: Foreign -> F Int
    parseSize o = do
      size    <- value ! "size"   >>= optional readInt
      length_ <- value ! "length" >>= optional readInt
      case (size <|> length_) of
        Just n  -> pure n
        Nothing ->
          fail $ ForeignError "`Avatar::size` or `Avatar::length` is required"

derive instance eqAvatar :: Eq Avatar


