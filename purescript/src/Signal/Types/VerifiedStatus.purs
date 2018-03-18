module Signal.Types.VerifiedStatus
    ( VerifiedStatus(..)
    , fromInt
    , fromIntWithDefault
    , toInt
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)

data VerifiedStatus
  = Default
  | Verified
  | Unverified


derive instance eqVerifiedStatus :: Eq VerifiedStatus

instance showVerifiedStatus :: Show VerifiedStatus where
    show Default    = "Default"
    show Verified   = "Verified"
    show Unverified = "Unverified"

fromInt :: Int -> Maybe VerifiedStatus
fromInt 0 = Just Default
fromInt 1 = Just Verified
fromInt 2 = Just Unverified
fromInt _ = Nothing

toInt :: VerifiedStatus -> Int
toInt Default    = 0
toInt Verified   = 1
toInt Unverified = 2

fromIntWithDefault :: Maybe Int -> VerifiedStatus
fromIntWithDefault (Just n) = fromMaybe Default (fromInt n)
fromIntWithDefault Nothing  = Default
