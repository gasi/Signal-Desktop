module I18n.PhoneNumbers.PhoneNumber
  ( format
  , formatString
  , parse
  , regionCodeForNumber
  , PhoneNumber
  , module I18n.PhoneNumbers.PhoneNumberFormat
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn3, runFn3)

import I18n.PhoneNumbers.PhoneNumberFormat (PhoneNumberFormat(..))
import I18n.PhoneNumbers.PhoneNumberFormat as PNF


type NumberFormat = Int
type RegionCode = String

foreign import data PhoneNumber :: Type

foreign import formatImpl :: NumberFormat -> PhoneNumber -> String
foreign import parseImpl  :: Fn3 (PhoneNumber -> Maybe PhoneNumber) (Maybe PhoneNumber) String (Maybe PhoneNumber)
foreign import regionCodeForNumberImpl :: Fn3 (String -> Maybe RegionCode) (Maybe RegionCode) PhoneNumber (Maybe RegionCode)

parse :: String -> Maybe PhoneNumber
parse = runFn3 parseImpl Just Nothing

regionCodeForNumber :: PhoneNumber -> Maybe RegionCode
regionCodeForNumber = runFn3 regionCodeForNumberImpl Just Nothing

format :: PhoneNumberFormat -> PhoneNumber -> String
format nf = formatImpl (PNF.toForeign nf)

formatString :: PhoneNumberFormat -> String -> Maybe String
formatString nf s = (format nf) <$> parse s
