module I18n.PhoneNumbers.PhoneNumber
  ( parse
  , format
  , formatString
  , regionCodeForNumber
  , PhoneNumber
  , module I18n.PhoneNumbers.PhoneNumberFormat
  ) where

-- import Prelude

-- import Data.Foreign (Foreign)
import I18n.PhoneNumbers.PhoneNumberFormat (PhoneNumberFormat(..))
import I18n.PhoneNumbers.PhoneNumberFormat as PNF


foreign import data PhoneNumber :: Type

foreign import formatImpl :: PhoneNumber -> Int -> String
foreign import formatStringImpl :: String -> Int -> String
foreign import parseImpl :: String -> PhoneNumber
foreign import regionCodeForNumberImpl :: PhoneNumber -> String

parse :: String -> PhoneNumber
parse = parseImpl

regionCodeForNumber :: PhoneNumber -> String
regionCodeForNumber = regionCodeForNumberImpl

format :: PhoneNumber -> PhoneNumberFormat -> String
format pn nf = formatImpl pn (PNF.toForeign nf)

formatString :: String -> PhoneNumberFormat -> String
formatString s nf = formatStringImpl s (PNF.toForeign nf)
