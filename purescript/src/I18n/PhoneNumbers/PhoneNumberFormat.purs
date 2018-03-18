module I18n.PhoneNumbers.PhoneNumberFormat
  ( PhoneNumberFormat(..)
  , toForeign
  ) where


data PhoneNumberFormat
  = E164
  | International
  | National
  | RFC3966

toForeign :: PhoneNumberFormat -> Int
toForeign E164          = 0
toForeign International = 1
toForeign National      = 2
toForeign RFC3966       = 3
