module Main where

import Prelude

import Control.Monad.Aff           (Aff, launchAff)
import Control.Monad.Aff.Console   (CONSOLE)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either                 (Either(..))
import Database.IndexedDB.Core     (IDB)

import Signal.Types.Message        (Message)
import Signal.Database             (getMessageById, open)


-- helpers
launchAff' :: forall a e. Aff e a -> Eff (exception :: EXCEPTION | e) Unit
launchAff' aff =
  pure unit <* (launchAff aff)

-- messages
incomingMessageId :: String
incomingMessageId = "069f009b-0c13-f4d7-0fbd-5d81db37d893"

outgoingMessageId :: String
outgoingMessageId = "f9c4ef48-0fc9-7c21-3855-ab836fe15ef5"

keyChangeMessageId :: String
keyChangeMessageId = "b217ace9-2718-c5fd-5e25-898b6cbb37ec"

-- main
main
    :: (Array Message -> Unit)
    -> Eff (idb :: IDB, exception :: EXCEPTION, console :: CONSOLE) Unit
main callback = launchAff' $ do
    db <- open

    incomingMessage <- getMessageById db incomingMessageId
    outgoingMessage <- getMessageById db outgoingMessageId
    keyChangeMessage <- getMessageById db keyChangeMessageId

    case ([incomingMessage, keyChangeMessage]) of
        ([Right im, Right kcm]) -> pure $ callback [im, kcm]
        _                       -> pure $ callback []
