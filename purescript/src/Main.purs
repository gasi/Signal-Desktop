module Main where

import Prelude

import Control.Monad.Aff           (Aff, launchAff)
import Control.Monad.Aff.Console   (CONSOLE, log)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Database.IndexedDB.Core     (IDB)

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
main :: Eff (idb :: IDB, exception :: EXCEPTION, console :: CONSOLE) Unit
main = launchAff' do
    db <- open

    incomingMessage <- getMessageById db incomingMessageId
    log $ "[PureScript] incoming message: " <> show incomingMessage

    outgoingMessage <- getMessageById db outgoingMessageId
    log $ "[PureScript] outgoing message: " <> show outgoingMessage

    keyChangeMessage <- getMessageById db keyChangeMessageId
    log $ "[PureScript] key change message: " <> show keyChangeMessage
