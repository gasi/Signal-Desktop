module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Maybe (Maybe(..), maybe)
import Database.IndexedDB.Core            as IDB
import Database.IndexedDB.IDBDatabase     as IDBDatabase
import Database.IndexedDB.IDBFactory      as IDBFactory
import Database.IndexedDB.IDBKeyRange     as IDBKeyRange
import Database.IndexedDB.IDBObjectStore  as IDBObjectStore
import Database.IndexedDB.IDBTransaction  as IDBTransaction

import Signal.Types.Message (readMessage)

-- helpers
launchAff' :: forall a e. Aff e a -> Eff (exception :: EXCEPTION | e) Unit
launchAff' aff =
  pure unit <* (launchAff aff)

-- database
databaseVersion :: Maybe Int
databaseVersion = Just 16

databaseName :: String
databaseName = "signal"

-- messages
incomingMessageId :: String
incomingMessageId = "069f009b-0c13-f4d7-0fbd-5d81db37d893"

outgoingMessageId :: String
outgoingMessageId = "f9c4ef48-0fc9-7c21-3855-ab836fe15ef5"

keyChangeMessageId :: String
keyChangeMessageId = "b217ace9-2718-c5fd-5e25-898b6cbb37ec"

-- main
main :: Eff (idb :: IDB.IDB, exception :: EXCEPTION, console :: CONSOLE) Unit
main = launchAff' do
    db <- IDBFactory.open databaseName databaseVersion callbacks

    tx    <- IDBDatabase.transaction db ["messages"] IDB.ReadOnly
    store <- IDBTransaction.objectStore tx "messages"

    incomingMessage <- getMessageById store incomingMessageId
    log $ "[PureScript] incoming message: " <> showMessage incomingMessage

    keyChangeMessage <- getMessageById store keyChangeMessageId
    log $ "[PureScript] key change message: " <> showMessage keyChangeMessage
  where
    callbacks = { onBlocked: Nothing
                , onUpgradeNeeded: Nothing
                }
    getMessageById store id_ = IDBObjectStore.get store (IDBKeyRange.only id_)
    showMessage = maybe "[message not found]" (show <<< runExcept <<< readMessage)
