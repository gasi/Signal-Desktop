module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..), maybe)
import Database.IndexedDB.Core            as IDB
import Database.IndexedDB.IDBDatabase     as IDBDatabase
import Database.IndexedDB.IDBFactory      as IDBFactory
import Database.IndexedDB.IDBKeyRange     as IDBKeyRange
import Database.IndexedDB.IDBObjectStore  as IDBObjectStore
import Database.IndexedDB.IDBTransaction  as IDBTransaction

import Signal.Types.RawMessage (readRawMessage)

launchAff' :: forall a e. Aff e a -> Eff (exception :: EXCEPTION | e) Unit
launchAff' aff =
  pure unit <* (launchAff aff)

databaseVersion :: Maybe Int
databaseVersion = Just 16

sampleMessageId :: String
-- sampleMessageId = "069f009b-0c13-f4d7-0fbd-5d81db37d893" -- incoming
sampleMessageId = "f9c4ef48-0fc9-7c21-3855-ab836fe15ef5" -- outgoing

main :: Eff (idb :: IDB.IDB, exception :: EXCEPTION, console :: CONSOLE) Unit
main = launchAff' do
  let callbacks = { onBlocked: Nothing
                  , onUpgradeNeeded: Nothing
                  }
  db <- IDBFactory.open "signal" databaseVersion callbacks

  tx    <- IDBDatabase.transaction db ["messages"] IDB.ReadOnly
  store <- IDBTransaction.objectStore tx "messages"
  (maybeRawMessage :: Maybe Foreign) <- IDBObjectStore.get store (IDBKeyRange.only sampleMessageId)
  log $ "[PureScript] message: " <> maybe "(message not found)"
    (show <<< runExcept <<< readRawMessage) maybeRawMessage
