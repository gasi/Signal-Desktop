module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..), maybe)
import Data.Record.ShowRecord (showRecord)

import Database.IndexedDB.Core            as IDB
import Database.IndexedDB.IDBDatabase     as IDBDatabase
import Database.IndexedDB.IDBFactory      as IDBFactory
import Database.IndexedDB.IDBKeyRange     as IDBKeyRange
import Database.IndexedDB.IDBObjectStore  as IDBObjectStore
import Database.IndexedDB.IDBTransaction  as IDBTransaction

launchAff' :: forall a e. Aff e a -> Eff (exception :: EXCEPTION | e) Unit
launchAff' aff =
  pure unit <* (launchAff aff)


type Attachment =
  { contentType :: String
  -- , data :: ArrayBuffer
  -- , digest :: ArrayBuffer
  , fileName :: String
  -- , flags :: Maybe Int
  -- , height :: Maybe Int
  -- , id :: String
  -- , key :: ArrayBuffer
  , schemaVersion :: Maybe Int
  , size :: Int
  -- , thumbnail :: Maybe {}
  -- , width :: Maybe Int
  }
type Message =
  -- { attachments :: Array Attachment
  { body :: String
  -- , conversationId :: String
  -- , decrypted_at :: Int
  -- , flags :: Int
  -- , id :: String
  -- , received_at :: Int
  -- , recipients :: Array String
  -- , sent :: Boolean
  -- , sent_at :: Int
  , source :: String
  , sourceDevice :: Int
  , timestamp :: Int
  , type :: String -- 'outgoing'
  }

main :: Eff (idb :: IDB.IDB, exception :: EXCEPTION, console :: CONSOLE) Unit
main = launchAff' do
  let callbacks = { onBlocked : Nothing
                  , onUpgradeNeeded : Nothing
                  }
      version = Just 16
  db <- IDBFactory.open "signal" version callbacks

  tx    <- IDBDatabase.transaction db ["messages"] IDB.ReadOnly
  store <- IDBTransaction.objectStore tx "messages"
  (maybeMessage :: Maybe Message) <-
    IDBObjectStore.get store (IDBKeyRange.only "1052563c-a81d-1477-e69e-353a05850400")
  log $ "[PureScript] message: " <> maybe "(message not found)" showRecord maybeMessage
