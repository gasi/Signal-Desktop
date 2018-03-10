module Signal.Database
 ( getMessageById
 , open
 ) where

import Prelude

import Control.Monad.Aff                 (Aff)
import Control.Monad.Except              (runExcept)
import Data.Either                       (Either(..))
import Data.Foreign                      (Foreign, ForeignError(..))
import Data.List.Types                   (NonEmptyList)
import Data.List.NonEmpty                as NonEmpty
import Data.Maybe                        (Maybe(..), maybe)
import Database.IndexedDB.Core           as IDB
import Database.IndexedDB.IDBDatabase    as IDBDatabase
import Database.IndexedDB.IDBFactory     as IDBFactory
import Database.IndexedDB.IDBKeyRange    as IDBKeyRange
import Database.IndexedDB.IDBObjectStore as IDBObjectStore
import Database.IndexedDB.IDBTransaction as IDBTransaction

import Signal.Types.Message (Message, readMessage)


type MessageId = String

-- meta
databaseVersion :: Maybe Int
databaseVersion = Just 16

databaseName :: String
databaseName = "signal"

callbacks :: forall e. IDBFactory.Callbacks e
callbacks = { onBlocked: Nothing
            , onUpgradeNeeded: Nothing
            }

-- stores
messageStoreName :: String
messageStoreName = "messages"

open
  :: forall e
  . Aff (idb :: IDB.IDB | e) IDB.Database
open = IDBFactory.open databaseName databaseVersion callbacks

getMessageById
  :: forall e
  .  IDB.Database
  -> MessageId
  -> Aff (idb :: IDB.IDB | e) (Either (NonEmptyList ForeignError) Message)
getMessageById db messageId = do
    tx      <- IDBDatabase.transaction db [messageStoreName] IDB.ReadOnly
    store   <- IDBTransaction.objectStore tx messageStoreName
    message <- IDBObjectStore.get store (IDBKeyRange.only messageId)
    pure $ maybe (Left $ NonEmpty.singleton $ ForeignError "Not found") toEither message
  where
    toEither :: Foreign -> Either (NonEmptyList ForeignError) Message
    toEither = runExcept <<< readMessage
