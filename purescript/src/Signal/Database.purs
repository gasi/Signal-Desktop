module Signal.Database
 ( getMessageById
 , getConversationById
 , getAllConversationIds
 , getAllConversations
 , getRegionCode
 , open
 ) where

import Prelude

import Control.Monad.Aff                 (Aff)
import Control.Monad.Except              (runExcept)
import Data.Array                        (catMaybes)
import Data.Either                       (Either(..), hush)
import Data.Foreign                      (Foreign, ForeignError(..))
import Data.List.NonEmpty                as NonEmpty
import Data.List.Types                   (NonEmptyList)
import Data.Maybe                        (Maybe(..), maybe)
import Data.Traversable                  (traverse)
import Database.IndexedDB.Core           as IDB
import Database.IndexedDB.IDBDatabase    as IDBDatabase
import Database.IndexedDB.IDBFactory     as IDBFactory
import Database.IndexedDB.IDBKey         as IDBKey
import Database.IndexedDB.IDBKeyRange    as IDBKeyRange
import Database.IndexedDB.IDBObjectStore as IDBObjectStore
import Database.IndexedDB.IDBTransaction as IDBTransaction

import Signal.Types.Message              (Message, readMessage)
import Signal.Types.Conversation         (Conversation, readConversation)


type ConversationId = String
type MessageId = String
type RegionCode = String

-- meta
databaseVersion :: Maybe Int
databaseVersion = Just 17

databaseName :: String
databaseName = "signal"

callbacks :: forall e. IDBFactory.Callbacks e
callbacks = { onBlocked: Nothing
            , onUpgradeNeeded: Nothing
            }

-- stores
messagesStoreName :: String
messagesStoreName = "messages"

conversationsStoreName :: String
conversationsStoreName = "conversations"

-- TODO: Rename to `settings`?
itemsStoreName :: String
itemsStoreName = "items"

open
  :: forall e
  . Aff (idb :: IDB.IDB | e) IDB.Database
open = IDBFactory.open databaseName databaseVersion callbacks

getMessageById
  :: forall e
  .  IDB.Database
  -> MessageId
  -> Aff (idb :: IDB.IDB | e) (Either (NonEmptyList ForeignError) Message)
getMessageById db mId = do
    tx      <- IDBDatabase.transaction db [messagesStoreName] IDB.ReadOnly
    store   <- IDBTransaction.objectStore tx messagesStoreName
    message <- IDBObjectStore.get store (IDBKeyRange.only mId)
    pure $ maybe (error "Message not found") toEither message
  where
    toEither :: Foreign -> Either (NonEmptyList ForeignError) Message
    toEither = runExcept <<< readMessage

getConversationById
  :: forall e
  .  IDB.Database
  -> ConversationId
  -> Aff (idb :: IDB.IDB | e) (Either (NonEmptyList ForeignError) Conversation)
getConversationById db cId = do
    tx           <- IDBDatabase.transaction db [conversationsStoreName] IDB.ReadOnly
    store        <- IDBTransaction.objectStore tx conversationsStoreName
    conversation <- IDBObjectStore.get store (IDBKeyRange.only cId)
    pure $ maybe (error "Conversation not found") toEither conversation
  where
    toEither :: Foreign -> Either (NonEmptyList ForeignError) Conversation
    toEither = runExcept <<< readConversation

getAllConversationIds
  :: forall e
  .  IDB.Database
  -> Aff (idb :: IDB.IDB | e) (Array ConversationId)
getAllConversationIds db = do
    tx    <- IDBDatabase.transaction db [conversationsStoreName] IDB.ReadOnly
    store <- IDBTransaction.objectStore tx conversationsStoreName
    cIds  <- IDBObjectStore.getAllKeys store Nothing (Just 100)
    pure $ map IDBKey.unsafeFromKey cIds

getAllConversations
  :: forall e
  .  IDB.Database
  -> Aff (idb :: IDB.IDB | e) (Array Conversation)
getAllConversations db = do
  cIds <- getAllConversationIds db
  cs   <- traverse (getConversationById db) cIds
  pure $ catMaybes $ map hush cs

getSetting
  :: forall e
  .  IDB.Database
  -> String
  -> Aff (idb :: IDB.IDB | e) (Maybe RegionCode)
getSetting db name = do
    tx    <- IDBDatabase.transaction db [itemsStoreName] IDB.ReadOnly
    store <- IDBTransaction.objectStore tx itemsStoreName
    value <- IDBObjectStore.get store (IDBKeyRange.only name)
    pure value

getRegionCode
  :: forall e
  .  IDB.Database
  ->  Aff (idb :: IDB.IDB | e) (Maybe RegionCode)
getRegionCode db = getSetting db "regionCode"

error :: forall a. String -> Either (NonEmptyList ForeignError) a
error message = Left $ NonEmpty.singleton $ ForeignError message
