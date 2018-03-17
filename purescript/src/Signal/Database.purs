module Signal.Database
 ( getMessageById
 , getConversationById
 , getAllConversationIds
 , getAllConversations
 , open
 ) where

import Prelude

import Control.Monad.Aff                 (Aff)
import Control.Monad.Except              (runExcept)
import Data.Array                        (catMaybes)
import Data.Either                       (Either(..), hush)
import Data.Foreign                      (Foreign, ForeignError(..))
import Data.List.Types                   (NonEmptyList)
import Data.List.NonEmpty                as NonEmpty
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


type MessageId = String
type ConversationId = String

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
messagesStoreName :: String
messagesStoreName = "messages"

conversationsStoreName :: String
conversationsStoreName = "conversations"

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
    tx      <- IDBDatabase.transaction db [conversationsStoreName] IDB.ReadOnly
    store   <- IDBTransaction.objectStore tx conversationsStoreName
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

error :: forall a. String -> Either (NonEmptyList ForeignError) a
error message = Left $ NonEmpty.singleton $ ForeignError message
