module Main
    ( getMessages
    , getAllConversations
    ) where

import Prelude

import Control.Monad.Eff                  (Eff)
import Control.Promise                    (Promise)
import Control.Promise                    as Promise
import Data.Either                        (Either, hush)
import Data.Foreign                       (Foreign, ForeignError)
import Data.List.Types                    (NonEmptyList)
import Data.Nullable                      (Nullable, toNullable)
import Data.Traversable                   (traverse)
import Database.IndexedDB.Core            (IDB)
import Database.IndexedDB.IDBKey.Internal (unsafeFromKey)

import Signal.Database                    as DB
import Signal.Types.Foreign.Conversation  as FC
import Signal.Types.Message               (Message)


type Results a = Array (Either (NonEmptyList ForeignError) a)

-- messages
incomingMessageId :: String
incomingMessageId = "069f009b-0c13-f4d7-0fbd-5d81db37d893"

outgoingMessageId :: String
outgoingMessageId = "f9c4ef48-0fc9-7c21-3855-ab836fe15ef5"

keyChangeMessageId :: String
keyChangeMessageId = "b217ace9-2718-c5fd-5e25-898b6cbb37ec"

-- Public API
getMessages :: Eff (idb :: IDB) (Promise (Results Message))
getMessages = Promise.fromAff $ do
    db <- DB.open

    incomingMessage  <- DB.getMessageById db incomingMessageId
    outgoingMessage  <- DB.getMessageById db outgoingMessageId
    keyChangeMessage <- DB.getMessageById db keyChangeMessageId

    pure [incomingMessage, outgoingMessage, keyChangeMessage]

getAllConversations :: Eff (idb :: IDB) (Promise (Array (Nullable Foreign)))
getAllConversations = Promise.fromAff $ do
    db <- DB.open
    cIds <- DB.getAllConversations db
    cs <- traverse (\k -> DB.getConversationById db (unsafeFromKey k)) cIds
    let csWithoutErrors = map hush cs
    pure $ map toNullable $ (map $ map FC.toForeign) csWithoutErrors
