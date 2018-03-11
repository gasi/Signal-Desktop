module Main
    ( getMessages
    , getConversations
    ) where

import Prelude

import Control.Monad.Eff           (Eff)
import Control.Promise             (Promise)
import Control.Promise             as Promise
import Data.Either                 (Either(..))
import Data.Foreign                (ForeignError)
import Data.List.Types             (NonEmptyList)
import Database.IndexedDB.Core     (IDB)

import Signal.Types.Message        (Message)
import Signal.Types.Conversation   (Conversation)
import Signal.Database             as DB

-- messages
incomingMessageId :: String
incomingMessageId = "069f009b-0c13-f4d7-0fbd-5d81db37d893"

outgoingMessageId :: String
outgoingMessageId = "f9c4ef48-0fc9-7c21-3855-ab836fe15ef5"

keyChangeMessageId :: String
keyChangeMessageId = "b217ace9-2718-c5fd-5e25-898b6cbb37ec"

-- Public API
getMessages :: Eff (idb :: IDB) (Promise (Array Message))
getMessages = Promise.fromAff $ do
    db <- DB.open

    incomingMessage <- DB.getMessageById db incomingMessageId
    outgoingMessage <- DB.getMessageById db outgoingMessageId
    keyChangeMessage <- DB.getMessageById db keyChangeMessageId

    case ([incomingMessage, keyChangeMessage]) of
        ([Right im, Right kcm]) -> pure [im, kcm]
        _                       -> pure []

getConversations :: Eff (idb :: IDB)
                    (Promise (Array (Either (NonEmptyList ForeignError) Conversation)))
getConversations = Promise.fromAff $ do
    db <- DB.open

    c1 <- DB.getConversationById db "+15803240718"
    c2 <- DB.getConversationById db "+12068836381"
    c3 <- DB.getConversationById db "+12069294704"

    pure [c1, c2, c3]
