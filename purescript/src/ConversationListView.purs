module ConversationListView where

import Prelude

import Control.Monad.Eff                  (Eff)
import Data.Array                         (filter)
import Data.Maybe                         (Maybe(..))
import Database.IndexedDB.Core            as IDB
import DOM.HTML.Types                     (HTMLElement)
import Halogen.Aff as                     HA
import Halogen.VDom.Driver                (runUI)

import Signal.Components.ConversationList (conversationList)
import Signal.Database                    as DB
import Signal.Types.Conversation          (isActive)


render :: HTMLElement -> Eff (HA.HalogenEffects (idb :: IDB.IDB)) Unit
render container = HA.runHalogenAff do
  db            <- DB.open
  conversations <- DB.getAllConversations db
  let activeConversations = filter isActive conversations
      initialState =
        { items : activeConversations
        , selectedItem : Nothing
        }
  runUI (conversationList initialState) unit container
