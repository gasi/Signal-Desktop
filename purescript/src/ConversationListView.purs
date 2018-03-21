module ConversationListView where

import Prelude

import Control.Monad.Eff                  (Eff)
import Control.Monad.Eff.Console          (CONSOLE)
import Data.Array                         (filter, reverse, sort)
import Data.Foreign                       (Foreign)
import Data.Maybe                         (Maybe(..))
import Database.IndexedDB.Core            (IDB)
import DOM.HTML.Types                     (HTMLElement)
import Halogen.Aff as                     HA
import Halogen.VDom.Driver                (runUI)

import Signal.Components.ConversationList (ConversationListItem(..), conversationList)
import Signal.Database                    as DB
import Signal.Types.Conversation          (isActive)
import Signal.Types.Foreign.Conversation  as FC


type Effects eff = Eff (HA.HalogenEffects (idb :: IDB, console :: CONSOLE | eff)) Unit

render ::
    forall eff
    .  { container :: HTMLElement, onItemClick :: Foreign -> Effects eff }
    -> Effects eff
render opts = HA.runHalogenAff do
  db            <- DB.open
  conversations <- DB.getAllConversations db
  regionCode    <- DB.getRegionCode db
  let activeConversations = filter isActive conversations
      initialState =
        { items : reverse $ sort $ map
            (\c -> ConversationListItem { regionCode, conversation: c }) activeConversations
        , selectedItem : Nothing
        , regionCode
        }
      onConversationSelect = opts.onItemClick <<< FC.toForeign
  runUI (conversationList initialState onConversationSelect) unit opts.container
