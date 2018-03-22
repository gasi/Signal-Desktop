module ConversationListView where

import Prelude

import Control.Monad.Eff                  (Eff)
import Control.Monad.Eff.Console          (CONSOLE)
import Data.Array                         (filter, reverse, sort)
import Data.Foreign                       (Foreign)
import Data.Maybe                         (Maybe(..), maybe)
import Database.IndexedDB.Core            (IDB)
import DOM                                (DOM)
import DOM.HTML                           (window)
import DOM.HTML.URL                       (createObjectURL)
import DOM.HTML.Types                     (HTMLElement, URL)
import DOM.HTML.Window as                 W
import Halogen as                         H
import Halogen.Aff as                     HA
import Halogen.VDom.Driver                (runUI)
import Data.Traversable                   (traverse)
import Data.String                        as S


import Signal.Components.ConversationList as CL
import Signal.Components.ConversationList (ConversationListItem(..), conversationList)
import Signal.Database                    as DB
import Signal.Types.ArrayBuffer           (ArrayBuffer, toFile)
import Signal.Types.Conversation          (Conversation(..), getAvatar, getColor, isActive)
import Signal.Types.Avatar                (Avatar(..))
import Signal.Types.Foreign.Conversation  as FC


type Effects eff a = Eff (HA.HalogenEffects (idb :: IDB, console :: CONSOLE, dom :: DOM | eff)) a

render ::
    forall eff
    .  { container :: HTMLElement, onItemClick :: Foreign -> Effects eff Unit }
    -> Effects eff Unit
render opts = HA.runHalogenAff do
  db            <- DB.open
  conversations <- DB.getAllConversations db
  regionCode    <- DB.getRegionCode db
  url           <- H.liftEff $ W.url =<< window
  let activeConversations = filter isActive conversations
  items        <- traverse (\conversation -> do
    avatar <- H.liftEff $ createAvatar url conversation
    pure $ ConversationListItem
          { regionCode
          , conversation
          , avatar
          }
    ) activeConversations
  let initialState =
        { items : reverse $ sort $ items
        , selectedItem : Nothing
        , regionCode
        }
      onConversationSelect = opts.onItemClick <<< FC.toForeign
  runUI (conversationList initialState onConversationSelect) unit opts.container
  where

  createAvatar :: URL -> Conversation -> Effects eff CL.Avatar
  createAvatar url c = case getAvatar c of
    Just (Avatar o) -> do
      blobURL <- arrayBufferToURL url o.contentType o.data
      pure $ CL.Image blobURL
    _               -> case c of
      p@(Private o) -> pure $ CL.Color (getColor p) (maybe "#" (S.take 1) o.name)
      (Group o)     -> pure CL.GroupIcon

  arrayBufferToURL :: URL -> String -> ArrayBuffer -> Effects eff String
  arrayBufferToURL url mt ab = createObjectURL (toFile ab mt) url
