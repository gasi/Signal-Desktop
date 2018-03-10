module MessagesListView where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (maybe)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import DOM.Node.ParentNode (QuerySelector(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Signal.Components.MessagesList as ML

render :: HTMLElement -> Eff (HA.HalogenEffects ()) Unit
render container = HA.runHalogenAff do
  runUI ML.messagesList unit container
