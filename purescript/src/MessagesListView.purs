module MessagesListView where

import Prelude

import Control.Monad.Eff   (Eff)
import DOM.HTML.Types      (HTMLElement)
import Halogen.Aff         as HA
import Halogen.VDom.Driver (runUI)

import Signal.Components.MessagesList as ML

render :: HTMLElement -> Eff (HA.HalogenEffects ()) Unit
render container = HA.runHalogenAff do
  runUI ML.messagesList unit container
