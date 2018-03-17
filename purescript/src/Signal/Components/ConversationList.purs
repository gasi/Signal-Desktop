module Signal.Components.ConversationList where

import Prelude

import CSS.Background (backgroundImage, url)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.CSS as HCS
import Halogen.HTML.Properties as HP

import Signal.Types.Conversation (Conversation)


type State = Array Conversation

data Query a
  = Toggle a
  | IsOn (State -> a)

data Message = Toggled State

conversationList :: forall m. State -> H.Component HH.HTML Query Unit Message m
conversationList initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.class_ $ HC.ClassName "conversation-list-item contact c112 selected"
      ]
      [ HH.span
          [ HP.class_ $ HC.ClassName "avatar"
          , HCS.style do
              backgroundImage $ url "blob:file:///<GUID>"
          ]
          [ HH.text "" ]
      , HH.div
          [ HP.class_ $ HC.ClassName "contact-details" ]
          [ HH.span
              [ HP.class_ $ HC.ClassName "last-timestamp"
              , HP.title "Sat, Jan 1, 2018 0:00 PM"
              , HH.attr (HC.AttrName "dir") "auto"
              , HH.attr (HC.AttrName "data-timestamp") "1000000000"
              ] [ HH.text "1 hour" ]
          , HH.h3
              [ HP.class_ $ HC.ClassName "name"
              , HH.attr (HC.AttrName "dir") "auto"
              ]
              [ HH.text "John Lennon"
              -- , HH.text " " -- HACK
              -- , HH.img
              --     [ HP.src "node_modules/emoji-datasource-apple/img/apple/64/1f52a.png"
              --     , HP.class_ $ HC.ClassName "emoji"
              --     , HH.attr (HC.AttrName "data-codepoints") "1f52a"
              --     , HP.title ":hocho:"
              --     ]
              ]
          , HH.div
              [ HP.class_ $ HC.ClassName "number" ]
              []
          , HH.p
              [ HP.class_ $ HC.ClassName "last-message"
              , HH.attr (HC.AttrName "dir") "auto"
              ]
              [ HH.text "Imagine…"
              ]
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      -- state <- H.get
      let nextState = [] --not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)
