module Signal.Components.ConversationList where

import Prelude

import Control.Alt ((<|>))
import CSS.Background (backgroundImage, url)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.CSS as HCS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import I18n.PhoneNumbers.PhoneNumber as PN
import Signal.Types.Conversation (Conversation(..))


type State =
  { items        :: Array Conversation
  , selectedItem :: Maybe Conversation
  }

data Query a
  = Select Conversation a


conversationList :: forall m. State -> H.Component HH.HTML Query Unit Void m
conversationList initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state = HH.div
    [ HP.class_ $ HC.ClassName "conversations inbox"
    ] (map (renderContactDetails state.selectedItem) state.items)

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Select c next -> do
      -- selectedItem <- H.gets _.selectedItem
      H.modify $ \s -> s { selectedItem = Just c }
      -- H.raise $ Selected (Just c)
      pure next

  renderContactDetails :: Maybe Conversation -> Conversation -> H.ComponentHTML Query
  renderContactDetails selectedItem current =
    let o          = toContactDetails current
        isSelected = selectedItem == Just current
    in
    HH.div
      [ HP.class_ $ HC.ClassName $ "conversation-list-item contact" <> if isSelected then " selected" else ""
      , HE.onClick (HE.input_ (Select current))
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
              [ HH.text (fromMaybe "" o.name)
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
          , renderLastMessage $ fromMaybe "" o.lastMessage
          ]
      ]

renderLastMessage :: String -> H.ComponentHTML Query
renderLastMessage s =
  HH.p
    [ HP.class_ $ HC.ClassName "last-message"
    , HH.attr (HC.AttrName "dir") "auto"
    ]
    [ HH.text s ]

type ContactDetails =
  { name        :: Maybe String
  , lastMessage :: Maybe String
  }

toContactDetails :: Conversation -> ContactDetails
toContactDetails (Group o) =
  { name        : o.name
  , lastMessage : o.lastMessage
  }
toContactDetails (Private o) =
  { name : o.name <|> o.profileName <|> (Just $ PN.formatString o.id PN.National)
  , lastMessage : o.lastMessage
  }
