module Domination.UI.Rename (render) where

import Prelude

import AppAction (AppAction(..))
import Data.Array (mapWithIndex)
import Data.Map as Map
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.Data.Card (Card)
import Domination.Data.Cards as Cards
import Domination.Data.Retheme (Proposals, RenameTarget(..), Renameable, cardRenameables, changeCount, fieldLabel, previewCard, proposal, termRenameables)
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Domination.UI.DomSlot (Area(..), DomSlot(..))
import Halogen.Component (ComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)

type RenderedForm a b r m = HTML
  ( ComponentSlot
    (description :: Slot a b DomSlot | r) m AppAction
  ) AppAction

render
  :: forall a b r m
  . Dom m
  => Log m
  => Proposals
  -> RenderedForm a b r m
render proposals = HH.div
  [ HP.class_ Css.renameForm ]
  [ HH.p_
    [ HH.text $ "Suggest new names for cards, card text and game terms."
      <> " Blank fields keep their current text."
    ]
  , renderActions proposals
  , HH.div
    [ HP.class_ Css.renameEntries ]
    [ HH.h3_ [ HH.text "Terms" ]
    , HH.div
      [ HP.class_ Css.renameTerms ]
      $ renderTermField proposals <$> termRenameables
    , HH.h3_ [ HH.text "Cards" ]
    , HH.div_ $ renderCardEntry proposals `mapWithIndex` Cards.cardMap
    ]
  ]

renderActions :: forall w. Proposals -> HTML w AppAction
renderActions proposals = HH.div
  [ HP.class_ Css.renameActions ]
  [ HH.button
    [ HE.onClick \_ -> CopyRenameMapping
    , HP.disabled $ count == zero
    ]
    [ HH.text $ "Copy " <> show count <> " proposed change(s)" ]
  , HH.button
    [ HE.onClick \_ -> ClearRenameProposals
    , HP.disabled $ Map.isEmpty proposals
    ]
    [ HH.text "Clear all" ]
  ]
  where
    count = changeCount proposals

renderCardEntry
  :: forall a b r m
  . Dom m
  => Log m
  => Proposals
  -> Int
  -> Card
  -> RenderedForm a b r m
renderCardEntry proposals i card = HH.div
  [ HP.class_ Css.renameCard ]
  [ Card.render (const DoNothing) [] (previewCard proposals card) (CardSlot RenameArea i)
  , HH.div
    [ HP.class_ Css.renameFields ]
    $ renderCardField proposals <$> cardRenameables card
  ]

renderTermField :: forall w. Proposals -> Renameable -> HTML w AppAction
renderTermField proposals renameable@{ target } = HH.label
  [ HP.class_ Css.renameField ]
  [ HH.span [ HP.class_ Css.renameLabel ] [ HH.text $ fieldLabel target ]
  , renameInput proposals renameable
  ]

renderCardField :: forall w. Proposals -> Renameable -> HTML w AppAction
renderCardField proposals renameable@{ target, current } = HH.label
  [ HP.class_ Css.renameField ]
  [ HH.span [ HP.class_ Css.renameLabel ] [ HH.text $ fieldLabel target ]
  , HH.span [ HP.class_ Css.renameCurrent ] [ HH.text current ]
  , renameInput proposals renameable
  ]

renameInput :: forall w. Proposals -> Renameable -> HTML w AppAction
renameInput proposals { target, current } = case target of
  CardText _ -> HH.textarea textProperties
  ReactionText _ -> HH.textarea textProperties
  _ -> HH.input $ [ HP.type_ HP.InputText ] <> textProperties
  where
    textProperties :: forall t. Array (HP.IProp (value :: String, placeholder :: String, onInput :: Event | t) AppAction)
    textProperties =
      [ HP.value $ proposal target proposals
      , HP.placeholder current
      , HE.onValueInput $ ProposeRename target
      ]
