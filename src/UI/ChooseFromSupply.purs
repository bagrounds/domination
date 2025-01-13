--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Component that manages choosing cards from a supply.
--|
--| ### Key Concepts
--| * **Domination Capability**: Understanding the `Dom` and `Log` classes, which provide the foundation for Domination's functionality.
--| * **Component Structure**: Recognizing the component's main components, including `initialState`, `render`, and `eval`.
--| * **State and Actions**: Familiarity with the `State` type and the two types of actions: `Toggle Int` and `Done`.
module Domination.UI.ChooseFromSupply where

import Prelude

import Data.Array (filter, find)
import Data.Foldable (length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.Data.Card (Card)
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Domination.UI.DomSlot (DomSlot)
import Halogen as H
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Array (Tuple Card Boolean)

data Action
  = Toggle Int
  | Done

type ComponentSpec =
  { cards :: Array Card
  , baseSlotNumber :: Int -> DomSlot
  }
component
  :: forall query m
  . Dom m
  => Log m
  => ComponentSpec
  -> Component query Unit (Maybe String) m
component { cards, baseSlotNumber } =
  H.mkComponent { initialState, render, eval }
    where
    initialState :: forall a. a -> Array (Tuple Card Boolean)
    initialState _ = (\x -> Tuple x false) <$> cards
    render xs = HH.div_ $
          [ HH.text "Choose a card"
          , HH.p_
            [ HH.button
              [ HP.class_ Css.resolveChoice
              , HE.onClick \_ -> Done
              ]
              [ HH.text "Done" ]
            ]
          ]
          <> renderCard `mapWithIndex` xs

    eval = H.mkEval H.defaultEval
      { handleAction = case _ of
        Toggle i -> do
          xs <- H.get
          let total = length $ snd `filter` xs
          H.modify_
            $ mapWithIndex
              \j (Tuple c selected) -> Tuple c
                $ if i == j && (selected || total == 0)
                  then not selected
                  else selected
        Done -> (resolution <$> H.get) >>= H.raise
      }
      where
        resolution :: State -> Maybe String
        resolution xs = (_.name <<< fst) <$> find snd xs

    renderCard cardIndex (Tuple card selected) =
      Card.render onClick extraClasses card (baseSlotNumber cardIndex)
      where
        onClick _ = Toggle cardIndex
        extraClasses =
          [ if selected
            then Css.toTrash
            else Css.toKeep
          ]
