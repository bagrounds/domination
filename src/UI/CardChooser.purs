module Domination.UI.CardChooser
  ( component
  ) where

import Prelude

import Data.Array (filter)
import Data.Foldable (length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Data.Card (Card)
import Domination.Data.Choice (Choice)
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Domination.UI.Util (h2__)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Array (Tuple Card Boolean)

data Action
  = Toggle Int
  | Done

type RenderChoice
  = Choice
  -> Maybe { title :: String, buttonText :: String }

type CanToggle = { selected :: Boolean, total :: Int } -> Boolean

type ComponentSpec =
  { renderChoice :: RenderChoice
  , canToggle :: CanToggle
  , resolve :: Maybe (Array Int) -> Choice
  , player :: Player
  , choice :: Choice
  , pile :: Pile
  }
--component
--  :: forall query input m
--  . ComponentSpec
--  -> Component HTML query input Choice m
component { renderChoice, canToggle, resolve, player, choice, pile } =
  H.mkComponent { initialState, render, eval }
    where
    initialState :: forall a. a -> Array (Tuple Card Boolean)
    initialState _ = (\x -> Tuple x false) <$> cards
      where
        cards = case pile of
          Pile.Hand -> player.hand
          Pile.Discard -> player.discard
          Pile.Deck -> player.deck
          Pile.Trash -> []
    render xs =
      case Player.firstChoice player >>= renderChoice of
        Just { title, buttonText } -> HH.div_ $
          [ h2__ title
          , HH.p_
            [ HH.button
              [ HP.class_ Css.resolveChoice
              , HE.onClick \_ -> Just $ Done
              ]
              [ HH.text buttonText ]
            ]
          ]
          <> renderCardToTrash `mapWithIndex` xs
        Nothing -> h2__ "Something has gone terribly wrong!"

--    eval
--      :: forall a
--      . HalogenQ query Action input a
--      -> HalogenM State Action () Choice m a
    eval = H.mkEval H.defaultEval
      { handleAction = case _ of
        Toggle i -> do
          xs <- H.get
          let total = length $ snd `filter` xs
          H.modify_
            $ mapWithIndex
              \j (Tuple c selected) -> Tuple c
                $ if i == j
                  && canToggle { selected, total }
                  then not selected
                  else selected
        Done -> ((resolve <<< resolution) <$> H.get) >>= H.raise
      }
      where
        resolution :: State -> Maybe (Array Int)
        resolution xs =
          Just $ map snd
          $ filter fst
          $ (\i (Tuple _ b) -> (Tuple b i)) `mapWithIndex` xs

--    renderCardToTrash
--      :: forall w
--      . Int
--      -> Tuple Card Boolean
--      -> HTML w Action
    renderCardToTrash cardIndex (Tuple card selected) =
      Card.render onClick extraClasses card
      where
        onClick _ = Just (Toggle cardIndex)
        extraClasses =
          [ if selected
            then Css.toTrash
            else Css.toKeep
          ]

