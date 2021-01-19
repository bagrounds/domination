module Domination.UI.PickN
  ( component
  ) where

import Prelude

import Data.Array (filter, (:))
import Data.Foldable (length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Data.Card (Card)
import Domination.Data.Choice (Choice)
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.UI.Card as Card
import Domination.UI.Choice as Choice
import Domination.UI.Css as Css
import Domination.UI.Util (h2__)
import Halogen (Component)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Halogen.Query.HalogenQ (HalogenQ)

type State = Array { i :: Int, selected :: Boolean, choice :: Choice }

data Action
  = Toggle Int
  | Done

component
  :: forall query input m
  . { title :: String, n :: Int, choices :: Array Choice }
  -> Component HTML query input (Array Choice) m
component { n, title, choices } =
  H.mkComponent { initialState, render, eval }
    where
    initialState :: forall a. a -> State
    initialState _ = (\i choice -> { choice, i, selected: false })
      `mapWithIndex` choices

    render xs = HH.div_ $ (h2__ title)
      : HH.p_
        [ HH.button
          [ HE.onClick \_ -> Just Done ]
          [ HH.text "Done choosing" ]
        ]
      : (xs <#> f)
      where
        f { i, selected, choice } =
          HH.button
            [ HE.onClick \_ -> Just (Toggle i)
            , HP.class_ if selected
              then Css.toTrash
              else Css.toKeep
            ]
            [ HH.text $ Choice.renderText' choice ]

    eval
      :: forall a
      . HalogenQ query Action input a
      -> HalogenM State Action () (Array Choice) m a
    eval = H.mkEval H.defaultEval
      { handleAction = case _ of
        Toggle j -> do
          xs <- H.get
          let total = length $ _.selected `filter` xs
          H.modify_ $ map \{ i, selected, choice } ->
            if i == j && selected
            then { i, selected: false, choice }
            else if i == j && not selected && total < n
            then { i, selected: true, choice }
            else { i, selected, choice }
        Done -> ((filter (_.selected) >>> map _.choice) <$> H.get) >>= H.raise
      }

