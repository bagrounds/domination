-- | The game over view: the result, a compact standings table, then each
-- | player's final deck in its own foldable section.
module Domination.UI.Standings
  ( ordinal
  , render
  ) where

import Prelude

import Data.Array (filter, (:))
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Lens.Getter ((^.))
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Data.Points (Points)
import Domination.Data.Points as Points
import Domination.Data.Result (Result)
import Domination.Data.Stack (Stack)
import Domination.Data.Stack as Stack
import Domination.Data.Standings (Standing)
import Domination.UI.RenderText (renderText)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

render :: forall w i. Int -> Result -> NonEmptyArray Standing -> HTML w i
render viewerIndex result standings = HH.div
  [ HP.class_ $ ClassName "game-over" ]
  $ renderText result
  : HH.table
    [ HP.class_ $ ClassName "standings-table" ]
    [ HH.thead_
      [ HH.tr_ $ (HH.th_ <<< pure <<< HH.text)
        <$> [ "Place", "Player", "Score", "Behind" ]
      ]
    , HH.tbody_ $ toArray $ renderStanding viewerIndex <$> standings
    ]
  : HH.h3_ [ HH.text "Final decks" ]
  : toArray (renderFinalDeck viewerIndex <$> standings)

renderStanding :: forall w i. Int -> Standing -> HTML w i
renderStanding viewerIndex { playerIndex, place, score, behindLeader } = HH.tr
  [ HP.classes $ (ClassName <<< fst) <$> filter snd
    [ Tuple "winner" (place == one)
    , Tuple "viewer" (playerIndex == viewerIndex)
    ]
  ]
  [ HH.td_ [ HH.text $ ordinal place ]
  , HH.td_ [ HH.text $ playerLabel viewerIndex playerIndex ]
  , HH.td_ [ renderText score ]
  , HH.td_ [ HH.text $ behindText behindLeader ]
  ]

renderFinalDeck :: forall w i. Int -> Standing -> HTML w i
renderFinalDeck viewerIndex { playerIndex, cardCount, deck } = HH.details
  [ HP.class_ $ ClassName "final-deck" ]
  [ HH.summary_
    [ HH.text $ playerLabel viewerIndex playerIndex
      <> " · " <> cardsText cardCount
    ]
  , case deck of
    [] -> HH.p_ [ HH.text "No cards" ]
    stacks -> HH.table_ $ renderStack <$> stacks
  ]

renderStack :: forall w i. Stack -> HTML w i
renderStack stack = HH.tr_
  [ HH.td_ [ HH.text stack.card.name ]
  , HH.td_ [ HH.text $ "×" <> show stack.count ]
  , HH.td_ $ renderPoints $ Stack.points stack
  ]

renderPoints :: forall w i. Points -> Array (HTML w i)
renderPoints points
  | points == zero = []
  | otherwise = [ renderText points ]

playerLabel :: Int -> Int -> String
playerLabel viewerIndex playerIndex =
  "Player " <> show (playerIndex + one)
    <> if playerIndex == viewerIndex then " (you)" else ""

cardsText :: Int -> String
cardsText = case _ of
  1 -> "1 card"
  n -> show n <> " cards"

behindText :: Points -> String
behindText behindLeader = case behindLeader ^. Points._int of
  0 -> "—"
  n -> "-" <> show n

ordinal :: Int -> String
ordinal n = show n <> suffix
  where
    suffix = case n `mod` 100, n `mod` 10 of
      tens, _ | tens >= 11 && tens <= 13 -> "th"
      _, 1 -> "st"
      _, 2 -> "nd"
      _, 3 -> "rd"
      _, _ -> "th"
