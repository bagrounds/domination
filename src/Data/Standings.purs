-- | The final standings of a game: every player's place, score, how far
-- | behind the leader they finished, and the cards their points came from.
-- |
-- | Everything here is derived from the players' final cards with the same
-- | scoring rule the game uses (`Player.score`), so the standings always agree
-- | with the game's result. Players with equal scores share a place.
module Domination.Data.Standings
  ( Standing
  , scoreSources
  , standings
  ) where

import Prelude

import Data.Array (filter, length, sortBy)
import Data.Array.NonEmpty (NonEmptyArray, mapWithIndex)
import Data.Array.NonEmpty as NonEmpty
import Data.Semigroup.Foldable (maximum)
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Points (Points)
import Domination.Data.Stack (Stack, stackCards)
import Domination.Data.Stack as Stack

type Standing =
  { playerIndex :: Int
  , place :: Int
  , score :: Points
  , behindLeader :: Points
  , scoreSources :: Array Stack
  }

standings :: NonEmptyArray Player -> NonEmptyArray Standing
standings players =
  NonEmpty.sortBy (comparing _.place <> comparing _.playerIndex)
    $ standing `mapWithIndex` players
  where
    scores = NonEmpty.toArray $ Player.score <$> players
    leaderScore = maximum $ Player.score <$> players
    standing playerIndex player =
      { playerIndex
      , place: one + length (filter (_ > score) scores)
      , score
      , behindLeader: leaderScore - score
      , scoreSources: scoreSources player
      }
      where
        score = Player.score player

scoreSources :: Player -> Array Stack
scoreSources =
  sortBy (flip (comparing Stack.points) <> comparing _.card.name)
    <<< filter ((_ /= zero) <<< Stack.points)
    <<< stackCards
    <<< Player.allCards
