-- | The final standings of a game: every player's place, score, how far
-- | behind the leader they finished, and the full deck they finished with.
-- |
-- | Everything here is derived from the players' final cards with the same
-- | scoring rule the game uses (`Player.score`), so the standings always agree
-- | with the game's result. Players with equal scores share a place.
module Domination.Data.Standings
  ( Standing
  , finalDeck
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
  , cardCount :: Int
  , deck :: Array Stack
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
      , cardCount: length $ Player.allCards player
      , deck: finalDeck player
      }
      where
        score = Player.score player

-- | Every card a player owns, one stack per card name: the cards worth the
-- | most points first, then the most numerous, then alphabetically.
finalDeck :: Player -> Array Stack
finalDeck =
  sortBy
    ( flip (comparing Stack.points)
      <> flip (comparing _.count)
      <> comparing _.card.name
    )
    <<< stackCards
    <<< Player.allCards
