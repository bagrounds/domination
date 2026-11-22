module Test.Standings (standings_tests) where

import Prelude

import Data.Array (all, filter, length, nub, sort, zipWith, (..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (maximum)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Cards as Cards
import Domination.Data.Game as Game
import Domination.Data.Game.Engine (gameResult)
import Domination.Data.Player (Player, newPlayer)
import Domination.Data.Player as Player
import Data.Lens.Getter (view)
import Domination.Data.Points (Points)
import Domination.Data.Points as Points
import Domination.Data.Result as GameResult
import Domination.Data.Stack (Stack)
import Domination.Data.Stack as Stack
import Domination.Data.Standings (Standing, finalDeck, standings)
import Domination.UI.Standings (ordinal)
import Effect (Effect)
import Test.QuickCheck (Result, assertEquals, (<?>))
import Test.QuickCheck.Gen (Gen, chooseInt, elements, vectorOf)
import Test.Room (all_success, quick_check)

type EffectTest = { name :: String, test :: Effect Result }

standings_tests :: Array EffectTest
standings_tests =
  [ { name: "∀ players: each standing's score and card count are the player's, and its deck adds up to both"
    , test: quick_check 300 do
        players <- playersGen
        pure $ all
          (\s -> s.score == Player.score (playerAt players s.playerIndex)
            && sum (Stack.points <$> s.deck) == s.score
            && s.cardCount == length (Player.allCards (playerAt players s.playerIndex))
            && sum (_.count <$> s.deck) == s.cardCount)
          (NEA.toArray $ standings players)
          <?> describe players
    }
  , { name: "∀ players: standings list every player once, ordered by place then seat"
    , test: quick_check 300 do
        players <- playersGen
        let ranked = NEA.toArray $ standings players
        pure $ (sort (_.playerIndex <$> ranked) == 0 .. (NEA.length players - 1)
          && all identity (zipWith inOrder ranked (Array.drop 1 ranked)))
          <?> describe players
    }
  , { name: "∀ players: place is 1 + the number of players with a higher score"
    , test: quick_check 300 do
        players <- playersGen
        let scores = NEA.toArray $ Player.score <$> players
        pure $ all
          (\s -> s.place == one + length (filter (_ > s.score) scores))
          (NEA.toArray $ standings players)
          <?> describe players
    }
  , { name: "∀ players: leaders are 0 behind, everyone else trails by their gap to the leader"
    , test: quick_check 300 do
        players <- playersGen
        let leaderScore = maximum $ Player.score <$> players
        pure $ all
          (\s -> s.behindLeader == leaderScore - s.score
            && s.behindLeader >= zero
            && (s.place == one) == (s.behindLeader == zero))
          (NEA.toArray $ standings players)
          <?> describe players
    }
  , { name: "∀ players: first place is exactly the game result's winners"
    , test: quick_check 300 do
        players <- playersGen
        let
          game = (Game.new (NEA.length players) Cards.cardMap false) { players = players }
          firstPlace = _.playerIndex <$> filter ((_ == one) <<< _.place) (NEA.toArray $ standings players)
        pure $ (Just firstPlace == (winners <$> gameResult game))
          <?> describe players <> " first place " <> show firstPlace
            <> " result " <> show (gameResult game)
    }
  , { name: "∀ player: the final deck has one stack per card owned, in points then count order"
    , test: quick_check 300 do
        player <- playerGen
        let
          deck = finalDeck player
          names = _.name <$> Player.allCards player
          count name = length $ filter (_ == name) names
        pure $ (sort (_.card.name <$> deck) == sort (nub names)
          && all (\stack -> stack.count == count stack.card.name) deck
          && all identity (zipWith deckOrder deck (Array.drop 1 deck)))
          <?> show names
    }
  , { name: "a three player example: a tie for first, then a one point loss"
    , test: pure do
        let
          ranked = NEA.toArray $ standings $ NEA.cons'
            (playerWith [ Cards.province, Cards.province, Cards.copper ])
            [ playerWith [ Cards.province, Cards.duchy, Cards.curse, Cards.duchy ]
            , playerWith [ Cards.province, Cards.province ]
            ]
        all_success
          [ assertEquals
            (summarize <$> ranked)
            [ { playerIndex: 0, place: 1, score: 12, behindLeader: 0 }
            , { playerIndex: 2, place: 1, score: 12, behindLeader: 0 }
            , { playerIndex: 1, place: 3, score: 11, behindLeader: 1 }
            ]
          , assertEquals
            (deckSummaries <$> Array.head ranked)
            (Just
              [ { name: "Province", count: 2, points: 12 }
              , { name: "Copper", count: 1, points: 0 }
              ])
          , assertEquals
            (deckSummaries <$> Array.last ranked)
            (Just
              [ { name: "Duchy", count: 2, points: 6 }
              , { name: "Province", count: 1, points: 6 }
              , { name: "Curse", count: 1, points: -1 }
              ])
          ]
    }
  , { name: "ordinal examples"
    , test: pure $ assertEquals
        (ordinal <$> [ 1, 2, 3, 4, 10, 11, 12, 13, 21, 22, 23, 101, 111, 112 ])
        [ "1st", "2nd", "3rd", "4th", "10th", "11th", "12th", "13th"
        , "21st", "22nd", "23rd", "101st", "111th", "112th"
        ]
    }
  ]

inOrder :: Standing -> Standing -> Boolean
inOrder a b = a.place < b.place
  || (a.place == b.place && a.playerIndex < b.playerIndex)

winners :: GameResult.Result -> Array Int
winners = case _ of
  GameResult.Victory playerIndex -> [ playerIndex ]
  GameResult.Tie playerIndices -> playerIndices

summarize
  :: Standing
  -> { playerIndex :: Int, place :: Int, score :: Int, behindLeader :: Int }
summarize { playerIndex, place, score, behindLeader } =
  { playerIndex, place, score: toInt score, behindLeader: toInt behindLeader }

deckOrder :: Stack -> Stack -> Boolean
deckOrder a b = Stack.points a > Stack.points b
  || (Stack.points a == Stack.points b && a.count >= b.count)

deckSummaries
  :: Standing
  -> Array { name :: String, count :: Int, points :: Int }
deckSummaries = map summarizeSource <<< _.deck
  where
    summarizeSource stack =
      { name: stack.card.name
      , count: stack.count
      , points: toInt $ Stack.points stack
      }

toInt :: Points -> Int
toInt = view Points._int

playerAt :: NonEmptyArray Player -> Int -> Player
playerAt players i = case NEA.index players i of
  Just player -> player
  Nothing -> newPlayer

describe :: NonEmptyArray Player -> String
describe players = show $ NEA.toArray $ map _.name <<< Player.allCards <$> players

playerWith :: Array Card.CardSpec -> Player
playerWith specs = newPlayer { discard = Card._card <$> specs }

cardPool :: NonEmptyArray Card
cardPool = Card._card <$> NEA.cons' Cards.copper
  [ Cards.silver, Cards.gold, Cards.estate, Cards.duchy, Cards.province
  , Cards.curse, Cards.greatHall, Cards.harem, Cards.village, Cards.nobles
  ]

pileGen :: Gen (Array Card)
pileGen = chooseInt 0 6 >>= flip vectorOf (elements cardPool)

playerGen :: Gen Player
playerGen = withPiles
  <$> pileGen <*> pileGen <*> pileGen <*> pileGen <*> pileGen <*> pileGen
  where
    withPiles hand deck discard atPlay toDiscard buying =
      newPlayer { hand = hand, deck = deck, discard = discard, atPlay = atPlay, toDiscard = toDiscard, buying = buying }

playersGen :: Gen (NonEmptyArray Player)
playersGen = do
  count <- chooseInt 1 6
  NEA.cons' <$> playerGen <*> vectorOf (count - 1) playerGen
