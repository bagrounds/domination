module Domination.Data.NormalGame where

import Prelude

import Data.Array (replicate, snoc)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Domination.Data.Phase (Phase(..))

newtype Order = Order Int
newtype Quantity = Q Int

newtype CardID = CardID Int
newtype PlayerID = PlayerID Int
newtype PileID = PileID Int
newtype ChoiceID = ChoiceID Int
newtype ReactionID = ReactionID Int
newtype CardClass = CardClass Int

type Table a b = Array (Tuple a b)
type CardTable a = Table CardID a
type PlayerTable a = Table PlayerID a
type PileTable a = Table PileID a
data PileType
  = Trash
  | Deck
  | Hand
  | Discard
  | AtPlay
  | Buying
  | Discarding

type NormalGame =
  { turn :: PlayerID

  , phase :: Phase

  , supply :: Array Quantity

  , pileType :: Array PileType
  , pileOwner :: PileTable PlayerID

  , card :: Array CardID
  , cardClass :: CardTable CardClass
  , cardPile :: CardTable PileID
  , cardOrder :: CardTable Order

  , playerActions :: Array Quantity
  , playerBuys :: Array Quantity
  , playerChoices :: PlayerTable ChoiceID
  , playerReaction :: PlayerTable ReactionID
  , playerBonusCash :: PlayerTable Quantity
  }

type DeNormalGame =
  { turn :: PlayerID
  , phase :: Phase
  , supply :: Array Quantity
  , pile :: Array Pile
  , card :: Array (Maybe Card)
  , player :: Array Player
  }

data Pile = Pile PileType (Maybe PlayerID)
data Card = Card CardClass PileID Order
data Player = Player
  Quantity -- actions
  Quantity -- buys
  (Maybe ChoiceID)
  (Maybe ReactionID)
  Quantity -- bonusCash

example :: DeNormalGame
example =
  { turn: PlayerID 0
  , phase: ActionPhase
  , supply: Q
    <$> replicate 4 20
    <> replicate 4 8
    `snoc` 0
    <> replicate 4 10
    `snoc` 8
    <> replicate 5 10
  , pile:
    [ Pile Trash Nothing
    , Pile Deck $ Just $ PlayerID 0
    , Pile Hand $ Just $ PlayerID 0
    , Pile Discard $ Just $ PlayerID 0
    , Pile AtPlay $ Just $ PlayerID 0
    , Pile Buying $ Just $ PlayerID 0
    , Pile Discarding $ Just $ PlayerID 0
    ]
  , card: Just <$>
    [ Card (CardClass 0) (PileID 3) (Order 0)
    , Card (CardClass 0) (PileID 3) (Order 1)
    , Card (CardClass 0) (PileID 3) (Order 2)
    , Card (CardClass 0) (PileID 3) (Order 3)
    , Card (CardClass 0) (PileID 3) (Order 4)
    , Card (CardClass 0) (PileID 3) (Order 5)
    , Card (CardClass 0) (PileID 3) (Order 6)
    , Card (CardClass 4) (PileID 3) (Order 7)
    , Card (CardClass 4) (PileID 3) (Order 8)
    , Card (CardClass 4) (PileID 3) (Order 9)
    ]
  , player:
    [ Player (Q 1) (Q 1) Nothing Nothing (Q 0)
    ]
  }
