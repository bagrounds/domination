module Domination.Data.NormalGame where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (catMaybes, elemIndex, nubEq, (!!))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum.Generic (genericPred, genericSucc)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.HeytingAlgebra (ff)
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso, re)
import Data.Lens.Lens.Tuple (_1, _2)
import Data.Lens.Setter (over)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Domination.Capability.Random (class Random)
import Domination.Data.Actions (Actions)
import Domination.Data.Bonus (Bonus)
import Domination.Data.Buys (Buys)
import Domination.Data.Card (Card, _choice)
import Domination.Data.CardID (CardID(..))
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice)
import Domination.Data.Choice as Choice
import Domination.Data.Lattice ((/\), (\/))
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Pile (Pile(..), playerPiles)
import Domination.Data.PileID (PileID(..))
import Domination.Data.PlayerID (PlayerID(..))
import Domination.Data.PlayerID as PlayerID
import Domination.Data.Result (Result)
import Domination.Data.Stack as Stack
import Domination.Data.Supply as SupplyObject
import Domination.Data.Table (Table(..), Table2(..), cardinality, (-#>), (-=>), (...), (<?<), (?), (?$), (?&?), (?@), (|=|), (|>), (|>|))
import Domination.Data.Table as Table
import Domination.Data.Wire.Card as Card
import Domination.Data.Wire.Int as Int
import Safe.Coerce (coerce)
import Util ((.^), (<<~))

newtype Order = Order Int
derive instance genericOrder :: Generic Order _
instance enumOrder :: Enum Order where
  succ = genericSucc
  pred = genericPred
instance boundedOrder :: Bounded Order where
  bottom = Order zero
  top = Order top
instance boundedEnumOrder :: BoundedEnum Order where
  cardinality = Cardinality $ top + 1
  toEnum i = let o = Order i in
    if o > top
    || o < bottom
    then Nothing
    else Just $ coerce i
  fromEnum = coerce
derive instance eqOrder :: Eq Order
derive instance ordOrder :: Ord Order
derive newtype instance semiringOrder :: Semiring Order
derive newtype instance hashableOrder :: Hashable Order

newtype CardClass = CardClass Int
derive instance eqCardClass :: Eq CardClass
derive newtype instance hashableCardClass :: Hashable CardClass

newtype ChoiceID = ChoiceID Int
derive instance ordChoiceID :: Ord ChoiceID
derive instance genericChoiceID :: Generic ChoiceID _
instance enumChoiceID :: Enum ChoiceID where
  succ = genericSucc
  pred = genericPred
instance boundedChoiceID :: Bounded ChoiceID where
  bottom = ChoiceID zero
  top = ChoiceID top
derive instance eqChoiceID :: Eq ChoiceID
derive newtype instance hashableChoiceID :: Hashable ChoiceID

newtype ChoiceClass = ChoiceClass Int
derive instance eqChoiceClass :: Eq ChoiceClass
derive newtype instance hashableChoiceClass :: Hashable ChoiceClass

type NormalGame =
  { longGame :: Boolean
  , phase :: Phase
  , result :: Maybe Result
  , turn :: PlayerID

  -- |card| = NumCard = (constant)
  , card :: Table CardID
  -- all cards have 1 class
  -- all classes have zero or more cards
  , card_class :: Table2 CardID CardClass
  -- all cards have 1 pile
  -- all piles have 1 or more card
  , card_pile :: Table2 CardID PileID
  -- all cards have zero or more orders
  -- all orders have zero or more cards
  , card_order :: Table2 CardID Order

  -- |pile| = 2 + 6 * |player|
  , pile :: Table PileID
  -- all piles have zero or 1 player
  -- all players have N piles
  , pile_owner :: Table2 PileID PlayerID
  -- all piles have 1 Pile
  -- all Piles have 1 or more piles
  , pile_type :: Table2 PileID Pile

  -- |player| <- [1, *] (constant)
  , player :: Table PlayerID
  -- all players have 1 Actions
  -- all actions have zero or more players
  , player_actions :: Table2 PlayerID Actions
  -- all players have 1 Bonus
  -- all bonuses have zero or more players
  , player_bonus :: Table2 PlayerID Bonus
  -- all players have 1 Buys
  -- all Buys have zero or more players
  , player_buys :: Table2 PlayerID Buys
  -- all players have zero or more choices
  -- all choices have zero or more players
  , player_choice :: Table2 PlayerID ChoiceID

  -- |choice| <- [0, *] (variable)
  , choice :: Table ChoiceID
  -- all choices have 1 ChoiceClass
  -- all ChoiceClasses have zero or more choices
  , choice_class :: Table2 ChoiceID ChoiceClass
  -- all choices have 1 ChoiceOrder
  -- all ChoiceOrders have zero or more choices
  , choice_order :: Table2 ChoiceID Order
  }

isAttacked :: PlayerID -> NormalGame -> Boolean
isAttacked pid game = pid ? attacked game

choicesOutstanding :: NormalGame -> Boolean
choicesOutstanding { choice } = choice |>| zero

cleanup
  :: forall m
  . Random m
  => NormalGame
  -> m NormalGame
cleanup game = pure game
  { phase = Phase.next game.phase
  , result = game.result -- wrong
  , turn = PlayerID.nextPlayer game.turn
  , card_pile = game.card_pile --wrong
  , card_order = game.card_order --wrong
  , player_actions = game.player_actions --wrong
  , player_bonus = game.player_bonus --wrong
  , player_buys = game.player_buys --wrong
  , player_choice = game.player_choice --wrong
  , choice = game.choice --wrong
  , choice_class = game.choice_class --wrong
  , choice_order = game.choice_order --wrong
  }
  where
    currentPlayer = game.turn
    cardsOwnedByCurrentPlayer = cardsOwnedBy currentPlayer game
    pilesToDiscard = cleanupCards game
    newDiscard = pilesToDiscard /\ cardsOwnedByCurrentPlayer
    newAtPlay = Table.empty
    newHand = Table.empty
    newToDiscard = Table.empty
    newBuying = Table.empty
    newActions = one
    newBuys = one
    newBonuses = Table.empty
    currentDeck = cardsOwnedByCurrentPlayer /\ piles Deck game
    sizeOfCurrentDeck = cardinality currentDeck

cleanupCards :: NormalGame -> Table CardID
cleanupCards =
  piles Discard
  \/ piles AtPlay
  \/ piles Hand
  \/ piles Discarding
  \/ piles Buying

isEmpty :: PileID -> NormalGame -> Boolean
isEmpty pileID { card_pile } = (card_pile ?@ pileID) |=| 0

drawCard
  :: forall m
  . Random m
  => PlayerID
  -> NormalGame
  -> m NormalGame
drawCard playerID game = do
  let playerDeck = playerPileCards playerID Deck game
  game' <-
    if playerDeck |=| 0
    then shuffleFor playerID game
    else pure game
  -- TODO: finish this
  pure game'

shuffleFor
  :: forall m
  . Random m
  => PlayerID
  -> NormalGame
  -> m NormalGame
shuffleFor playerID game@{ card_pile, pile_type, pile_owner } = do
  let
    playerDiscard = playerPileCards playerID Discard
    newCardPile = Table.empty
    playerPiles :: Table PileID
    playerPiles = pile_owner ?@ playerID
    discardPiles :: Table PileID
    discardPiles = pile_type ?@ Discard
    playerDiscardPileID :: Table PileID
    playerDiscardPileID = playerPiles /\ discardPiles
    discards :: Table CardID
    discards = card_pile ?$ playerDiscardPileID
    -- TODO: figure out how to write a lens that targets multiple
    -- elements of a set so we can set them all to a new value
    -- new_card_pile = card_pile ?=~
  -- TODO: finish this
  pure game

cardsOwnedBy :: PlayerID -> NormalGame -> Table CardID
cardsOwnedBy player game = card_owner game -=> player |> _1

card_owner :: NormalGame -> Table2 CardID PlayerID
card_owner { card_pile, pile_owner } = pile_owner <?< card_pile

piles :: Pile -> NormalGame -> Table CardID
piles pileType game = card_pileType game -=> pileType |> _1

playerPileCards :: PlayerID -> Pile -> NormalGame -> Table CardID
playerPileCards playerID pileType =
  cardsOwnedBy playerID /\ cardsInPileType pileType

card_pileType :: NormalGame -> Table2 CardID Pile
card_pileType { card_pile, pile_type } = pile_type <?< card_pile

cardsInPileType :: Pile -> NormalGame -> Table CardID
cardsInPileType pileType game = card_pileType game ?@ pileType

attacked :: NormalGame -> Table PlayerID
attacked { player_choice, choice_class, choice_order } =
  player_choice ?$ currentAttacks
  where
    currentAttacks :: Table ChoiceID
    currentAttacks = attacks /\ firstChoices
    firstChoices = choice_order -=> one |> _1
    attacks = choice_class -#> isAttack |> _1
    isAttack = _classChoice <<~ Choice.isAttack

_card_class :: Iso' Card CardClass
_card_class = iso to from
  where
    to card = CardClass $ card ^. Card._toWire .^ Int._toWire
    from (CardClass i) = i ^. Int._toWire .^ Card._toWire

_choice_class :: Iso' Choice ChoiceClass
_choice_class = iso to from
  where
    to choice = ChoiceClass
      $ fromMaybe (-1)
      $ elemIndex choice choiceUniverse
    from (ChoiceClass i) = fromMaybe Cards.emptyChoice
      $ choiceUniverse !! i

_classChoice :: Iso' ChoiceClass Choice
_classChoice = re _choice_class

cardUniverse :: Array Card
cardUniverse = Cards.cardMap

choiceUniverse :: Array Choice
choiceUniverse = nubEq $ catMaybes
  $ map (view _choice <<< _.command) <<< _.special <$> cardUniverse

new
  :: forall m
  . MonadError String m
  => Int
  -> Array Card
  -> Boolean
  -> m NormalGame
new playerCount cards longGame = do
  let
    allCards :: Array Card
    allCards = SupplyObject.makeSupply playerCount cards
      >>= Stack.toCards

    card :: Table CardID
    card = Table.fromFoldable
      $ CardID <$> const `mapWithIndex` allCards

    maybeCardClasses :: Array (Maybe CardClass)
    maybeCardClasses = map CardClass
      <<< flip elemIndex Cards.cardMap
      <$> allCards

    card_classes :: Array CardClass
    card_classes = catMaybes maybeCardClasses

    card_class :: Table2 CardID CardClass
    card_class = card ?&? Table.fromFoldable card_classes

    pileCount = 2 + playerCount * 6

    pile :: Table PileID
    pile = coerce $ zero ... pileCount

    player :: Table PlayerID
    player = coerce $ zero ... playerCount

    pilePlayerType
      :: Array (Tuple PileID (Tuple (Maybe PlayerID) Pile))
    pilePlayerType =
      mapWithIndex (Tuple <<< PileID)
      $ [ Tuple Nothing Supply, Tuple Nothing Trash ]
      <> ((Table.toUnfoldable player)
      >>= (\p -> Tuple (Just p) <$> playerPiles))

    pile_owner :: Table2 PileID PlayerID
    pile_owner = Table2 $ Table.fromFoldable
      $ catMaybes
      $ sequenceTupleMaybeSnd <<< over _2 (view _1)
      <$> pilePlayerType

    pile_type :: Table2 PileID Pile
    pile_type = Table2 $ Table.fromFoldable
      $ over _2 (view _2) <$> pilePlayerType

  let
    supplyPile = pile_type -=> Supply |> _1
    card_pile :: Table2 CardID PileID
    card_pile = card ?&? supplyPile

    card_order :: Table2 CardID Order
    card_order = Table2 Table.empty

    player_actions :: Table2 PlayerID Actions
    player_actions =  player ?&? Table.singleton one

    player_bonus :: Table2 PlayerID Bonus
    player_bonus = Table2 Table.empty

    player_buys :: Table2 PlayerID Buys
    player_buys = player ?&? Table.singleton one

    player_choice :: Table2 PlayerID ChoiceID
    player_choice = Table2 Table.empty

    allChoices :: Array Choice
    allChoices = catMaybes $ map (view _choice <<< _.command)
      <<< _.special
      <$> allCards

    choice :: Table ChoiceID
    choice = Table.fromFoldable
      $ ChoiceID <$> const `mapWithIndex` allChoices

    choice_class :: Table2 ChoiceID ChoiceClass
    choice_class = Table.emptyTable2

    choice_order :: Table2 ChoiceID Order
    choice_order = ff

  pure
    { longGame
    , phase: ActionPhase
    , result: Nothing
    , turn: PlayerID zero
    , card
    , card_class
    , card_pile
    , card_order
    , pile
    , pile_owner
    , pile_type
    , player
    , player_actions
    , player_bonus
    , player_buys
    , player_choice
    , choice
    , choice_class
    , choice_order
    }

sequenceTupleMaybeFst
  :: forall a b. Tuple (Maybe a) b -> Maybe (Tuple a b)
sequenceTupleMaybeFst (Tuple maybeA b) = (flip Tuple b) <$> maybeA

sequenceTupleMaybeSnd
  :: forall a b. Tuple a (Maybe b) -> Maybe (Tuple a b)
sequenceTupleMaybeSnd (Tuple a maybeB) = (Tuple a) <$> maybeB

