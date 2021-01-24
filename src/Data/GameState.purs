module Domination.Data.GameState where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (all, catMaybes, dropWhile, filter, find, findIndex, head, length, null, takeWhile, updateAt)
import Data.Either (Either(..))
import Data.Foldable (any, foldM)
import Data.Lens.Fold ((^?))
import Data.Lens.Getter (view, (^.))
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set, (.~), (<>~))
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicate)
import Domination.Capability.Random (class Random)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Card (Card, Command(..), Special)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice(..))
import Domination.Data.Choice as Choice
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter (Filter(..))
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player, handSizeIs)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack)
import Domination.Data.Stack as Stack
import Domination.Data.Target (Target(..))
import Relation (Relation(..))
import Rule (appendError, appendErrorOn, check, enforceOn, lengthIs, (!<>), (!>), (!@>), (<>!), (<@!))
import Util (assert, dropIndices, fromJust, indices, justIf, moveAll, takeIndices, withIndices)

type GameState =
  { turn :: Int
  , phase :: Phase
  , players :: Array Player
  , supply :: Supply
  , trash :: Array Card
  }

_turn :: Lens' GameState Int
_turn = prop (SProxy :: SProxy "turn")
_phase :: Lens' GameState Phase
_phase = prop (SProxy :: SProxy "phase")
_players :: Lens' GameState (Array Player)
_players = prop (SProxy :: SProxy "players")
_supply :: Lens' GameState Supply
_supply = prop (SProxy :: SProxy "supply")
_trash :: Lens' GameState (Array Card)
_trash = prop (SProxy :: SProxy "trash")


_player :: Int -> Traversal' GameState Player
_player i = _players <<< (ix i)
_stack :: Int -> Traversal' GameState Stack
_stack i = _supply <<< (ix i)

_ofPhase :: Phase -> Prism' GameState GameState
_ofPhase phase = prism' identity $ justIf ((==) phase <<< _.phase)

getPlayer
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m Player
getPlayer i = fromJust "cannot get player!" <<< (_ ^? _player i)

getPlayerHand
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m (Array Card)
getPlayerHand playerIndex = fromJust "cannot get player hand!"
  <<< (_ ^? _player playerIndex <<< Player._hand)

updatePlayer
  :: forall m
  . MonadError String m
  => Int
  -> Player
  -> GameState
  -> m GameState
updatePlayer i player state = fromJust "cannot update player!"
  $ updateAt i player state.players <#> flip (set _players) state

updateStack
  :: forall m
  . MonadError String m
  => Int
  -> Stack
  -> GameState
  -> m GameState
updateStack i stack state = fromJust "cannot update stack!"
  $ updateAt i stack state.supply <#> flip (set _supply) state

modifyStack
  :: forall m
  . MonadError String m
  => Int
  -> (Stack -> Stack)
  -> GameState
  -> m GameState
modifyStack i f state =
  getStack i state <#> f >>= flip (updateStack i) state

modifyPlayer
  :: forall m
  . MonadError String m
  => Int
  -> (Player -> Player)
  -> GameState
  -> m GameState
modifyPlayer i f state =
  getPlayer i state <#> f >>= flip (updatePlayer i) state

modifyPlayerM
  :: forall m
  . MonadError String m
  => Int
  -> (Player -> m Player)
  -> GameState
  -> m GameState
modifyPlayerM i f state =
  getPlayer i state >>= f >>= flip (updatePlayer i) state

modifyStackM
  :: forall m
  . MonadError String m
  => Int
  -> (Stack -> m Stack)
  -> GameState
  -> m GameState
modifyStackM i f state =
  getStack i state >>= f >>= flip (updateStack i) state

getStack
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m Stack
getStack i = fromJust "cannot get stack!" <<< (_ ^? _stack i)

stackByName
  :: forall m
  . MonadError String m
  => String
  -> GameState
  -> m Stack
stackByName cardName { supply } = fromJust "card not in supply!" $
  find (view (Stack._card <<< Card._name) >>> (_ == cardName)) supply

indexOfStack
  :: forall m
  . MonadError String m
  => Card
  -> GameState
  -> m Int
indexOfStack card = fromJust "card not in supply!"
  <<< findIndex (view Stack._card >>> (_ == card)) <<< _.supply

type Supply = Array Stack

newGame :: Int -> GameState
newGame playerCount =
  { turn: 0
  , phase: ActionPhase
  , players: replicate playerCount newPlayer
  , supply:
    [ { card: copper, count: treasureCount }
    , { card: silver, count: treasureCount }
    , { card: gold, count: treasureCount }
    , { card: platinum, count: treasureCount }
    , { card: estate, count: victoryCount }
    , { card: duchy, count: victoryCount }
    , { card: province, count: victoryCount }
    , { card: colony, count: victoryCount }
    , { card: curse, count: curseCount }
    , { card: chapel, count: kingdomCount }
    , { card: moat, count: kingdomCount }
    , { card: pawn, count: kingdomCount }
    , { card: consolation, count: kingdomCount }
    , { card: greatHall, count: victoryCount }
    , { card: village, count: kingdomCount }
    , { card: woodCutter, count: kingdomCount }
    , { card: steward, count: kingdomCount }
    , { card: monument, count: victoryCount }
    , { card: smithy, count: kingdomCount }
    , { card: workersVillage, count: kingdomCount }
    , { card: militia, count: kingdomCount }
    , { card: moneyLender, count: kingdomCount }
    , { card: bazaar, count: kingdomCount }
    , { card: festival, count: kingdomCount }
    , { card: laboratory, count: kingdomCount }
    , { card: market, count: kingdomCount }
    , { card: witch, count: kingdomCount }
    , { card: councilRoom, count: kingdomCount }
    , { card: scholar, count: kingdomCount }
    , { card: torturer, count: kingdomCount }
    , { card: harem, count: victoryCount }
    , { card: nobles, count: kingdomCount }
    ]
  , trash: []
  }
  where
    curseCount = 10 * (playerCount - 1)
    victoryCount = 4 * playerCount
    kingdomCount = 4 * playerCount
    treasureCount = 10 * playerCount

makeAutoPlay
  :: forall m
  . Random m
  => Play -> GameState -> m (Either String GameState)
makeAutoPlay p s = runExceptT $ do
  state <- makePlay p s
  eNextState <- runExceptT $ autoAdvance state
  pure case eNextState of
    Left e -> state
    Right nextState -> nextState

makePlay
  :: forall m
  . MonadError String m
  => Random m
  => Play
  -> GameState
  -> m GameState
makePlay = case _ of
  NewGame { playerCount } -> const $ setup (newGame playerCount)
  EndPhase { playerIndex } -> nextPhase playerIndex
  PlayCard x -> play x
  Purchase x -> purchase x
  ResolveChoice x -> resolveChoice x
  React x -> react x

getCurrentPlayer
  :: forall m
  . MonadError String m
  => GameState
  -> m Player
getCurrentPlayer state = getPlayer state.turn state

autoAdvance
  :: forall m
  . MonadError String m
  => Random m
  => GameState -> m GameState
autoAdvance gameState = do
    player <- getCurrentPlayer gameState
    case gameState.phase of
      ActionPhase ->
        if Player.hasActions player
        && Player.hasActionCardsInHand player
        then pure gameState
        else advancePhase >>= autoAdvance
      BuyPhase ->
        if player.buys > 0
        then pure gameState
        else advancePhase >>= autoAdvance
      CleanupPhase ->
        advancePhase >>= autoAdvance
    where
      advancePhase :: m GameState
      advancePhase = nextPhase gameState.turn gameState

nextPhase
  :: forall m
  . MonadError String m
  => Random m
  => Int
  -> GameState
  -> m GameState
nextPhase playerIndex state =
  assertTurn playerIndex state
    >>= assertChoicesResolved
    >>= modifyPlayerM playerIndex playerUpdate
    <$> nextPlayer >>> over _phase Phase.next
  where
    playerUpdate = case state.phase of
      CleanupPhase -> Player.cleanup
      _ -> pure
    nextPlayer s = s
      { turn =
        if s.phase == CleanupPhase
        then (s.turn + 1) `mod` (length s.players)
        else s.turn
      }

setup
  :: forall m
  . MonadError String m
  => Random m
  => GameState -> m GameState
setup gameState = flip (set _players) gameState
  <$> traverse (Player.drawCards 5) gameState.players

assertPhase
  :: forall m
  . MonadError String m
  => Phase
  -> GameState
  -> m GameState
assertPhase expected = assert
  (_.phase >>> (_ == expected))
  ("Expected: " <> show expected)

purchase
  :: forall m
  . MonadError String m
  => { playerIndex :: Int, stackIndex :: Int }
  -> GameState
  -> m GameState
purchase { playerIndex, stackIndex } =
  assertTurn playerIndex
    >=> assertPhase BuyPhase
    >=> modifyPlayerM playerIndex Player.assertHasBuys
    >=> modifyStackM stackIndex Stack.assertNotEmpty
    >=> assertPlayerCanAfford playerIndex stackIndex
    >=> purchase'
  where
    purchase' :: GameState -> m GameState
    purchase' state = do
      stack <- getStack stackIndex state
      modifyPlayer playerIndex (Player.purchase stack.card) state
        >>= modifyStack stackIndex Stack.take

assertPlayerCanAfford
  :: forall m
  . MonadError String m
  => Int
  -> Int
  -> GameState
  -> m GameState
assertPlayerCanAfford playerIndex stackIndex state = do
  stack <- getStack stackIndex state
  modifyPlayerM
    playerIndex
    (Player.assertHasCash stack.card.cost)
    state

overStackM
  :: forall m a
  . MonadError String m
  => (Stack -> m a)
  -> Int
  -> GameState
  -> m a
overStackM assertion stackIndex state =
  assertion =<< getStack stackIndex state

hasReaction :: Int -> GameState -> Boolean
hasReaction playerIndex state =
  case state ^? _player playerIndex <<< Player._reaction of
    Just _ -> true
    Nothing -> false

isAttacked :: Int -> GameState -> Boolean
isAttacked playerIndex state =
  case firstChoice playerIndex state of
    Just choice -> Choice.isAttack choice
    _ -> false

firstChoice :: Int -> GameState -> Maybe Choice
firstChoice playerIndex state =
  state ^? _player playerIndex >>= Player.firstChoice

reaction :: Int -> GameState -> Maybe Reaction
reaction playerIndex state = do
  hand <- state ^? _player playerIndex <<< Player._hand
  head $ catMaybes $ _.reaction <$> hand

react
  :: forall m
  . MonadError String m
  => { playerIndex :: Int, reaction :: Maybe Reaction }
  -> GameState
  -> m GameState
react { playerIndex, reaction } =
  modifyPlayer playerIndex Player.dropReaction >=>
  case reaction of
    Nothing ->
      pure
    Just BlockAttack ->
      traverseOf (_player playerIndex) Player.dropChoice

_pile pile playerIndex = case pile of
  Pile.Hand -> _player playerIndex <<< Player._hand
  Pile.Trash -> _trash
  Pile.Deck -> _player playerIndex <<< Player._deck
  Pile.Discard -> _player playerIndex <<< Player._discard

resolveChoice
  :: forall m
  . MonadError String m
  => Random m
  => { playerIndex :: Int, choice :: Choice }
  -> GameState
  -> m GameState
resolveChoice { playerIndex, choice } state =
  case choice of
    MoveFromHand
      { filter
      , n: constraint
      , destination
      , resolution: Just cardIndices
      } -> do
      let
        _source = _pile Pile.Hand playerIndex
        _source' = _player playerIndex <<< Player._hand
        _destination = _pile destination playerIndex
      source <- fromJust "failed to get source" $ state ^? _source
      player <- getPlayer playerIndex state
      selected <- takeIndices cardIndices source
      remaining <- dropIndices cardIndices source
      let
        forSelected = ("selected cards" <>! _) >>> (selected <@! _)
        forRemaining = ("remaining cards" <>! _) >>> (remaining <@! _)
        forSource = ("source cards" <>! _) >>> (source <@! _)
      case constraint of
        UpTo n -> check $
          forSelected $ lengthIs LTE n
        DownTo n -> check $
          forRemaining (lengthIs EQ n)
          ||
          ( forSource (lengthIs LT n)
          && forSelected (lengthIs EQ 0)
          )
        Exactly n -> check $
          forSelected (lengthIs EQ n)
          ||
          ( forSource (lengthIs LT n)
          && forSelected (lengthIs EQ $ length source)
          )
      case filter of
        Just f -> check $
          forSelected $ all (passFilter f) !> "illegal choice in"
        Nothing -> pure unit
      pure
        $ _source' .~ remaining
        $ _destination <>~ selected
        $ state

    GainCards { n, cardName, resolution: Just unit } -> do
      stack <- stackByName cardName state
      stackIndex <- indexOfStack stack.card state
      let cardsToGain = min n stack.count
      let newCount = max 0 (stack.count - n)
      let stackUpdate = Stack._count .~ newCount
      let cards = replicate cardsToGain stack.card
      let playerUpdate = Player._discard <>~ cards
      modifyPlayer playerIndex playerUpdate state
        >>= modifyStack stackIndex stackUpdate
    GainActions { n, resolution: Just unit } ->
      modifyPlayer playerIndex (Player.gainActions n) state
    GainBuys { n, resolution: Just unit } ->
      modifyPlayer playerIndex (Player.gainBuys n) state
    Discard { selection: SelectAll, resolution: Just unit } ->
      modifyPlayer playerIndex (moveAll Player._hand Player._toDiscard) state
    Draw { n, resolution: Just unit } ->
      modifyPlayerM playerIndex (Player.drawCards n) state
    GainBonus { bonus, resolution: Just unit } ->
      modifyPlayer playerIndex (Player.gainBonus bonus) state
    If { choice: choice', condition, resolution: Just unit } ->
      modifyPlayer playerIndex playerUpdate state
      where
        playerUpdate player =
          if condition `describes` player
          then Player.gainChoice choice' player
          else player
    And { choices, resolution: Just unit } ->
      modifyPlayer playerIndex (Player.gainChoices choices) state
    Or { resolution: Just chosen } ->
      modifyPlayer playerIndex (Player.gainChoice chosen) state
    PickN { n, resolution: Just choices } -> do
      check $ choices <@! lengthIs EQ n !<> "choices"
      modifyPlayer playerIndex (Player.gainChoices choices) state
    Option { choice, resolution: Just agree } ->
      let
        playerUpdate =
          if agree
          then Player.gainChoice choice
          else identity
      in
      modifyPlayer playerIndex playerUpdate state
    If { resolution: Nothing } -> unresolved
    And { resolution: Nothing } -> unresolved
    Or { resolution: Nothing } -> unresolved
    PickN { resolution: Nothing } -> unresolved
    Option { resolution: Nothing } -> unresolved
    MoveFromHand { resolution: Nothing } -> unresolved
    GainCards { resolution: Nothing } -> unresolved
    GainActions { resolution: Nothing } -> unresolved
    GainBuys { resolution: Nothing } -> unresolved
    Discard { resolution: Nothing } -> unresolved
    Draw { resolution: Nothing } -> unresolved
    GainBonus { resolution: Nothing } -> unresolved
  >>= traverseOf (_player playerIndex) Player.dropChoice
  where
    unresolved =
      throwError $ "this is an unresolved choice: " <> show choice

assertTurn
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m GameState
assertTurn playerIndex = assert
  ((playerIndex == _) <<< _.turn)
  "not your turn!"

assertChoicesResolved ::
  forall m
  . MonadError String m
  => GameState
  -> m GameState
assertChoicesResolved = assert
  (not <<< choicesOutstanding)
  "error: play: choices outstanding!"

play
  :: forall m
  . MonadError String m
  => Random m
  => { playerIndex :: Int, cardIndex :: Int }
  -> GameState
  -> m GameState
play { playerIndex, cardIndex } state = do
  player <- getPlayer playerIndex state
  card <- Player.getCard cardIndex player
  assertTurn playerIndex state
    >>= assertChoicesResolved
    >>= modifyPlayerM playerIndex Player.assertHasActions
    >>= modifyPlayerM playerIndex (Player.play cardIndex)
    >>= applySpecialsToTargets card
  where
    applySpecialsToTargets :: Card -> GameState -> m GameState
    applySpecialsToTargets card =
      flip (foldM $ applySpecialToTargets playerIndex) card.specials

    applySpecialToTargets
      :: Int
      -> GameState
      -> Special
      -> m GameState
    applySpecialToTargets attackerIndex state { target, command } =
      foldM (flip $ applySpecialToTarget command) state
      $ targetIndices target attackerIndex state

    applySpecialToTarget (Choose choice) targetIndex state =
      modifyPlayer targetIndex (Player.gainChoice choice) state

    targetIndices :: Target -> Int -> GameState -> Array Int
    targetIndices EveryoneElse attackerIndex =
      filter (_ /= attackerIndex) <<< indices <<< _.players
    targetIndices Everyone _ = indices <<< _.players
    targetIndices Self attackerIndex = const [ attackerIndex ]

choicesOutstanding :: GameState -> Boolean
choicesOutstanding = view _players >>> any Player.hasChoices

-- should we return a maybe here in case there are no players?
-- or should players be a non-empty list?
choiceTurn :: GameState -> Int
choiceTurn state =
  let players' = withIndices state.players in
  let prefix = takeWhile (fst >>> (_ /= state.turn)) players' in
  let suffix = dropWhile (fst >>> (_ /= state.turn)) players' in
  let rotated = suffix <> prefix in
  let withChoices = (snd >>> Player.hasChoices) `filter` rotated in
  fromMaybe 0 $ fst <$> (head withChoices)

newPlayer :: Player
newPlayer =
  { deck: []
  , hand: []
  , discard: (replicate 7 copper) <> (replicate 3 estate)
  , toDiscard: []
  , atPlay: []
  , buying: []
  , actions: 1
  , buys: 1
  , choices: []
  , reaction : Nothing
  , bonuses : []
  }

copper :: Card
copper = Card.treasure { name = "Copper", treasure = 1 }
silver :: Card
silver = Card.treasure { name = "Silver", cost = 3, treasure = 2 }
gold :: Card
gold = Card.treasure { name = "Gold", cost = 6, victoryPoints = 0, treasure = 3 }
platinum :: Card
platinum = Card.treasure { name = "Platinum", cost = 9, treasure = 5 }
estate :: Card
estate = Card.victory { name = "Estate", cost = 2, victoryPoints  = 1 }
duchy :: Card
duchy = Card.victory { name = "Duchy", cost = 5, victoryPoints = 3 }
province :: Card
province = Card.victory { name = "Province", cost = 8, victoryPoints = 6 }
colony :: Card
colony = Card.victory { name = "Colony", cost = 11, victoryPoints = 10 }
curse :: Card
curse = Card.card { types = [Curse], name = "Curse", victoryPoints = -1 }
greatHall :: Card
greatHall = Card.victory { types = [Action, Victory], name = "Great Hall", cost = 3, cards = 1, actions = 1, victoryPoints = 1 }
village :: Card
village = Card.action { name = "Village", cost = 3, cards = 1, actions = 2 }
woodCutter :: Card
woodCutter = Card.action { name = "Wood Cutter", cost = 3, buys = 1, treasure = 2 }
laboratory :: Card
laboratory = Card.action { name = "Laboratory", cost = 5, cards = 2, actions = 1 }
smithy :: Card
smithy = Card.action { name = "Smithy", cost = 4, cards = 3 }
festival :: Card
festival = Card.action { name = "Festival", cost = 5, actions = 2, buys = 1, treasure = 2 }
market :: Card
market = Card.action { name = "Market", cost = 5, actions = 1, cards = 1, buys = 1, treasure = 1 }
harem :: Card
harem = Card.treasure { types = [Treasure, Victory], name = "Harem", cost = 6, treasure = 2, victoryPoints = 2 }
bazaar :: Card
bazaar = Card.action { name = "Bazaar", cost = 5, cards = 1, actions = 2, treasure = 1 }
monument :: Card
monument = Card.action { types = [Action, Victory], name = "Monument", cost = 4, treasure = 2, victoryPoints = 1 }
workersVillage :: Card
workersVillage = Card.action { name = "Worker's Village", cost = 4, cards = 1, actions = 2, buys = 1 }
witch :: Card
witch = let attack = true in
  Card.actionAttack
  { name = "Witch"
  , cost = 5
  , cards = 2
  , specials =
    [ { target: EveryoneElse
      , command: Choose $ GainCards
        { cardName: "Curse"
        , n: 1
        , resolution: Nothing
        , attack
        }
      , description: "Each other player gains a Curse."
      }
    ]
  }
councilRoom :: Card
councilRoom = let attack = false in
  Card.action
  { name = "Council Room"
  , cost = 5
  , cards = 4
  , buys = 1
  , specials =
    [ { target: EveryoneElse
      , command: Choose $ Draw
        { n: 1
        , resolution: Nothing
        , attack
        }
      , description: "Each other player draws a card."
      }
    ]
  }
scholar :: Card
scholar = let attack = false in
  Card.action
  { name = "Scholar"
  , cost = 5
  , specials =
    [ { target: Self
      , command: Choose $ Discard
        { selection: SelectAll
        , resolution: Nothing
        , attack
        }
      , description: "Discard your hand."
      }
    , { target: Self
      , command: Choose $ Draw
        { n: 7
        , resolution: Nothing
        , attack
        }
      , description: "Draw 7 cards"
      }
    ]
  }
chapel :: Card
chapel = let attack = false in
  Card.action
  { name = "Chapel"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose
        $ MoveFromHand
        { n: UpTo 4
        , filter: Nothing
        , destination: Pile.Trash
        , resolution: Nothing
        , attack
        }
      , description: "Trash up to 4 cards from your hand"
      }
    ]
  }
militia :: Card
militia = let attack = true in
  Card.actionAttack
  { name = "Militia"
  , cost = 4
  , treasure = 2
  , specials =
    [ { target: EveryoneElse
      , command: Choose
        $ MoveFromHand
        { n: DownTo 3
        , filter: Nothing
        , destination: Pile.Discard
        , resolution: Nothing
        , attack
        }
      , description: "Discard down to 3 cards"
      }
    ]
  }
moat :: Card
moat =
  Card.actionReaction
  { name = "Moat"
  , cost = 2
  , cards = 2
  , reaction = Just BlockAttack
  }
nobles :: Card
nobles = let attack = false in
  Card.actionVictory
  { name = "Nobles"
  , cost = 6
  , victoryPoints = 2
  , specials =
    [ { target: Self
      , command: Choose $ Or
        { choices:
          [ Draw { n: 3, attack, resolution: Nothing }
          , GainActions { n: 2, attack, resolution: Nothing }
          ]
        , resolution: Nothing
        , attack
        }
      , description: "Choose one: +3 cards or +2 actions"
      }
    ]
  }
steward :: Card
steward = let attack = false in
  Card.action
  { name = "Steward"
  , cost = 3
  , specials =
    [ { target: Self
      , command: Choose $ Or
        { choices:
          [ Draw { n: 2, attack, resolution: Nothing }
          , GainBonus { bonus: Cash 2, attack, resolution: Nothing }
          , MoveFromHand
            { destination: Pile.Trash
            , filter: Nothing
            , n: Exactly 2
            , attack
            , resolution: Nothing
            }
          ]
        , resolution: Nothing
        , attack
        }
      , description:
        "Choose one: + 2 cards, + $2, or trash 2 cards from your hand"
      }
    ]
  }
pawn :: Card
pawn = let attack = false in
  Card.action
  { name = "Pawn"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose $ PickN
        { n: 2
        , choices:
          [ Draw { n: 1, attack, resolution: Nothing }
          , GainBonus { bonus: Cash 1, attack, resolution: Nothing }
          , GainActions { n: 1, attack, resolution: Nothing }
          , GainBuys { n: 1, attack, resolution: Nothing }
          ]
        , resolution: Nothing
        , attack
        }
      , description:
        "Choose two of: + $1, + 1 card, + 1 action, or +1 buy"
      }
    ]
  }
torturer :: Card
torturer = let attack = true in
  Card.actionAttack
  { name = "Torturer"
  , cost = 5
  , cards = 3
  , specials =
    [ { target: EveryoneElse
      , command: Choose $ PickN
        { n: 1
        , choices:
          [ MoveFromHand
            { n: Exactly 2
            , filter: Nothing
            , destination: Pile.Discard
            , attack
            , resolution: Nothing
            }
          , GainCards
            { n: 1
            , cardName: "Curse"
            , attack
            , resolution: Nothing
            }
          ]
        , resolution: Nothing
        , attack
        }
      , description:
        "Discard 2 cards or gain a curse"
      }
    ]
  }
consolation :: Card
consolation = let attack = false in
  Card.action
  { name = "Consolation"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose $ If
        { condition: HasCard "Estate"
        , choice: GainBonus
          { bonus: Cash 2
          , attack
          , resolution: Nothing
          }
        , resolution: Nothing
        , attack
        }
      , description:
        "If you have an Estate in your hand, + $2"
      }
    ]
  }
moneyLender :: Card
moneyLender = let attack = false in
  Card.action
  { name = "Money Lender"
  , cost = 4
  , specials =
    [ { target: Self
      , command: Choose $ If
        { condition: HasCard "Copper"
        , choice: Option
          { choice: And
            { choices:
              [ MoveFromHand
                { n: Exactly 1
                , filter: Just (HasName "Copper")
                , destination: Pile.Trash
                , attack
                , resolution: Nothing
                }
              , GainBonus
                { bonus: Cash 3
                , attack
                , resolution: Nothing
                }
              ]
            , attack
            , resolution: Nothing
            }
          , attack
          , resolution: Nothing
          }
        , attack
        , resolution: Nothing
        }
      , description:
        "You may trash a copper from your hand for + $3"
      }
    ]
  }

describes :: Condition -> Player -> Boolean
describes (HasCard name) = _.hand >>> any (_.name >>> (_ == name))

passFilter :: Filter -> Card -> Boolean
passFilter (HasName name) = _.name >>> (_ == name)

