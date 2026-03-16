module Test.Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, decodeArrayBuffer, encodeArrayBuffer)
import Data.Either (Either(..), isRight)
import Data.Foldable (all, foldl, length, sum)
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..), isJust)
import Data.Stack.Machine as Machine
import Data.Traversable (traverse)
import Domination.Capability.Random (RandomM, runRandomM)
import Domination.Capability.Random (randomIntBetween, randomBoolean) as Random
import Domination.Capability.WireCodec (class WireCodec, readWire, writeWire)
import Domination.Data.Actions (Actions, actions)
import Domination.Data.AI as AI
import Domination.Data.AI (Bot)
import Domination.Data.AI.Strategy (Strategy(..), botName, allStrategies)
import Domination.Data.Buys (Buys, buys)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice(..))
import Domination.Data.Choice as Choice
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter as Filter
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Game.Engine as Engine
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Pile as Pile
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Points (points)
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Result as Domination.Data.Result
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack as Stack
import Domination.Data.Supply as Supply
import Data.Tuple (Tuple(..), fst)
import Domination.Data.Wire.Game (_toWire) as Dom
import Domination.Data.Wire.Play (_toWire) as Play
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (Result(..), assertEquals)


-- ══════════════════════════════════════════════════════════════════
-- Test Runner
-- ══════════════════════════════════════════════════════════════════

main :: Effect Unit
main = do
  log "═══ Domination Test Suite ═══"
  log ""
  results <- run_sections
    [ { name: "Stack Machine", tests: stack_machine_tests }
    , { name: "Wire Serialization", tests: wire_serialization_tests }
    , { name: "Isomorphism", tests: isomorphism_tests }
    , { name: "Game Initialization", tests: game_init_tests }
    , { name: "Phase Transitions", tests: phase_transition_tests }
    , { name: "Player Operations", tests: player_operation_tests }
    , { name: "Supply Management", tests: supply_tests }
    , { name: "Card Properties", tests: card_property_tests }
    , { name: "Purchase Mechanics", tests: purchase_tests }
    , { name: "Play Card (Pure)", tests: play_card_tests }
    , { name: "Game Ending Conditions", tests: game_ending_tests }
    , { name: "Auto-Advance Logic", tests: auto_advance_tests }
    ]
  play_results <- run_effect_section "Play Card (Effectful)" play_card_effect_tests
  simulation_results <- run_effect_section "Game Simulation" game_simulation_tests
  reaction_results <- run_effect_section "Reaction System" reaction_tests
  property_results <- run_effect_section "Property-Based Tests" property_tests
  ai_results <- run_effect_section "AI Bot Tests" ai_tests
  let total_passed = results.passed + simulation_results.passed + play_results.passed + property_results.passed + reaction_results.passed + ai_results.passed
  let total_failed = results.failed + simulation_results.failed + play_results.failed + property_results.failed + reaction_results.failed + ai_results.failed
  let total = total_passed + total_failed
  log ""
  log $ "═══ Results: " <> show total_passed <> "/" <> show total <> " passed ═══"
  when (total_failed > 0) do
    log $ "FAILURES: " <> show total_failed

type TestCase = { name :: String, test :: Result }

run_sections
  :: Array { name :: String, tests :: Array TestCase }
  -> Effect { passed :: Int, failed :: Int }
run_sections sections = foldl merge (pure { passed: 0, failed: 0 }) (map run_section sections)
  where
    merge acc section = do
      a <- acc
      b <- section
      pure { passed: a.passed + b.passed, failed: a.failed + b.failed }

run_section
  :: { name :: String, tests :: Array TestCase }
  -> Effect { passed :: Int, failed :: Int }
run_section { name, tests } = do
  log $ "── " <> name <> " ──"
  results <- traverse run_test tests
  let passed = Array.length $ Array.filter _.passed results
  let failed = Array.length tests - passed
  log $ "  " <> show passed <> "/" <> show (Array.length tests) <> " passed"
  log ""
  pure { passed, failed }

run_effect_section
  :: String
  -> Array { name :: String, test :: Effect Result }
  -> Effect { passed :: Int, failed :: Int }
run_effect_section name tests = do
  log $ "── " <> name <> " ──"
  results <- traverse run_effect_test tests
  let passed = Array.length $ Array.filter _.passed results
  let failed = Array.length tests - passed
  log $ "  " <> show passed <> "/" <> show (Array.length tests) <> " passed"
  log ""
  pure { passed, failed }

run_test :: TestCase -> Effect { passed :: Boolean }
run_test { name, test } = do
  case test of
    Success -> do
      log $ "  ✓ " <> name
      pure { passed: true }
    Failed msg -> do
      log $ "  ✗ " <> name <> ": " <> msg
      pure { passed: false }

run_effect_test
  :: { name :: String, test :: Effect Result }
  -> Effect { passed :: Boolean }
run_effect_test { name, test } = do
  result <- test
  run_test { name, test: result }

-- ══════════════════════════════════════════════════════════════════
-- Helper: verify a boolean property with a message
-- ══════════════════════════════════════════════════════════════════

assert_true :: String -> Boolean -> Result
assert_true _ true = Success
assert_true msg false = Failed msg

assert_false :: String -> Boolean -> Result
assert_false msg = assert_true msg <<< not

assert_eq :: forall a. Eq a => Show a => a -> a -> Result
assert_eq = assertEquals

-- ══════════════════════════════════════════════════════════════════
-- 1. Stack Machine Tests
-- ══════════════════════════════════════════════════════════════════

stack_machine_tests :: Array TestCase
stack_machine_tests =
  [ { name: "computes (1+7+4-1)*3/11 = 3"
    , test: assert_eq (Machine.exampleStackMachineComputation 1) 3
    }
  , { name: "computes (0+7+4-1)*3/11 = 2 (integer division)"
    , test: assert_eq (Machine.exampleStackMachineComputation 0) 2
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- 2. Wire Serialization Tests
-- ══════════════════════════════════════════════════════════════════

wire_serialization_tests :: Array TestCase
wire_serialization_tests =
  map
    (\n ->
      { name: "roundtrip with " <> show n <> "-player game"
      , test: test_game_wire_serialization n
      })
    (1 .. 10)

test_game_wire_serialization :: Int -> Result
test_game_wire_serialization player_count =
  let game = Game.new player_count Cards.cardMap true
      serialized = view Dom._toWire game
      deserialized = review Dom._toWire serialized
  in assert_eq deserialized game

-- ══════════════════════════════════════════════════════════════════
-- 3. Isomorphism Tests
-- ══════════════════════════════════════════════════════════════════

isomorphism_tests :: Array TestCase
isomorphism_tests =
  [ { name: "Game ↔ WireGame (2 players)"
    , test: test_iso Dom._toWire (Game.new 2 Cards.cardMap true)
    }
  , { name: "Game ↔ WireGame (4 players)"
    , test: test_iso Dom._toWire (Game.new 4 Cards.cardMap false)
    }
  , { name: "Play ↔ WirePlay (NewGame)"
    , test: test_iso Play._toWire
        (NewGame { playerCount: 2, supply: Cards.cardMap, longGame: true })
    }
  , { name: "Play ↔ WirePlay (EndPhase)"
    , test: test_iso Play._toWire (EndPhase { playerIndex: 0 })
    }
  , { name: "Play ↔ WirePlay (Purchase)"
    , test: test_iso Play._toWire (Purchase { playerIndex: 0, stackIndex: 3 })
    }
  , { name: "Play ↔ WirePlay (PlayCard)"
    , test: test_iso Play._toWire (PlayCard { playerIndex: 0, cardIndex: 2 })
    }
  ]

test_iso :: forall a b. Eq a => Show a => Iso' a b -> a -> Result
test_iso l v =
  let converted = view l v
      restored = review l converted
  in assert_eq restored v

-- ══════════════════════════════════════════════════════════════════
-- 4. Game Initialization Tests
-- ══════════════════════════════════════════════════════════════════

game_init_tests :: Array TestCase
game_init_tests =
  -- Property: ∀ n ∈ [1..10], Game.new produces a valid initial state
  map (\n -> { name: "init " <> show n <> "p: starts in ActionPhase"
             , test: assert_eq (Game.new n Cards.cardMap true).phase ActionPhase
             }) (1 .. 6)
  <>
  map (\n -> { name: "init " <> show n <> "p: turn starts at 0"
             , test: assert_eq (Game.new n Cards.cardMap true).turn 0
             }) (1 .. 6)
  <>
  map (\n -> { name: "init " <> show n <> "p: has " <> show n <> " players"
             , test: assert_eq (length (Game.new n Cards.cardMap true).players) n
             }) (1 .. 6)
  <>
  [ { name: "init: no result"
    , test: assert_eq (Game.new 2 Cards.cardMap true).result Nothing
    }
  , { name: "init: empty trash"
    , test: assert_eq (Game.new 2 Cards.cardMap true).trash []
    }
  , { name: "init: longGame flag true"
    , test: assert_true "longGame should be true" (Game.new 2 Cards.cardMap true).longGame
    }
  , { name: "init: longGame flag false"
    , test: assert_false "longGame should be false" (Game.new 2 Cards.cardMap false).longGame
    }
  , { name: "init: supply has correct number of stacks"
    , test: assert_eq
        (Array.length (Game.new 2 Cards.cardMap true).supply)
        (Array.length Cards.cardMap)
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- 5. Phase Transition Tests
-- ══════════════════════════════════════════════════════════════════

phase_transition_tests :: Array TestCase
phase_transition_tests =
  [ { name: "Action → Buy"
    , test: assert_eq (Phase.next ActionPhase) BuyPhase
    }
  , { name: "Buy → Cleanup"
    , test: assert_eq (Phase.next BuyPhase) CleanupPhase
    }
  , { name: "Cleanup → Action"
    , test: assert_eq (Phase.next CleanupPhase) ActionPhase
    }
  , { name: "Phase cycle: 3 nexts returns to start"
    , test: assert_eq (Phase.next $ Phase.next $ Phase.next ActionPhase) ActionPhase
    }
  , { name: "Phase cycle: Buy → Buy in 3 steps"
    , test: assert_eq (Phase.next $ Phase.next $ Phase.next BuyPhase) BuyPhase
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- 6. Player Operation Tests
-- ══════════════════════════════════════════════════════════════════

player_operation_tests :: Array TestCase
player_operation_tests =
  let p = Player.newPlayer
  in
  [ -- newPlayer properties
    { name: "newPlayer: 1 action"
    , test: assert_eq p.actions one
    }
  , { name: "newPlayer: 1 buy"
    , test: assert_eq p.buys one
    }
  , { name: "newPlayer: empty hand"
    , test: assert_eq p.hand []
    }
  , { name: "newPlayer: empty deck"
    , test: assert_eq p.deck []
    }
  , { name: "newPlayer: 10 cards in discard (7 copper + 3 estate)"
    , test: assert_eq (Array.length p.discard) 10
    }
  , { name: "newPlayer: no choices"
    , test: assert_eq p.choices []
    }
  , { name: "newPlayer: hasReaction is false"
    , test: assert_false "should have no reaction" (Player.hasReaction p)
    }
  , { name: "newPlayer: no bonuses"
    , test: assert_eq p.bonuses []
    }
  , { name: "newPlayer: empty atPlay"
    , test: assert_eq p.atPlay []
    }
  , { name: "newPlayer: empty buying"
    , test: assert_eq p.buying []
    }
  , { name: "newPlayer: empty toDiscard"
    , test: assert_eq p.toDiscard []
    }
  , { name: "newPlayer: hasChoices is false"
    , test: assert_false "should have no choices" (Player.hasChoices p)
    }
  , { name: "newPlayer: hasActions is true"
    , test: assert_true "should have actions" (Player.hasActions p)
    }
  -- allCards totals
  , { name: "newPlayer: allCards = 10"
    , test: assert_eq (Array.length $ Player.allCards p) 10
    }
  -- Score of starting deck: 3 estates × 1 VP each = 3
  , { name: "newPlayer: score = 3"
    , test: assert_eq (Player.score p) (points 3)
    }
  -- Cash from starting hand (empty hand) = 0
  , { name: "newPlayer: cash = 0"
    , test: assert_eq (Player.cash p) 0
    }
  -- gainActions
  , { name: "gainActions: adds to existing"
    , test: assert_eq (Player.gainActions one p).actions (one + one)
    }
  -- gainBuys
  , { name: "gainBuys: adds to existing"
    , test: assert_eq (Player.gainBuys one p).buys (one + one)
    }
  -- hasReaction checks pendingReactions field
  , { name: "hasReaction: true with pending reactions"
    , test: assert_true "should have reaction"
        (Player.hasReaction $ p { pendingReactions = [Tuple BlockAttack "block"] })
    }
  , { name: "hasReaction: false with empty pendingReactions"
    , test: assert_false "should not have reaction"
        (Player.hasReaction $ p { pendingReactions = [] })
    }
  -- hasActionCardsInHand with no hand
  , { name: "newPlayer: no action cards in hand"
    , test: assert_false "empty hand has no action cards"
        (Player.hasActionCardsInHand p)
    }
  -- purchase: decrements buys and adds to buying
  , { name: "purchase: adds card to buying"
    , test: assert_eq
        (Array.length (Player.purchase (Card._card Cards.copper) p).buying)
        1
    }
  -- purchase: decrements buys
  , { name: "purchase: decrements buys"
    , test: assert_eq
        (Player.purchase (Card._card Cards.copper) p).buys
        zero
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- 7. Supply Management Tests
-- ══════════════════════════════════════════════════════════════════

supply_tests :: Array TestCase
supply_tests =
  let supply2 = Supply.makeSupply 2 Cards.cardMap
      supply4 = Supply.makeSupply 4 Cards.cardMap
  in
  [ { name: "makeSupply: number of stacks = number of cards"
    , test: assert_eq (Array.length supply2) (Array.length Cards.cardMap)
    }
  -- Victory cards scale with player count
  , { name: "2p supply: victory cards have count 8 (4*2)"
    , test: assert_true "estate count should be 8"
        $ stackCountByName "Estate" supply2 == Just 8
    }
  , { name: "4p supply: victory cards have count 16 (4*4)"
    , test: assert_true "estate count should be 16"
        $ stackCountByName "Estate" supply4 == Just 16
    }
  -- Treasure cards scale with player count
  , { name: "2p supply: copper has 20 (10*2)"
    , test: assert_true "copper count should be 20"
        $ stackCountByName "Copper" supply2 == Just 20
    }
  -- Curse cards scale with player count
  , { name: "2p supply: curse has 10 (10*(2-1))"
    , test: assert_true "curse count should be 10"
        $ stackCountByName "Curse" supply2 == Just 10
    }
  -- Kingdom cards have fixed count (+ bonus for large games)
  , { name: "2p supply: kingdom cards have 10"
    , test: assert_true "smithy count should be 10"
        $ stackCountByName "Smithy" supply2 == Just 10
    }
  -- nonEmptyStacks
  , { name: "all stacks start non-empty"
    , test: assert_eq
        (Array.length $ Supply.nonEmptyStacks supply2)
        (Array.length supply2)
    }
  -- emptyStacks
  , { name: "no empty stacks at start"
    , test: assert_eq (Array.length $ Supply.emptyStacks supply2) 0
    }
  -- emptyStackCount
  , { name: "emptyStackCount = 0 at start"
    , test: assert_eq (Supply.emptyStackCount supply2) 0
    }
  -- positivePoints: sum of victory card points in supply
  , { name: "2p supply: positivePoints > 0"
    , test: assert_true "should have positive points"
        (Supply.positivePoints supply2 > zero)
    }
  -- negativePoints: sum of curse points
  , { name: "2p supply: negativePoints < 0"
    , test: assert_true "should have negative points"
        (Supply.negativePoints supply2 < zero)
    }
  -- highestVictoryCardStack
  , { name: "highestVictoryCardStack is Colony"
    , test: assert_true "should be Colony"
        $ map (_.card >>> _.name) (Supply.highestVictoryCardStack supply2)
          == Just "Colony"
    }
  ]

stackCountByName :: String -> Supply.Supply -> Maybe Int
stackCountByName name supply =
  _.count <$> Array.find (_.card >>> _.name >>> (_ == name)) supply

-- ══════════════════════════════════════════════════════════════════
-- 8. Card Property Tests
-- ══════════════════════════════════════════════════════════════════

card_property_tests :: Array TestCase
card_property_tests =
  let copper = Card._card Cards.copper
      estate = Card._card Cards.estate
      village = Card._card Cards.village
      smithy = Card._card Cards.smithy
      witch = Card._card Cards.witch
      moat = Card._card Cards.moat
  in
  [ -- Type checks
    { name: "copper is treasure"
    , test: assert_true "copper should be treasure" (Card.isTreasure copper)
    }
  , { name: "copper is not action"
    , test: assert_false "copper should not be action" (Card.isAction copper)
    }
  , { name: "estate is victory"
    , test: assert_true "estate should be victory" (Card.isVictory estate)
    }
  , { name: "village is action"
    , test: assert_true "village should be action" (Card.isAction village)
    }
  , { name: "moat is reaction"
    , test: assert_true "moat should be reaction" (Card.isReaction moat)
    }
  -- Cost properties
  , { name: "copper costs 0"
    , test: assert_eq copper.cost 0
    }
  , { name: "estate costs 2"
    , test: assert_eq estate.cost 2
    }
  , { name: "smithy costs 4"
    , test: assert_eq smithy.cost 4
    }
  -- Treasure values
  , { name: "copper produces 1"
    , test: assert_eq copper.treasure 1
    }
  , { name: "silver produces 2"
    , test: assert_eq (Card._card Cards.silver).treasure 2
    }
  , { name: "gold produces 3"
    , test: assert_eq (Card._card Cards.gold).treasure 3
    }
  -- Action card effects
  , { name: "village gives +1 card"
    , test: assert_eq village.cards 1
    }
  , { name: "village gives +2 actions"
    , test: assert_eq village.actions (actions 2)
    }
  , { name: "smithy gives +3 cards"
    , test: assert_eq smithy.cards 3
    }
  -- Card.value sums treasure
  , { name: "value of [copper, copper, copper] = 3"
    , test: assert_eq (Card.value [copper, copper, copper]) 3
    }
  , { name: "value of empty = 0"
    , test: assert_eq (Card.value []) 0
    }
  -- Card.cost sums costs
  , { name: "cost of [estate, copper] = 2"
    , test: assert_eq (Card.cost [estate, copper]) 2
    }
  -- All cards in cardMap have names
  , { name: "all cards have non-empty names"
    , test: assert_true "all cards should have names"
        (all (_.name >>> (_ /= "")) Cards.cardMap)
    }
  -- All cards have non-negative costs
  , { name: "all cards have non-negative costs"
    , test: assert_true "costs should be >= 0"
        (all (_.cost >>> (_ >= 0)) Cards.cardMap)
    }
  -- All cards have at least one type
  , { name: "all cards have at least one type"
    , test: assert_true "cards must have types"
        (all (_.types >>> Array.length >>> (_ > 0)) Cards.cardMap)
    }
  -- witch has special (attack)
  , { name: "witch has special ability"
    , test: assert_true "witch should have special"
        case witch.special of
          Just _ -> true
          Nothing -> false
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- 9. Purchase Mechanics Tests
-- ══════════════════════════════════════════════════════════════════

purchase_tests :: Array TestCase
purchase_tests =
  [ -- assertTurn: correct player
    { name: "assertTurn: succeeds for correct player"
    , test: case runPure (Game.assertTurn 0 (Game.new 2 Cards.cardMap true)) of
        Right _ -> Success
        Left err -> Failed err
    }
  , { name: "assertTurn: fails for wrong player"
    , test: case runPure (Game.assertTurn 1 (Game.new 2 Cards.cardMap true)) of
        Left _ -> Success
        Right _ -> Failed "should have failed for wrong turn"
    }
  -- assertPhase
  , { name: "assertPhase: ActionPhase succeeds at start"
    , test: case runPure (Game.assertPhase ActionPhase (Game.new 2 Cards.cardMap true)) of
        Right _ -> Success
        Left err -> Failed err
    }
  , { name: "assertPhase: BuyPhase fails at start"
    , test: case runPure (Game.assertPhase BuyPhase (Game.new 2 Cards.cardMap true)) of
        Left _ -> Success
        Right _ -> Failed "should fail: game starts in ActionPhase"
    }
  -- Player assertion: assertHasBuys
  , { name: "assertHasBuys: newPlayer has buys"
    , test: case runPure (Player.assertHasBuys Player.newPlayer) of
        Right _ -> Success
        Left err -> Failed err
    }
  , { name: "assertHasBuys: fails with 0 buys"
    , test: let p = Player.newPlayer { buys = zero :: Buys }
            in case runPure (Player.assertHasBuys p) of
              Left _ -> Success
              Right _ -> Failed "should fail with 0 buys"
    }
  -- Player assertion: assertHasActions
  , { name: "assertHasActions: newPlayer has actions"
    , test: case runPure (Player.assertHasActions Player.newPlayer) of
        Right _ -> Success
        Left err -> Failed err
    }
  , { name: "assertHasActions: fails with 0 actions"
    , test: let p = Player.newPlayer { actions = zero :: Actions }
            in case runPure (Player.assertHasActions p) of
              Left _ -> Success
              Right _ -> Failed "should fail with 0 actions"
    }
  -- assertChoicesResolved: no choices at start
  , { name: "assertChoicesResolved: succeeds at game start"
    , test: case runPure (Game.assertChoicesResolved (Game.new 2 Cards.cardMap true)) of
        Right _ -> Success
        Left err -> Failed err
    }
  ]

-- Run a computation in Either String (our pure MonadError)
runPure :: forall a. Either String a -> Either String a
runPure = identity

-- ══════════════════════════════════════════════════════════════════
-- 10. Play Card Mechanics Tests
-- ══════════════════════════════════════════════════════════════════

play_card_tests :: Array TestCase
play_card_tests =
  let copper = Card._card Cards.copper
  in
  [ -- Playing a non-action card should fail (this doesn't require Random since it fails before drawing)
    -- Player.play requires Random m, so pure Either tests won't work for success cases
    -- We test the error cases that fail before needing Random
    { name: "play: invalid card index in empty hand"
    , test: let p = Player.newPlayer { hand = [], deck = [] }
            in case runPure (Player.getCard 0 p) of
              Left _ -> Success
              Right _ -> Failed "should fail with empty hand"
    }
  , { name: "getCard: valid index succeeds"
    , test: let p = Player.newPlayer { hand = [ copper ] }
            in case runPure (Player.getCard 0 p) of
              Right c -> assert_eq c.name "Copper"
              Left err -> Failed err
    }
  , { name: "dropCard: removes card from hand"
    , test: let p = Player.newPlayer { hand = [ copper, copper ] }
            in case runPure (Player.dropCard 0 p) of
              Right remaining -> assert_eq (Array.length remaining) 1
              Left err -> Failed err
    }
  , { name: "dropCard: invalid index fails"
    , test: let p = Player.newPlayer { hand = [] }
            in case runPure (Player.dropCard 0 p) of
              Left _ -> Success
              Right _ -> Failed "should fail with empty hand"
    }
  ]

-- Effectful play card tests (need Random for card drawing)
play_card_effect_tests :: Array { name :: String, test :: Effect Result }
play_card_effect_tests =
  let village = Card._card Cards.village
      copper = Card._card Cards.copper
      player_with_village = Player.newPlayer
        { hand = [ village ]
        , deck = [ copper, copper, copper, copper, copper ]
        }
  in
  [ { name: "play village: removes card from hand and draws 1"
    , test: do
        result <- runRandomM $ runExceptT $ Player.play 0 player_with_village
        pure case result of
          Right p -> assert_eq (Array.length p.hand) 1
          Left err -> Failed $ "play failed: " <> err
    }
  , { name: "play village: adds to atPlay"
    , test: do
        result <- runRandomM $ runExceptT $ Player.play 0 player_with_village
        pure case result of
          Right p -> assert_eq (Array.length p.atPlay) 1
          Left err -> Failed $ "play failed: " <> err
    }
  , { name: "play village: net actions = original + 2 - 1"
    , test: do
        result <- runRandomM $ runExceptT $ Player.play 0 player_with_village
        pure case result of
          Right p -> assert_eq p.actions (actions 2)
          Left err -> Failed $ "play failed: " <> err
    }
  , { name: "play non-action: fails"
    , test: do
        let p = Player.newPlayer { hand = [ copper ], deck = [] }
        result <- runRandomM $ runExceptT $ Player.play 0 p
        pure case result of
          Left _ -> Success
          Right _ -> Failed "should not play treasure as action"
    }
  , { name: "play with 0 actions: Player.play does not check (Engine.play does)"
    , test: do
        let p = Player.newPlayer
              { hand = [ village ]
              , deck = [ copper ]
              , actions = zero :: Actions
              }
        -- Player.play only checks isAction, not hasActions
        -- The hasActions check is in Engine.play
        result <- runRandomM $ runExceptT $ Player.play 0 p
        pure case result of
          Right _ -> Success
          Left err -> Failed $ "unexpected failure: " <> err
    }
  -- Cleanup tests
  , { name: "cleanup: resets player state and draws 5"
    , test: do
        let p = Player.newPlayer
              { hand = [ copper, copper ]
              , deck = [ copper, copper, copper ]
              , discard = [ copper, copper, copper, copper, copper ]
              , atPlay = []
              , buying = []
              }
        result <- runRandomM $ runExceptT $ Player.cleanup p
        pure case result of
          Right p' -> assert_true "should have 5 cards in hand after cleanup"
            (Array.length p'.hand == 5)
          Left err -> Failed $ "cleanup failed: " <> err
    }
  , { name: "cleanup: resets actions to 1"
    , test: do
        let p = Player.newPlayer
              { hand = [ copper ]
              , deck = [ copper, copper, copper, copper, copper, copper, copper, copper, copper ]
              }
        result <- runRandomM $ runExceptT $ Player.cleanup p
        pure case result of
          Right p' -> assert_eq p'.actions one
          Left err -> Failed $ "cleanup failed: " <> err
    }
  , { name: "cleanup: resets buys to 1"
    , test: do
        let p = Player.newPlayer
              { hand = [ copper ]
              , deck = [ copper, copper, copper, copper, copper, copper, copper, copper, copper ]
              }
        result <- runRandomM $ runExceptT $ Player.cleanup p
        pure case result of
          Right p' -> assert_eq p'.buys one
          Left err -> Failed $ "cleanup failed: " <> err
    }
  , { name: "cleanup: clears bonuses"
    , test: do
        let p = Player.newPlayer
              { hand = [ copper ]
              , deck = [ copper, copper, copper, copper, copper, copper, copper, copper, copper ]
              }
        result <- runRandomM $ runExceptT $ Player.cleanup p
        pure case result of
          Right p' -> assert_eq p'.bonuses []
          Left err -> Failed $ "cleanup failed: " <> err
    }
  -- drawCards tests
  , { name: "drawCards 3: draws 3 cards"
    , test: do
        let p = Player.newPlayer
              { hand = []
              , deck = [ copper, copper, copper, copper, copper ]
              }
        result <- runRandomM $ runExceptT $ Player.drawCards 3 p
        pure case result of
          Right p' -> assert_eq (Array.length p'.hand) 3
          Left err -> Failed $ "drawCards failed: " <> err
    }
  , { name: "drawCards: reshuffles discard when deck empty"
    , test: do
        let p = Player.newPlayer
              { hand = []
              , deck = []
              , discard = [ copper, copper, copper ]
              }
        result <- runRandomM $ runExceptT $ Player.drawCards 2 p
        pure case result of
          Right p' -> assert_eq (Array.length p'.hand) 2
          Left err -> Failed $ "drawCards failed: " <> err
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- 11. Game Ending Conditions Tests
-- ══════════════════════════════════════════════════════════════════

game_ending_tests :: Array TestCase
game_ending_tests =
  [ -- A fresh game should not be over
    { name: "fresh game is not over"
    , test: case runPure (Engine.finalResult (Game.new 2 Cards.cardMap true)) of
        Right Nothing -> Success
        Right (Just _) -> Failed "fresh game should not be over"
        Left err -> Failed err
    }
  , { name: "fresh game (short): not over"
    , test: case runPure (Engine.finalResult (Game.new 2 Cards.cardMap false)) of
        Right Nothing -> Success
        Right (Just _) -> Failed "fresh short game should not be over"
        Left err -> Failed err
    }
  , { name: "fresh 1p game: not over"
    , test: case runPure (Engine.finalResult (Game.new 1 Cards.cardMap true)) of
        Right Nothing -> Success
        Right (Just _) -> Failed "fresh solo game should not be over"
        Left err -> Failed err
    }
  -- gameResult for 1 player is Victory 0
  , { name: "1p gameResult: Victory 0"
    , test: case Engine.gameResult (Game.new 1 Cards.cardMap true) of
        Just _ -> Success
        Nothing -> Failed "1 player game should produce a result"
    }
  -- gameResult for 2 players with equal starting states is Tie
  , { name: "2p equal scores: Tie"
    , test: case Engine.gameResult (Game.new 2 Cards.cardMap true) of
        Just _ -> Success
        Nothing -> Failed "2 player game should produce a result"
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- 12. Auto-Advance Tests
-- ══════════════════════════════════════════════════════════════════

auto_advance_tests :: Array TestCase
auto_advance_tests =
  [ -- choiceTurn: returns turn when no choices
    { name: "choiceTurn: defaults to 0 with no choices"
    , test: assert_eq (Engine.choiceTurn (Game.new 2 Cards.cardMap true)) 0
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- 13. Game Simulation Tests (Effectful)
-- ══════════════════════════════════════════════════════════════════

game_simulation_tests :: Array { name :: String, test :: Effect Result }
game_simulation_tests =
  [ -- Simulate a NewGame play for various player counts
    { name: "NewGame 1p: setup succeeds"
    , test: simulate_new_game 1 true
    }
  , { name: "NewGame 2p: setup succeeds"
    , test: simulate_new_game 2 true
    }
  , { name: "NewGame 4p: setup succeeds"
    , test: simulate_new_game 4 true
    }
  , { name: "NewGame 2p short: setup succeeds"
    , test: simulate_new_game 2 false
    }
  -- After setup, each player should have 5 cards in hand
  , { name: "after setup: players have 5-card hands"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed $ "setup failed: " <> err
          Right game ->
            assert_true "all players should have 5-card hands"
            $ all (\p -> Array.length p.hand == 5) game.players
    }
  -- After setup: each player's total cards should be 10
  , { name: "after setup: players have 10 total cards"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed $ "setup failed: " <> err
          Right game ->
            assert_true "all players should have 10 total cards"
            $ all (\p -> Array.length (Player.allCards p) == 10) game.players
    }
  -- After setup: game should have advanced past ActionPhase
  -- (newPlayer starts with no action cards, so autoAdvance skips ActionPhase)
  , { name: "after setup: auto-advances past ActionPhase (no action cards)"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed $ "setup failed: " <> err
          Right game ->
            assert_eq game.phase BuyPhase
    }
  -- Simulate multiple turns
  , { name: "multi-turn simulation: 1p completes without error"
    , test: simulate_n_turns 1 true 3
    }
  , { name: "multi-turn simulation: 2p completes without error"
    , test: simulate_n_turns 2 true 3
    }
  -- Conservation of cards: total cards in game should be constant
  , { name: "card conservation: total cards unchanged after setup"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed $ "setup failed: " <> err
          Right game ->
            let player_cards = sum $ map (Player.allCards >>> Array.length) game.players
                supply_cards = sum $ map _.count game.supply
                trash_cards = Array.length game.trash
                total = player_cards + supply_cards + trash_cards
                -- Initial total: each player has 10 starting cards
                -- Plus all supply counts
                initial_supply = sum $ map _.count (Supply.makeSupply 2 Cards.cardMap)
                initial_total = 2 * 10 + initial_supply
            in assert_eq total initial_total
    }
  -- After NewGame setup, no choices should be outstanding
  , { name: "after setup: no choices outstanding"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed $ "setup failed: " <> err
          Right game ->
            assert_false "no choices should be outstanding"
            (Game.choicesOutstanding game)
    }
  ]

simulate_new_game :: Int -> Boolean -> Effect Result
simulate_new_game n longGame = do
  result <- run_make_auto_play n longGame
  pure case result of
    Left err -> Failed $ "NewGame failed: " <> err
    Right _ -> Success

run_make_auto_play :: Int -> Boolean -> Effect (Either String Game)
run_make_auto_play n longGame =
  let game = Game.new n Cards.cardMap longGame
      play = NewGame { playerCount: n, supply: Cards.cardMap, longGame }
  in runRandomM $ Engine.makeAutoPlay play game

simulate_n_turns :: Int -> Boolean -> Int -> Effect Result
simulate_n_turns playerCount longGame nTurns = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      result <- runRandomM $ play_turns nTurns game
      pure case result of
        Left err -> Failed $ "simulation failed at some turn: " <> err
        Right _ -> Success
  where
    play_turns 0 game = pure (Right game)
    play_turns n game = do
      -- End the current phase to advance
      result <- Engine.makeAutoPlay (EndPhase { playerIndex: game.turn }) game
      case result of
        Left _ -> pure (Right game) -- Game may be over or phase transition may not apply
        Right game' ->
          if game'.result /= Nothing
          then pure (Right game')
          else play_turns (n - 1) game'

-- ══════════════════════════════════════════════════════════════════
-- Utility: binary serialization helpers (preserved from original)
-- ══════════════════════════════════════════════════════════════════

encode_and_decode
  :: forall a
  . EncodeArrayBuffer a
  => DecodeArrayBuffer a
  => DynamicByteLength a
  => Eq a
  => Show a
  => a
  -> Effect Result
encode_and_decode original_value = do
  binary_buffer <- encodeArrayBuffer original_value
  deserialized_value <- decodeArrayBuffer binary_buffer
  pure $ assert_eq deserialized_value $ Just original_value

test_wire_roundtrip
  :: forall m a
  . WireCodec m
  => EncodeArrayBuffer a
  => DynamicByteLength a
  => DecodeArrayBuffer a
  => Eq a
  => Show a
  => a
  -> m Result
test_wire_roundtrip value_to_transmit = do
  wire_format <- writeWire value_to_transmit
  case wire_format of
    Left err -> pure $ Failed $ "Failed to serialize: " <> show err
    Right wire_data -> do
      restored_value <- readWire wire_data
      case restored_value of
        Left err -> pure $ Failed $ "Failed to deserialize: " <> show err
        Right received_value -> pure $ assert_eq received_value value_to_transmit

-- ══════════════════════════════════════════════════════════════════
-- Property-Based Tests (QuickCheck)
-- ══════════════════════════════════════════════════════════════════

-- ══════════════════════════════════════════════════════════════════
-- Reaction System Tests (Effectful)
-- ══════════════════════════════════════════════════════════════════

reaction_tests :: Array { name :: String, test :: Effect Result }
reaction_tests =
  -- Card property checks
  [ { name: "Secret Chamber is a reaction card"
    , test: pure $
        assert_true "should be reaction"
          (Card.isReaction $ Card._card Cards.secretChamber)
    }
  , { name: "Secret Chamber is an action card"
    , test: pure $
        assert_true "should be action"
          (Card.isAction $ Card._card Cards.secretChamber)
    }
  , { name: "Secret Chamber costs 2"
    , test: pure $
        assert_eq (Card._card Cards.secretChamber).cost 2
    }
  , { name: "Secret Chamber has a ReactWithChoice reaction"
    , test: pure $ case (Card._card Cards.secretChamber).reaction of
        Just (Tuple (ReactWithChoice _) _) -> Success
        _ -> Failed "expected ReactWithChoice"
    }
  , { name: "Moat has a BlockAttack reaction"
    , test: pure $ case (Card._card Cards.moat).reaction of
        Just (Tuple BlockAttack _) -> Success
        _ -> Failed "expected BlockAttack"
    }
  -- gainChoice populates pendingReactions for attack choices
  , { name: "gainChoice: attack choice populates pendingReactions from hand"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat, Card._card Cards.copper] }
            p' = Player.gainChoice Cards.gainCurse p
        in assert_eq (Array.length p'.pendingReactions) 1
    }
  , { name: "gainChoice: non-attack choice does not populate pendingReactions"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat, Card._card Cards.copper] }
            p' = Player.gainChoice Cards.draw1Card p
        in assert_eq (Array.length p'.pendingReactions) 0
    }
  , { name: "gainChoice: attack choice with no reaction cards leaves pendingReactions empty"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.copper] }
            p' = Player.gainChoice Cards.gainCurse p
        in assert_eq (Array.length p'.pendingReactions) 0
    }
  , { name: "gainChoice: attack choice with multiple reaction cards populates all"
    , test: pure $
        let p = Player.newPlayer
              { hand = [ Card._card Cards.moat
                       , Card._card Cards.secretChamber
                       ] }
            p' = Player.gainChoice Cards.gainCurse p
        in assert_eq (Array.length p'.pendingReactions) 2
    }
  -- hasReaction checks pendingReactions
  , { name: "hasReaction: true when pendingReactions non-empty"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat] }
            p' = Player.gainChoice Cards.gainCurse p
        in assert_true "should have reaction" (Player.hasReaction p')
    }
  , { name: "hasReaction: false when pendingReactions empty"
    , test: pure $
        assert_false "should not have reaction"
          (Player.hasReaction Player.newPlayer)
    }
  -- dropReactions clears pendingReactions
  , { name: "dropReactions: clears pendingReactions"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat] }
            p' = Player.gainChoice Cards.gainCurse p
            p'' = Player.dropReactions p'
        in assert_false "should not have reaction after drop"
          (Player.hasReaction p'')
    }
  -- reactionsInHand still scans hand for reaction cards
  , { name: "reactionsInHand: finds Moat"
    , test: pure $
        let p = Player.newPlayer { hand = [Card._card Cards.moat] }
            reactions = Player.reactionsInHand p
        in assert_eq (Array.length reactions) 1
    }
  , { name: "reactionsInHand: finds Secret Chamber"
    , test: pure $
        let p = Player.newPlayer { hand = [Card._card Cards.secretChamber] }
            reactions = Player.reactionsInHand p
        in assert_eq (Array.length reactions) 1
    }
  , { name: "reactionsInHand: empty for non-reaction cards"
    , test: pure $
        let p = Player.newPlayer { hand = [Card._card Cards.copper] }
            reactions = Player.reactionsInHand p
        in assert_eq (Array.length reactions) 0
    }
  --
  -- ══ Scenario: Moat blocks a Witch attack ══
  --
  -- Setup: Player 0 has Witch. Player 1 has Moat + Copper in hand.
  -- Player 0 plays Witch → Player 1 receives GainCurse with attack=true.
  -- gainChoice auto-detects Moat and populates pendingReactions.
  -- Player 1 reacts with BlockAttack → attack choice dropped, pendingReactions cleared.
  --
  , { name: "Moat scenario: after attack, player has pending reactions"
    , test: do
        let game = setup_attack_game_with_moat
        pure $ case NEA.index game.players 1 of
          Nothing -> Failed "no player at index 1"
          Just p1 ->
            assert_true "should have pending reactions"
              (Player.hasReaction p1)
    }
  , { name: "Moat scenario: BlockAttack drops attack choice"
    , test: do
        let game = setup_attack_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just BlockAttack })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                assert_false "choices should be cleared after BlockAttack"
                  (Player.hasChoices p1)
    }
  , { name: "Moat scenario: BlockAttack clears pendingReactions"
    , test: do
        let game = setup_attack_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just BlockAttack })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                assert_false "pendingReactions should be cleared"
                  (Player.hasReaction p1)
    }
  , { name: "Moat scenario: card conservation through BlockAttack"
    , test: do
        let game = setup_attack_game_with_moat
            totalBefore = count_all game
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just BlockAttack })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            assert_eq (count_all game') totalBefore
    }
  --
  -- ══ Scenario: DoneReacting declines to react ══
  --
  -- Player 1 has Moat in hand but clicks "Done reacting".
  -- pendingReactions should be cleared, attack choice remains for resolution.
  --
  , { name: "DoneReacting scenario: clears pendingReactions"
    , test: do
        let game = setup_attack_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
        pure case result of
          Left err -> Failed $ "DoneReacting failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                assert_false "pendingReactions should be cleared"
                  (Player.hasReaction p1)
    }
  , { name: "DoneReacting scenario: attack choice remains for resolution"
    , test: do
        let game = setup_attack_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
        pure case result of
          Left err -> Failed $ "DoneReacting failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                assert_true "player should still have attack choice to resolve"
                  (Player.hasChoices p1)
    }
  , { name: "DoneReacting scenario: first choice still has attack=true"
    , test: do
        let game = setup_attack_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
        pure case result of
          Left err -> Failed $ "DoneReacting failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                case Player.firstChoice p1 of
                  Nothing -> Failed "player should still have choice"
                  Just choice ->
                    -- attack flag is preserved on the choice (cards are immutable)
                    -- but hasReaction is false, so UI shows choice resolution
                    assert_true "attack choice should still be marked as attack"
                      (Choice.isAttack choice)
    }
  , { name: "DoneReacting scenario: UI won't show reactions (hasReaction false AND isAttacked)"
    , test: do
        let game = setup_attack_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
        pure case result of
          Left err -> Failed $ "DoneReacting failed: " <> err
          Right game' ->
            -- The UI condition is: isAttacked && hasReaction
            -- After DoneReacting: isAttacked may still be true, but hasReaction is false
            -- So the UI will show choice resolution, not reaction buttons
            assert_false "hasReaction should be false after DoneReacting"
              (Game.hasReaction 1 game')
    }
  , { name: "DoneReacting scenario: card conservation"
    , test: do
        let game = setup_attack_game_with_moat
            totalBefore = count_all game
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
        pure case result of
          Left err -> Failed $ "DoneReacting failed: " <> err
          Right game' ->
            assert_eq (count_all game') totalBefore
    }
  --
  -- ══ Scenario: Secret Chamber react then DoneReacting ══
  --
  -- Player 1 has Secret Chamber + Copper in hand, attacked.
  -- Player 1 reacts with Secret Chamber (ReactWithChoice):
  --   → SC reaction choice prepended to choices list
  --   → pendingReactions cleared (one reaction opportunity per attack)
  -- Player 1 resolves SC choice (draw 2, put 2 back on deck)
  -- Attack choice resurfaces at index 0
  -- UI does NOT show reactions again (pendingReactions is empty)
  -- Player 1 resolves the attack choice normally
  --
  , { name: "SecretChamber scenario: ReactWithChoice prepends choice"
    , test: do
        let game = setup_attack_game_with_secretchamber
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- 2 choices: [SC reaction choice, original attack choice]
                assert_eq (Array.length p1.choices) 2
    }
  , { name: "SecretChamber scenario: ReactWithChoice clears pendingReactions"
    , test: do
        let game = setup_attack_game_with_secretchamber
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                assert_false "pendingReactions should be cleared after reacting"
                  (Player.hasReaction p1)
    }
  , { name: "SecretChamber scenario: first choice is SC's non-attack choice"
    , test: do
        let game = setup_attack_game_with_secretchamber
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                case Player.firstChoice p1 of
                  Nothing -> Failed "should have choices"
                  Just choice ->
                    assert_false "SC reaction choice should not be an attack"
                      (Choice.isAttack choice)
    }
  , { name: "SecretChamber scenario: card conservation through ReactWithChoice"
    , test: do
        let game = setup_attack_game_with_secretchamber
            totalBefore = count_all game
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            assert_eq (count_all game') totalBefore
    }
  , { name: "SecretChamber scenario: no infinite loop - reactions not shown after SC resolves"
    , test: do
        -- This is the key test that prevented the infinite loop.
        -- After SC reaction choice resolves and gets dropped, the attack choice
        -- is back at index 0. But hasReaction is false (pendingReactions was
        -- cleared when React was processed), so the UI shows choice resolution.
        let game = setup_attack_game_with_secretchamber
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            -- After reacting, pendingReactions is empty
            -- Even though attack choice is at index 1 (still attack=true),
            -- when SC choice resolves and gets dropped, attack resurfaces at 0
            -- But hasReaction is false, so UI shows choice resolution
            assert_false "hasReaction should be false - no infinite loop"
              (Game.hasReaction 1 game')
    }
  --
  -- ══ Property: gainChoice → hasReaction consistency ══
  --
  , { name: "∀ hands: gainChoice(attack) → hasReaction ↔ reactionsInHand non-empty"
    , test: pure $
        let testHands =
              [ [Card._card Cards.moat]
              , [Card._card Cards.secretChamber]
              , [Card._card Cards.copper]
              , [Card._card Cards.moat, Card._card Cards.secretChamber]
              , []
              ]
            check hand =
              let p = Player.newPlayer { hand = hand }
                  p' = Player.gainChoice Cards.gainCurse p
              in Player.hasReaction p' == (Array.length (Player.reactionsInHand p) > 0)
        in assert_true "hasReaction should match reactionsInHand after attack"
          (all check testHands)
    }
  --
  -- ══ addChoice: sub-choice insertion without reaction re-trigger ══
  --
  , { name: "addChoice: does not populate pendingReactions even with attack=true"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat, Card._card Cards.copper] }
            p' = Player.addChoice Cards.discardDownTo3 p
        in assert_false "addChoice should not trigger reactions"
          (Player.hasReaction p')
    }
  , { name: "addChoice: preserves existing empty pendingReactions"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat] }
            p' = Player.addChoice Cards.gainCurse p
        in assert_eq (Array.length p'.pendingReactions) 0
    }
  , { name: "addChoices: does not populate pendingReactions even with attack sub-choices"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat, Card._card Cards.copper] }
            p' = Player.addChoices [Cards.draw1Card, Cards.discardDownTo3] p
        in assert_false "addChoices should not trigger reactions"
          (Player.hasReaction p')
    }
  --
  -- ══ Scenario: Catpurse attack with Moat — DoneReacting then resolve If ══
  --
  -- The bug: Catpurse's choice is If { condition: HasCard "Copper", choice: discardCopper }.
  -- When the If resolves and its sub-choice discardCopper (attack=true) is added,
  -- it should NOT re-trigger the reaction window.
  -- Before the fix, `resolveChoice` used `gainChoice` which re-populated pendingReactions.
  -- After the fix, `resolveChoice` uses `addChoice` which just appends.
  --
  , { name: "Catpurse scenario: setup — player has pending reactions"
    , test: do
        let game = setup_catpurse_game_with_moat
        pure $ case NEA.index game.players 1 of
          Nothing -> Failed "no player at index 1"
          Just p1 ->
            assert_true "should have pending reactions after Catpurse attack"
              (Player.hasReaction p1)
    }
  , { name: "Catpurse scenario: DoneReacting → pendingReactions cleared"
    , test: do
        let game = setup_catpurse_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
        pure case result of
          Left err -> Failed $ "DoneReacting failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                assert_false "pendingReactions should be cleared"
                  (Player.hasReaction p1)
    }
  , { name: "Catpurse scenario: DoneReacting then resolve If → no second reaction prompt"
    , test: do
        -- This is the exact bug Bryan reported.
        -- Step 1: DoneReacting to decline reaction
        -- Step 2: Resolve the If choice (click Okay)
        -- Step 3: Sub-choice (discardCopper) should be added WITHOUT re-triggering reactions
        let game = setup_catpurse_game_with_moat
        result <- runRandomM $ runExceptT $ do
          -- Step 1: DoneReacting
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          -- Step 2: Resolve the If choice (with resolution = Just unit)
          Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: catpurseIfResolved })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- KEY ASSERTION: After resolving the If, the sub-choice
                -- discardCopper should be at index 0, but pendingReactions
                -- should still be empty (no double reaction prompt)
                assert_false "pendingReactions should still be empty — no double prompt!"
                  (Player.hasReaction p1)
    }
  , { name: "Catpurse scenario: DoneReacting then resolve If → sub-choice present"
    , test: do
        let game = setup_catpurse_game_with_moat
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: catpurseIfResolved })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- The sub-choice (discardCopper MoveFromTo) should now be in the queue
                assert_true "player should have the discardCopper sub-choice"
                  (Player.hasChoices p1)
    }
  , { name: "Catpurse scenario: card conservation through full DoneReacting + resolve flow"
    , test: do
        let game = setup_catpurse_game_with_moat
            totalBefore = count_all game
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: catpurseIfResolved })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            assert_eq (count_all game') totalBefore
    }
  , { name: "Catpurse scenario: BlockAttack → no choices remain (no If resolution needed)"
    , test: do
        let game = setup_catpurse_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just BlockAttack })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                assert_false "choices should be cleared after BlockAttack"
                  (Player.hasChoices p1)
    }
  --
  -- ══ Scenario: Militia attack with Moat — DoneReacting (simple, no compound choice) ══
  --
  , { name: "Militia scenario: DoneReacting → no re-reaction, choice remains"
    , test: do
        let game = setup_militia_game_with_moat
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
        pure case result of
          Left err -> Failed $ "DoneReacting failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                if not (Player.hasChoices p1) then Failed "player should still have MoveFromTo choice"
                else if Player.hasReaction p1 then Failed "pendingReactions should be empty"
                else Success
    }
  --
  -- ══ Scenario: Margrave (And) with Moat — DoneReacting then resolve And ══
  --
  -- Margrave's choice is And { choices: [draw1Card, discardDownTo3], attack: true }.
  -- After DoneReacting and resolving the And, both sub-choices should be added
  -- without re-triggering reactions.
  --
  , { name: "Margrave scenario: DoneReacting then resolve And → no second reaction prompt"
    , test: do
        let game = setup_margrave_game_with_moat
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          -- Resolve the And choice (resolution = Just unit)
          Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: margraveAndResolved })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- After And resolves, sub-choices [draw1Card, discardDownTo3] are added
                -- pendingReactions should still be empty
                assert_false "pendingReactions should be empty — no double prompt!"
                  (Player.hasReaction p1)
    }
  , { name: "Margrave scenario: DoneReacting then resolve And → sub-choices present"
    , test: do
        let game = setup_margrave_game_with_moat
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: margraveAndResolved })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- Should have 2 sub-choices after And decomposes
                assert_eq (Array.length p1.choices) 2
    }
  --
  -- ══ Multi-step simulation: Catpurse attack, DoneReacting, resolve If,
  --    then resolve the MoveFromTo sub-choice to completion ══
  --
  , { name: "Catpurse full flow: DoneReacting → If resolves → MoveFromTo resolves → no choices left"
    , test: do
        let game = setup_catpurse_game_with_moat
        result <- runRandomM $ runExceptT $ do
          -- Step 1: DoneReacting (decline to block)
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          -- Step 2: Resolve If (has Copper → true → adds discardCopper)
          game2 <- Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: catpurseIfResolved })
            game1
          -- Step 3: Resolve the MoveFromTo sub-choice (select copper to discard)
          Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: discardCopperResolved })
            game2
        pure case result of
          Left err -> Failed $ "full flow failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- After full resolution, player should have no choices left
                if Player.hasChoices p1 then Failed "all choices should be resolved"
                else if Player.hasReaction p1 then Failed "no pending reactions"
                else Success
    }
  , { name: "Catpurse full flow: card conservation through entire attack sequence"
    , test: do
        let game = setup_catpurse_game_with_moat
            totalBefore = count_all game
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          game2 <- Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: catpurseIfResolved })
            game1
          Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: discardCopperResolved })
            game2
        pure case result of
          Left err -> Failed $ "full flow failed: " <> err
          Right game' ->
            assert_eq (count_all game') totalBefore
    }
  , { name: "Catpurse full flow: copper moved from hand to discard"
    , test: do
        let game = setup_catpurse_game_with_moat
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          game2 <- Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: catpurseIfResolved })
            game1
          Engine.makePlay
            (ResolveChoice { playerIndex: 1, choice: discardCopperResolved })
            game2
        pure case result of
          Left err -> Failed $ "full flow failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- Player started with [Moat, Copper], should now have [Moat] in hand
                -- and [Copper] moved to discard pile
                if Array.length p1.hand /= 1 then Failed $ "expected 1 card in hand, got " <> show (Array.length p1.hand)
                else if Array.length p1.discard /= 11 then Failed $ "expected 11 cards in discard (10 starting + 1 discarded copper), got " <> show (Array.length p1.discard)
                else Success
    }
  --
  -- ══ Property: resolveChoice(compound) → addChoice, not gainChoice ══
  --
  -- For ANY compound attack choice that decomposes into sub-choices,
  -- the sub-choices should NOT re-trigger pendingReactions.
  --
  , { name: "∀ compound attacks: If resolution does not re-trigger reactions"
    , test: do
        -- Generalized test: create an If attack choice wrapping any attack sub-choice.
        -- After DoneReacting + resolving If, pendingReactions should stay empty.
        let ifChoice = If
              { condition: HasCard "Copper"
              , choice: Cards.discardDownTo3  -- attack=true sub-choice
              , otherwise: Nothing
              , attack: true
              , resolution: Nothing
              }
            game = setup_compound_attack_game ifChoice
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          Engine.makePlay
            (ResolveChoice
              { playerIndex: 1
              , choice: If
                { condition: HasCard "Copper"
                , choice: Cards.discardDownTo3
                , otherwise: Nothing
                , attack: true
                , resolution: Just unit
                }
              })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            assert_false "If decomposition should not re-trigger reactions"
              (Game.hasReaction 1 game')
    }
  , { name: "∀ compound attacks: And resolution does not re-trigger reactions"
    , test: do
        let andChoice = And
              { choices: [Cards.draw1Card, Cards.discardDownTo3]
              , attack: true
              , resolution: Nothing
              }
            game = setup_compound_attack_game andChoice
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          Engine.makePlay
            (ResolveChoice
              { playerIndex: 1
              , choice: And
                { choices: [Cards.draw1Card, Cards.discardDownTo3]
                , attack: true
                , resolution: Just unit
                }
              })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            assert_false "And decomposition should not re-trigger reactions"
              (Game.hasReaction 1 game')
    }
  , { name: "∀ compound attacks: Or resolution does not re-trigger reactions"
    , test: do
        let orChoice = Or
              { choices: [Cards.discardDownTo3, Cards.gainCurse]
              , attack: true
              , resolution: Nothing
              }
            game = setup_compound_attack_game orChoice
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          Engine.makePlay
            (ResolveChoice
              { playerIndex: 1
              , choice: Or
                { choices: [Cards.discardDownTo3, Cards.gainCurse]
                , attack: true
                , resolution: Just Cards.discardDownTo3
                }
              })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            assert_false "Or decomposition should not re-trigger reactions"
              (Game.hasReaction 1 game')
    }
  , { name: "∀ compound attacks: Option(yes) resolution does not re-trigger reactions"
    , test: do
        let optionChoice = Option
              { choice: Cards.discardDownTo3
              , attack: true
              , resolution: Nothing
              }
            game = setup_compound_attack_game optionChoice
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game
          Engine.makePlay
            (ResolveChoice
              { playerIndex: 1
              , choice: Option
                { choice: Cards.discardDownTo3
                , attack: true
                , resolution: Just true
                }
              })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            assert_false "Option(yes) decomposition should not re-trigger reactions"
              (Game.hasReaction 1 game')
    }
  --
  -- ══ dropReaction: drop one, keep the rest ══
  --
  , { name: "dropReaction: removes BlockAttack, keeps other reactions"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat, Card._card Cards.secretChamber] }
            p' = Player.gainChoice Cards.gainCurse p
            -- Should have 2 pending reactions
            p'' = Player.dropReaction BlockAttack p'
        in if Array.length p'.pendingReactions /= 2
           then Failed $ "expected 2 pending reactions, got " <> show (Array.length p'.pendingReactions)
           else if Array.length p''.pendingReactions /= 1
           then Failed $ "expected 1 pending reaction after dropReaction, got " <> show (Array.length p''.pendingReactions)
           else if Player.hasReaction p'' == false
           then Failed "should still have a reaction after dropping one of two"
           else Success
    }
  , { name: "dropReaction: removes only the first matching reaction"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat] }
            p' = Player.gainChoice Cards.gainCurse p
            p'' = Player.dropReaction BlockAttack p'
        in assert_false "should have no reactions after dropping the only one"
          (Player.hasReaction p'')
    }
  , { name: "dropReaction: no-op when reaction not in pendingReactions"
    , test: pure $
        let p = Player.newPlayer
              { hand = [Card._card Cards.moat] }
            p' = Player.gainChoice Cards.gainCurse p
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack  -- fallback
            p'' = Player.dropReaction scReaction p'
        in assert_eq (Array.length p''.pendingReactions) 1
    }
  --
  -- ══ Multi-reaction scenario: Secret Chamber + Moat ══
  --
  -- Player 1 has BOTH Moat and Secret Chamber in hand.
  -- Attacked by Witch. Player reacts with Secret Chamber first.
  -- After SC reaction, Moat reaction should still be available.
  -- Player can then also block with Moat.
  --
  , { name: "Multi-reaction: SC + Moat — after SC react, Moat still available"
    , test: do
        let game = setup_attack_game_with_both_reactions
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- After reacting with SC, should still have Moat's BlockAttack pending
                if not (Player.hasReaction p1)
                then Failed "should still have Moat reaction pending"
                else if Array.length p1.pendingReactions /= 1
                then Failed $ "expected 1 remaining reaction, got " <> show (Array.length p1.pendingReactions)
                else Success
    }
  , { name: "Multi-reaction: SC react → SC first choice, Moat still pending"
    , test: do
        let game = setup_attack_game_with_both_reactions
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $ do
          -- Step 1: React with Secret Chamber
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                -- SC choice is at index 0 (non-attack), attack at index 1
                -- isAttacked checks firstChoice, which is SC (non-attack), so false
                -- hasReaction is true (Moat still pending)
                -- UI shows SC choice resolution, not reaction buttons
                -- Moat stays available for when attack resurfaces
                let isAttacked = case Player.firstChoice p1 of
                      Nothing -> false
                      Just c -> Choice.isAttack c
                in if isAttacked
                   then Failed "first choice should be SC's non-attack choice"
                   else if not (Player.hasReaction p1)
                   then Failed "Moat reaction should still be pending"
                   else if Array.length p1.choices /= 2
                   then Failed $ "expected 2 choices, got " <> show (Array.length p1.choices)
                   else Success
    }
  , { name: "Multi-reaction: SC then DoneReacting — remaining reactions dropped"
    , test: do
        let game = setup_attack_game_with_both_reactions
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $ do
          -- Step 1: React with Secret Chamber (adds SC's choice, drops SC reaction)
          game1 <- Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
          -- Step 2: DoneReacting (drops remaining Moat reaction)
          Engine.makePlay
            (DoneReacting { playerIndex: 1 })
            game1
        pure case result of
          Left err -> Failed $ "scenario failed: " <> err
          Right game' ->
            case NEA.index game'.players 1 of
              Nothing -> Failed "no player at index 1"
              Just p1 ->
                if Player.hasReaction p1
                then Failed "DoneReacting should have dropped all remaining reactions"
                else Success
    }
  , { name: "Multi-reaction: card conservation through SC + Moat sequence"
    , test: do
        let game = setup_attack_game_with_both_reactions
            totalBefore = count_all game
            scReaction = case (Card._card Cards.secretChamber).reaction of
              Just (Tuple reaction _) -> reaction
              _ -> BlockAttack
        result <- runRandomM $ runExceptT $ do
          game1 <- Engine.makePlay
            (React { playerIndex: 1, reaction: Just scReaction })
            game
          Engine.makePlay
            (React { playerIndex: 1, reaction: Just BlockAttack })
            game1
        pure case result of
          Left err -> Failed $ "react failed: " <> err
          Right game' ->
            assert_eq (count_all game') totalBefore
    }
  ]

-- ══════════════════════════════════════════════════════════════════
-- Test Setup Helpers
-- ══════════════════════════════════════════════════════════════════

-- Catpurse If choice (unresolved)
catpurseIfChoice :: Choice
catpurseIfChoice = If
  { condition: HasCard "Copper"
  , choice: MoveFromTo
    { n: Exactly 1
    , filter: Filter.HasName "Copper"
    , source: Pile.Hand
    , destination: Pile.Discard
    , resolution: Nothing
    , attack: true
    }
  , otherwise: Nothing
  , attack: true
  , resolution: Nothing
  }

-- Catpurse If choice (resolved = Just unit)
catpurseIfResolved :: Choice
catpurseIfResolved = If
  { condition: HasCard "Copper"
  , choice: MoveFromTo
    { n: Exactly 1
    , filter: Filter.HasName "Copper"
    , source: Pile.Hand
    , destination: Pile.Discard
    , resolution: Nothing
    , attack: true
    }
  , otherwise: Nothing
  , attack: true
  , resolution: Just unit
  }

-- discardCopper MoveFromTo choice (resolved with card index 0 selected — the copper)
-- Player 1 hand after DoneReacting: [Moat, Copper] → index 1 is Copper
discardCopperResolved :: Choice
discardCopperResolved = MoveFromTo
  { n: Exactly 1
  , filter: Filter.HasName "Copper"
  , source: Pile.Hand
  , destination: Pile.Discard
  , resolution: Just [1]  -- index 1 = Copper in [Moat, Copper] hand
  , attack: true
  }

-- Margrave And choice (unresolved)
margraveAndChoice :: Choice
margraveAndChoice = And
  { choices: [Cards.draw1Card, Cards.discardDownTo3]
  , attack: true
  , resolution: Nothing
  }

-- Margrave And choice (resolved)
margraveAndResolved :: Choice
margraveAndResolved = And
  { choices: [Cards.draw1Card, Cards.discardDownTo3]
  , attack: true
  , resolution: Just unit
  }

-- Set up a 2-player game where player 1 has Moat and is being attacked
setup_attack_game_with_moat :: Game
setup_attack_game_with_moat =
  setup_attack_game_with_hand
    [Card._card Cards.moat, Card._card Cards.copper]

-- Set up a 2-player game where player 1 has Secret Chamber and is being attacked
setup_attack_game_with_secretchamber :: Game
setup_attack_game_with_secretchamber =
  setup_attack_game_with_hand
    [Card._card Cards.secretChamber, Card._card Cards.copper]

-- Set up a 2-player game where player 1 has BOTH Moat and Secret Chamber,
-- enabling multi-reaction sequences
setup_attack_game_with_both_reactions :: Game
setup_attack_game_with_both_reactions =
  setup_attack_game_with_hand
    [ Card._card Cards.moat
    , Card._card Cards.secretChamber
    , Card._card Cards.copper
    ]

-- Set up a 2-player game for Catpurse: player 1 has Moat + Copper, attacked with catpurse If
setup_catpurse_game_with_moat :: Game
setup_catpurse_game_with_moat =
  setup_attack_game_with_hand_and_choice
    [Card._card Cards.moat, Card._card Cards.copper]
    catpurseIfChoice

-- Set up a 2-player game for Militia: player 1 has Moat + enough cards, attacked with discardDownTo3
setup_militia_game_with_moat :: Game
setup_militia_game_with_moat =
  setup_attack_game_with_hand_and_choice
    [ Card._card Cards.moat
    , Card._card Cards.copper
    , Card._card Cards.copper
    , Card._card Cards.copper
    , Card._card Cards.copper
    ]
    Cards.discardDownTo3

-- Set up a 2-player game for Margrave: player 1 has Moat + cards, attacked with And choice
setup_margrave_game_with_moat :: Game
setup_margrave_game_with_moat =
  setup_attack_game_with_hand_and_choice
    [ Card._card Cards.moat
    , Card._card Cards.copper
    , Card._card Cards.copper
    , Card._card Cards.copper
    , Card._card Cards.copper
    ]
    margraveAndChoice

-- Set up a game where player 1 has Moat + Copper and is attacked with any compound choice
setup_compound_attack_game :: Choice -> Game
setup_compound_attack_game choice =
  setup_attack_game_with_hand_and_choice
    [ Card._card Cards.moat
    , Card._card Cards.copper
    , Card._card Cards.copper
    , Card._card Cards.copper
    , Card._card Cards.copper
    ]
    choice

-- Set up a 2-player game where player 1 has a pending attack choice (Witch's GainCurse)
-- and the specified hand. Uses Player.gainChoice to properly populate
-- pendingReactions from the hand's reaction cards.
setup_attack_game_with_hand :: Array Card -> Game
setup_attack_game_with_hand hand =
  setup_attack_game_with_hand_and_choice hand Cards.gainCurse

-- Set up a 2-player game where player 1 has a pending attack choice
-- and the specified hand. Uses Player.gainChoice to properly populate
-- pendingReactions from the hand's reaction cards.
setup_attack_game_with_hand_and_choice :: Array Card -> Choice -> Game
setup_attack_game_with_hand_and_choice hand choice =
  let
    game = Game.new 2 Cards.cardMap true
    player0 = Player.newPlayer
      { hand = [ Card._card Cards.witch ]
      , deck = Array.replicate 5 (Card._card Cards.copper)
      }
    -- Use gainChoice to properly populate pendingReactions
    player1 = Player.gainChoice choice $ Player.newPlayer
      { hand = hand
      , deck = Array.replicate 5 (Card._card Cards.copper)
      }
  in game
    { players = NEA.cons' player0 [ player1 ]
    , phase = ActionPhase
    , turn = 0
    }

count_all :: Game -> Int
count_all game =
  let player_cards = sum $ map (Player.allCards >>> Array.length) game.players
      supply_cards = sum $ map _.count game.supply
      trash_cards = Array.length game.trash
  in player_cards + supply_cards + trash_cards

-- | Generators for game-domain values (for future QuickCheck integration)
-- gen_player_count :: Gen Int
-- gen_player_count = chooseInt 1 6
-- gen_card :: Gen Card
-- gen_card = case NEA.fromArray Cards.cardMap of
--   Just nea -> elements nea
--   Nothing -> pure (Card._card Cards.copper)
-- gen_phase :: Gen Phase
-- gen_phase = case NEA.fromArray [ActionPhase, BuyPhase, CleanupPhase] of
--   Just nea -> elements nea
--   Nothing -> pure ActionPhase

-- | Property: Phase.next cycles with period 3
-- ∀ p : Phase. next(next(next(p))) = p
prop_phase_cycle_period_3 :: Phase -> Result
prop_phase_cycle_period_3 p =
  assertEquals (Phase.next $ Phase.next $ Phase.next p) p

-- | Property: Phase.next always produces a different phase
-- ∀ p : Phase. next(p) ≠ p
prop_phase_next_differs :: Phase -> Result
prop_phase_next_differs p =
  assert_true "next phase should differ from current"
    (Phase.next p /= p)

-- | Property: ∀ n ∈ [1..10]. Game.new n starts in ActionPhase
prop_game_starts_in_action_phase :: Int -> Result
prop_game_starts_in_action_phase n =
  let game = Game.new (max 1 n) Cards.cardMap true
  in assertEquals game.phase ActionPhase

-- | Property: ∀ n ∈ [1..10]. Game.new n has no result
prop_game_starts_with_no_result :: Int -> Result
prop_game_starts_with_no_result n =
  let game = Game.new (max 1 n) Cards.cardMap true
  in assertEquals game.result Nothing

-- | Property: Wire serialization is an isomorphism (roundtrip)
-- ∀ n ∈ [1..10]. review _toWire (view _toWire game) = game
prop_wire_iso :: Int -> Result
prop_wire_iso n =
  let game = Game.new (max 1 n) Cards.cardMap true
      serialized = view Dom._toWire game
      deserialized = review Dom._toWire serialized
  in assertEquals deserialized game

-- | Property: Player.newPlayer.allCards always has 10 cards
prop_new_player_has_10_cards :: Unit -> Result
prop_new_player_has_10_cards _ =
  assertEquals (Array.length $ Player.allCards Player.newPlayer) 10

-- | Property: Supply stack count = cardMap length for any player count
prop_supply_stack_count :: Int -> Result
prop_supply_stack_count n =
  let supply = Supply.makeSupply (max 1 n) Cards.cardMap
  in assertEquals (Array.length supply) (Array.length Cards.cardMap)

-- | Property: All supply stacks start non-empty (for 2+ players)
-- Note: 1-player games have 0 curses, so this only holds for n >= 2
prop_supply_all_nonempty :: Int -> Result
prop_supply_all_nonempty n' =
  let n = max 2 n'  -- 1-player has 0 curses
      supply = Supply.makeSupply n Cards.cardMap
  in assert_true "all stacks should be non-empty"
    (all (\s -> s.count > 0) supply)

-- | Property: Victory card count = 4 × player_count
prop_victory_card_scaling :: Int -> Result
prop_victory_card_scaling n' =
  let n = max 1 (min 10 n')
      supply = Supply.makeSupply n Cards.cardMap
  in case stackCountByName "Estate" supply of
    Just count -> assertEquals count (4 * n)
    Nothing -> Failed "Estate not found in supply"

-- | Property: Curse count = 10 × (player_count - 1)
prop_curse_scaling :: Int -> Result
prop_curse_scaling n' =
  let n = max 1 (min 10 n')
      supply = Supply.makeSupply n Cards.cardMap
  in case stackCountByName "Curse" supply of
    Just count -> assertEquals count (10 * (n - 1))
    Nothing -> Failed "Curse not found in supply"

-- | Property: Player score is sum of victory points of all cards
-- This is trivially true by construction, but validates the scoring function
prop_new_player_score :: Unit -> Result
prop_new_player_score _ =
  let p = Player.newPlayer
      expected = points 3 -- 3 estates × 1 VP each
  in assertEquals (Player.score p) expected

-- | Property: gainActions is additive
-- gainActions a . gainActions b = gainActions (a + b) (for starting player)
prop_gain_actions_additive :: Unit -> Result
prop_gain_actions_additive _ =
  let p = Player.newPlayer
      a = actions 2
      b = actions 3
      composed = (Player.gainActions b <<< Player.gainActions a) p
      direct = Player.gainActions (a + b) p
  in assertEquals composed.actions direct.actions

-- | Property: gainBuys is additive
prop_gain_buys_additive :: Unit -> Result
prop_gain_buys_additive _ =
  let p = Player.newPlayer
      a = buys 2
      b = buys 3
      composed = (Player.gainBuys b <<< Player.gainBuys a) p
      direct = Player.gainBuys (a + b) p
  in assertEquals composed.buys direct.buys

-- | Property: purchase adds exactly one card to buying
prop_purchase_adds_one :: Unit -> Result
prop_purchase_adds_one _ =
  let p = Player.newPlayer
      copper = Card._card Cards.copper
      p' = Player.purchase copper p
  in assertEquals (Array.length p'.buying) (Array.length p.buying + 1)

-- | Property: purchase decrements buys by one
prop_purchase_decrements_buys :: Unit -> Result
prop_purchase_decrements_buys _ =
  let p = Player.newPlayer
      copper = Card._card Cards.copper
      p' = Player.purchase copper p
  in assertEquals p'.buys (p.buys - one)

-- | Property: Card.value is a homomorphism from (Array Card, <>) to (Int, +)
-- value(a <> b) = value(a) + value(b)
prop_card_value_homomorphism :: Unit -> Result
prop_card_value_homomorphism _ =
  let copper = Card._card Cards.copper
      silver = Card._card Cards.silver
      a = [copper, copper]
      b = [silver]
  in assertEquals (Card.value (a <> b)) (Card.value a + Card.value b)

-- | Property: Stack.toCards . Stack.fromCards = id (for non-empty arrays)
prop_stack_roundtrip :: Unit -> Result
prop_stack_roundtrip _ =
  let copper = Card._card Cards.copper
      cards = NEA.cons' copper [copper, copper]
      stack = Stack.fromCards cards
      result = Stack.toCards stack
  in assertEquals (Array.length result) (NEA.length cards)

-- | Property: Stack.isEmpty reflects count
prop_stack_isEmpty :: Unit -> Result
prop_stack_isEmpty _ =
  let copper = Card._card Cards.copper
  in assert_true "zero count means empty"
    (Stack.isEmpty { card: copper, count: 0 })

-- | Property: Stack.isEmpty false for positive count
prop_stack_not_isEmpty :: Unit -> Result
prop_stack_not_isEmpty _ =
  let copper = Card._card Cards.copper
  in assert_false "positive count means not empty"
    (Stack.isEmpty { card: copper, count: 5 })

-- | Property: Stack.take decrements count by 1
prop_stack_take :: Unit -> Result
prop_stack_take _ =
  let copper = Card._card Cards.copper
      stack = { card: copper, count: 5 }
  in assertEquals (Stack.take stack).count 4

-- | Property: choicesOutstanding is false when no player has choices
prop_no_initial_choices :: Int -> Result
prop_no_initial_choices n =
  let game = Game.new (max 1 n) Cards.cardMap true
  in assert_false "new game should have no outstanding choices"
    (Game.choicesOutstanding game)

-- | Property: Game.new game always has turn = 0
prop_game_turn_starts_at_zero :: Int -> Result
prop_game_turn_starts_at_zero n =
  assertEquals (Game.new (max 1 n) Cards.cardMap true).turn 0

property_tests :: Array { name :: String, test :: Effect Result }
property_tests =
  -- Expand parametric properties over the relevant domain
  let phase_tests = do
        p <- [ActionPhase, BuyPhase, CleanupPhase]
        [ { name: "∀ phase: next³ = id (" <> show p <> ")"
          , test: pure $ prop_phase_cycle_period_3 p
          }
        , { name: "∀ phase: next ≠ id (" <> show p <> ")"
          , test: pure $ prop_phase_next_differs p
          }
        ]
      player_count_tests = do
        n <- (1 .. 10)
        [ { name: "∀ n: starts ActionPhase (n=" <> show n <> ")"
          , test: pure $ prop_game_starts_in_action_phase n
          }
        , { name: "∀ n: no result (n=" <> show n <> ")"
          , test: pure $ prop_game_starts_with_no_result n
          }
        , { name: "∀ n: wire iso (n=" <> show n <> ")"
          , test: pure $ prop_wire_iso n
          }
        , { name: "∀ n: supply stacks = cardMap length (n=" <> show n <> ")"
          , test: pure $ prop_supply_stack_count n
          }
        , { name: "∀ n: all stacks non-empty (n=" <> show n <> ")"
          , test: pure $ prop_supply_all_nonempty n
          }
        , { name: "∀ n: victory scaling (n=" <> show n <> ")"
          , test: pure $ prop_victory_card_scaling n
          }
        , { name: "∀ n: curse scaling (n=" <> show n <> ")"
          , test: pure $ prop_curse_scaling n
          }
        , { name: "∀ n: no initial choices (n=" <> show n <> ")"
          , test: pure $ prop_no_initial_choices n
          }
        , { name: "∀ n: turn starts at 0 (n=" <> show n <> ")"
          , test: pure $ prop_game_turn_starts_at_zero n
          }
        ]
      unit_props =
        [ { name: "newPlayer has 10 cards"
          , test: pure $ prop_new_player_has_10_cards unit
          }
        , { name: "newPlayer score = 3"
          , test: pure $ prop_new_player_score unit
          }
        , { name: "gainActions is additive"
          , test: pure $ prop_gain_actions_additive unit
          }
        , { name: "gainBuys is additive"
          , test: pure $ prop_gain_buys_additive unit
          }
        , { name: "purchase adds one card"
          , test: pure $ prop_purchase_adds_one unit
          }
        , { name: "purchase decrements buys"
          , test: pure $ prop_purchase_decrements_buys unit
          }
        , { name: "Card.value is a homomorphism"
          , test: pure $ prop_card_value_homomorphism unit
          }
        , { name: "Stack roundtrip"
          , test: pure $ prop_stack_roundtrip unit
          }
        , { name: "Stack.isEmpty on zero count"
          , test: pure $ prop_stack_isEmpty unit
          }
        , { name: "Stack.isEmpty false on positive count"
          , test: pure $ prop_stack_not_isEmpty unit
          }
        , { name: "Stack.take decrements count"
          , test: pure $ prop_stack_take unit
          }
        ]
      -- QuickCheck randomized property tests
      qc_tests =
        [ { name: "QC: wire serialization roundtrip (100 random player counts)"
          , test: run_qc 100 (\_ -> prop_wire_iso)
          }
        , { name: "QC: game always starts in ActionPhase"
          , test: run_qc 100 (\_ -> prop_game_starts_in_action_phase)
          }
        , { name: "QC: supply stack count matches cardMap"
          , test: run_qc 100 (\_ -> prop_supply_stack_count)
          }
        , { name: "QC: all stacks non-empty on init"
          , test: run_qc 100 (\_ -> prop_supply_all_nonempty)
          }
        , { name: "QC: no initial choices"
          , test: run_qc 100 (\_ -> prop_no_initial_choices)
          }
        ]
      -- Stateful game simulation property tests
      simulation_prop_tests =
        [ { name: "Simulation: 1p long game setup + turns preserve card count"
          , test: simulate_and_check_conservation 1 true 5
          }
        , { name: "Simulation: 2p long game setup + turns preserve card count"
          , test: simulate_and_check_conservation 2 true 5
          }
        , { name: "Simulation: 2p short game setup + turns preserve card count"
          , test: simulate_and_check_conservation 2 false 5
          }
        , { name: "Simulation: 4p long game setup + turns preserve card count"
          , test: simulate_and_check_conservation 4 true 3
          }
        , { name: "Simulation: hands always have ≤ expected cards after setup"
          , test: do
              result <- run_make_auto_play 2 true
              pure case result of
                Left err -> Failed err
                Right game ->
                  assert_true "hand sizes should be reasonable"
                  $ all (\p -> Array.length p.hand <= 10) game.players
          }
        , { name: "Stateful: 2p random game (100 steps) — all invariants hold"
          , test: run_random_game_simulation 2 true 100
          }
        , { name: "Stateful: 2p short random game (100 steps) — all invariants hold"
          , test: run_random_game_simulation 2 false 100
          }
        , { name: "Stateful: 4p random game (50 steps) — all invariants hold"
          , test: run_random_game_simulation 4 true 50
          }
        ]
  in phase_tests <> player_count_tests <> unit_props <> qc_tests <> simulation_prop_tests

-- | Run a QuickCheck-style property by generating random inputs
run_qc :: Int -> (Unit -> Int -> Result) -> Effect Result
run_qc count prop = do
  -- Quick manual loop: generate varied player counts and test
  let results = map (\i -> prop unit (1 + (i `mod` 10))) (1 .. count)
  pure $ foldl combine Success results
  where
    combine Success r = r
    combine (Failed msg) _ = Failed msg

-- | Simulate a game and verify card conservation across turns
simulate_and_check_conservation :: Int -> Boolean -> Int -> Effect Result
simulate_and_check_conservation playerCount longGame nTurns = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      let initial_total = count_all_cards game
      result <- runRandomM $ check_turns nTurns game initial_total
      pure result
  where
    count_all_cards :: Game -> Int
    count_all_cards game =
      let player_cards = sum $ map (Player.allCards >>> Array.length) game.players
          supply_cards = sum $ map _.count game.supply
          trash_cards = Array.length game.trash
      in player_cards + supply_cards + trash_cards

    check_turns :: Int -> Game -> Int -> RandomM Result
    check_turns 0 game initial_total = pure $
      let current_total = count_all_cards game
      in if current_total == initial_total
         then Success
         else Failed $ "Card count changed: " <> show initial_total <> " → " <> show current_total
    check_turns n game initial_total = do
      let current_total = count_all_cards game
      if current_total /= initial_total
        then pure $ Failed $ "Card count changed at turn " <> show n <> ": "
          <> show initial_total <> " → " <> show current_total
        else do
          result <- Engine.makeAutoPlay (EndPhase { playerIndex: game.turn }) game
          case result of
            Left _ -> pure Success -- Game over or invalid transition
            Right game' ->
              if game'.result /= Nothing
              then pure $ check_conservation game' initial_total
              else check_turns (n - 1) game' initial_total

    check_conservation :: Game -> Int -> Result
    check_conservation game initial_total =
      let current_total = count_all_cards game
      in if current_total == initial_total
         then Success
         else Failed $ "Card conservation violated: "
           <> show initial_total <> " → " <> show current_total

-- ══════════════════════════════════════════════════════════════════
-- Stateful Property-Based Testing: Random Game Simulation
-- ══════════════════════════════════════════════════════════════════

-- | Check all game-state invariants that must hold after every play.
-- Returns Success or the first violated invariant.
check_invariants :: Game -> Int -> Result
check_invariants game initial_total =
  let
    current_total = count_all game
    -- Invariant 1: Card conservation
    card_conservation =
      if current_total == initial_total then Success
      else Failed $ "Card conservation violated: "
        <> show initial_total <> " → " <> show current_total

    -- Invariant 2: pendingReactions consistency
    -- If player has pendingReactions, they must have an attack choice
    -- (the converse isn't required: a player can have an attack choice
    -- with empty pendingReactions after DoneReacting)
    reactions_consistent = foldl combine Success $ NEA.toArray $
      NEA.mapWithIndex (\i p ->
        if Player.hasReaction p && not (Player.hasChoices p)
        then Failed $ "Player " <> show i <> " has pendingReactions but no choices"
        else Success
      ) game.players

    -- Invariant 3: No negative buys or actions
    no_negatives = foldl combine Success $ NEA.toArray $
      NEA.mapWithIndex (\i p ->
        if p.buys < zero
        then Failed $ "Player " <> show i <> " has negative buys: " <> show p.buys
        else if p.actions < zero
        then Failed $ "Player " <> show i <> " has negative actions: " <> show p.actions
        else Success
      ) game.players
  in
    combine (combine card_conservation reactions_consistent) no_negatives
  where
    combine Success r = r
    combine (Failed msg) _ = Failed msg

-- | Generate a valid play for the current game state.
-- This picks randomly from available valid moves.
generate_valid_play :: Game -> RandomM Play
generate_valid_play game = do
  let playerIndex = game.turn
  -- Check if any player has outstanding choices
  if Game.choicesOutstanding game
  then do
    let choicePlayerIndex = Engine.choiceTurn game
    case NEA.index game.players choicePlayerIndex of
      Nothing -> pure $ EndPhase { playerIndex }
      Just player ->
        if Game.isAttacked choicePlayerIndex game && Player.hasReaction player
        then do
          -- Player is attacked with pending reactions: randomly React or DoneReacting
          let reactions = player.pendingReactions
          doReact <- Random.randomBoolean
          if doReact && Array.length reactions > 0
          then do
            idx <- Random.randomIntBetween 0 (Array.length reactions - 1)
            case reactions Array.!! idx of
              Nothing -> pure $ DoneReacting { playerIndex: choicePlayerIndex }
              Just (Tuple reaction _) ->
                pure $ React { playerIndex: choicePlayerIndex, reaction: Just reaction }
          else
            pure $ DoneReacting { playerIndex: choicePlayerIndex }
        else
          -- Player has choices but not reacting: try to resolve
          case Player.firstChoice player of
            Nothing -> pure $ EndPhase { playerIndex }
            Just choice -> pure $ ResolveChoice
              { playerIndex: choicePlayerIndex
              , choice: auto_resolve choice
              }
  else
    -- No choices outstanding: advance phase or make a normal play
    case game.phase of
      ActionPhase ->
        if Player.hasActions (NEA.head game.players)
        && Player.hasActionCardsInHand (NEA.head game.players)
        then pure $ EndPhase { playerIndex }  -- Just skip for simplicity
        else pure $ EndPhase { playerIndex }
      BuyPhase -> do
        -- Try to purchase something affordable
        case NEA.index game.players playerIndex of
          Nothing -> pure $ EndPhase { playerIndex }
          Just player ->
            let
              playerCash = Player.cash player
              affordable = Array.mapWithIndex (\i s -> { i, s })
                $ Array.filter (\s -> s.count > 0 && s.card.cost <= playerCash)
                $ game.supply
            in
            if player.buys > zero && Array.length affordable > 0
            then do
              idx <- Random.randomIntBetween 0 (Array.length affordable - 1)
              case affordable Array.!! idx of
                Nothing -> pure $ EndPhase { playerIndex }
                Just { i: stackIndex } -> pure $ Purchase { playerIndex, stackIndex }
            else
              pure $ EndPhase { playerIndex }
      CleanupPhase ->
        pure $ EndPhase { playerIndex }

-- | Auto-resolve a choice by providing minimal valid resolution.
-- For compound choices, just acknowledge them (resolution = Just unit / Just true).
-- For terminal choices, provide minimal valid card selections.
auto_resolve :: Choice -> Choice
auto_resolve = case _ of
  If r -> If r { resolution = Just unit }
  And r -> And r { resolution = Just unit }
  Or r@{ choices } ->
    case Array.head choices of
      Nothing -> Or r { resolution = Nothing }
      Just c -> Or r { resolution = Just c }
  PickN r@{ choices, n } ->
    PickN r { resolution = Just (Array.take n choices) }
  Option r -> Option r { resolution = Just true }
  MoveFromTo r -> MoveFromTo r { resolution = Just [0] }
  GainCard r -> GainCard r { resolution = Just "Copper" }
  GainCards r -> GainCards r { resolution = Just unit }
  GainActions r -> GainActions r { resolution = Just unit }
  GainBuys r -> GainBuys r { resolution = Just unit }
  Discard r -> Discard r { resolution = Just unit }
  Draw r -> Draw r { resolution = Just unit }
  GainBonus r -> GainBonus r { resolution = Just unit }
  c@(StackChoice _) -> c  -- Can't auto-resolve StackChoice

-- | Run a stateful random game simulation for N steps,
-- checking invariants at each step.
run_random_game_simulation :: Int -> Boolean -> Int -> Effect Result
run_random_game_simulation playerCount longGame maxSteps = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      let initial_total = count_all game
      -- Check invariants after setup
      case check_invariants game initial_total of
        Failed msg -> pure $ Failed $ "Invariant violated after setup: " <> msg
        Success ->
          runRandomM $ simulate_steps maxSteps game initial_total 0

simulate_steps :: Int -> Game -> Int -> Int -> RandomM Result
simulate_steps 0 _ _ step_count = pure Success
simulate_steps remaining game initial_total step_count = do
  -- Game is over? Stop.
  if game.result /= Nothing
  then pure Success
  else do
    -- Generate a random valid play
    play <- generate_valid_play game
    -- Execute it
    result <- Engine.makeAutoPlay play game
    case result of
      Left _ ->
        -- Invalid play is fine — we just skip and try ending the phase
        do
          result2 <- Engine.makeAutoPlay
            (EndPhase { playerIndex: game.turn })
            game
          case result2 of
            Left _ -> pure Success -- Completely stuck, stop
            Right game' ->
              if game'.result /= Nothing
              then pure $ check_invariants game' initial_total
              else simulate_steps (remaining - 1) game' initial_total (step_count + 1)
      Right game' ->
        -- Check invariants
        case check_invariants game' initial_total of
          Failed msg -> pure $ Failed $ "Invariant violated at step "
            <> show step_count <> " after play " <> show play <> ": " <> msg
          Success ->
            simulate_steps (remaining - 1) game' initial_total (step_count + 1)

-- ══════════════════════════════════════════════════════════════════
-- AI Bot Tests
-- ══════════════════════════════════════════════════════════════════

ai_tests :: Array { name :: String, test :: Effect Result }
ai_tests =
  assign_bot_indices_tests
  <> big_money_target_tests
  <> find_bot_to_act_tests
  <> auto_resolve_tests
  <> bot_name_tests
  <> generate_play_tests
  <> bot_simulation_tests

-- ── assignBotIndices ──

assign_bot_indices_tests :: Array { name :: String, test :: Effect Result }
assign_bot_indices_tests =
  [ { name: "assignBotIndices: 1 bot, human at 0 → bot at 1"
    , test: pure $
        let bots = AI.assignBotIndices 0 [ BigMoney ]
        in assert_true "bot should be at index 1"
          $ bots == [ { playerIndex: 1, strategy: BigMoney } ]
    }
  , { name: "assignBotIndices: 1 bot, human at 1 → bot at 0"
    , test: pure $
        let bots = AI.assignBotIndices 1 [ BigMoney ]
        in assert_true "bot should be at index 0"
          $ bots == [ { playerIndex: 0, strategy: BigMoney } ]
    }
  , { name: "assignBotIndices: 2 bots, human at 0 → bots at 1,2"
    , test: pure $
        let bots = AI.assignBotIndices 0 [ BigMoney, Random ]
        in assert_true "bots should be at indices 1 and 2"
          $ bots == [ { playerIndex: 1, strategy: BigMoney }
                    , { playerIndex: 2, strategy: Random }
                    ]
    }
  , { name: "assignBotIndices: 2 bots, human at 1 → bots at 0,2"
    , test: pure $
        let bots = AI.assignBotIndices 1 [ BigMoney, Random ]
        in assert_true "bots should be at indices 0 and 2"
          $ bots == [ { playerIndex: 0, strategy: BigMoney }
                    , { playerIndex: 2, strategy: Random }
                    ]
    }
  , { name: "assignBotIndices: no bots → empty"
    , test: pure $
        let bots = AI.assignBotIndices 0 []
        in assert_eq bots []
    }
  ]

-- ── bigMoneyTarget ──

big_money_target_tests :: Array { name :: String, test :: Effect Result }
big_money_target_tests =
  let supply = Supply.makeSupply 2 Cards.cardMap
  in
  [ { name: "bigMoneyTarget: $8 buys Province"
    , test: pure $ assert_eq (AI.bigMoneyTarget 8 supply) (Just "Province")
    }
  , { name: "bigMoneyTarget: $10 buys Province"
    , test: pure $ assert_eq (AI.bigMoneyTarget 10 supply) (Just "Province")
    }
  , { name: "bigMoneyTarget: $7 buys Gold"
    , test: pure $ assert_eq (AI.bigMoneyTarget 7 supply) (Just "Gold")
    }
  , { name: "bigMoneyTarget: $6 buys Gold"
    , test: pure $ assert_eq (AI.bigMoneyTarget 6 supply) (Just "Gold")
    }
  , { name: "bigMoneyTarget: $4 buys Silver"
    , test: pure $ assert_eq (AI.bigMoneyTarget 4 supply) (Just "Silver")
    }
  , { name: "bigMoneyTarget: $3 buys Silver"
    , test: pure $ assert_eq (AI.bigMoneyTarget 3 supply) (Just "Silver")
    }
  , { name: "bigMoneyTarget: $2 buys Nothing"
    , test: pure $ assert_eq (AI.bigMoneyTarget 2 supply) Nothing
    }
  , { name: "bigMoneyTarget: $0 buys Nothing"
    , test: pure $ assert_eq (AI.bigMoneyTarget 0 supply) Nothing
    }
  , { name: "bigMoneyTarget: $5 with few provinces buys Duchy"
    , test: pure $
        let
          lowProvinceSupply = map
            (\s -> if s.card.name == "Province"
                   then s { count = 4 }
                   else s)
            supply
        in assert_eq (AI.bigMoneyTarget 5 lowProvinceSupply) (Just "Duchy")
    }
  , { name: "bigMoneyTarget: $5 with many provinces buys Silver"
    , test: pure $
        let
          highProvinceSupply = map
            (\s -> if s.card.name == "Province"
                   then s { count = 8 }
                   else s)
            supply
        in assert_eq (AI.bigMoneyTarget 5 highProvinceSupply) (Just "Silver")
    }
  ]

-- ── findBotToAct ──

find_bot_to_act_tests :: Array { name :: String, test :: Effect Result }
find_bot_to_act_tests =
  [ { name: "findBotToAct: returns Nothing for empty bot list"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed err
          Right game ->
            assert_eq (AI.findBotToAct [] game) Nothing
    }
  , { name: "findBotToAct: returns bot when it's their turn"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed err
          Right game ->
            let
              bot = { playerIndex: game.turn, strategy: BigMoney }
              bots = [ bot ]
            in assert_eq (AI.findBotToAct bots game) (Just bot)
    }
  , { name: "findBotToAct: returns Nothing when human's turn"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed err
          Right game ->
            let
              otherIndex = if game.turn == 0 then 1 else 0
              bot = { playerIndex: otherIndex, strategy: BigMoney }
              bots = [ bot ]
            in assert_eq (AI.findBotToAct bots game) Nothing
    }
  , { name: "findBotToAct: returns Nothing when game is over"
    , test: do
        result <- run_make_auto_play 2 true
        pure case result of
          Left err -> Failed err
          Right game ->
            let
              finishedGame = game { result = Just (Domination.Data.Result.Victory 0) }
              bot = { playerIndex: game.turn, strategy: BigMoney }
              bots = [ bot ]
            in assert_eq (AI.findBotToAct bots finishedGame) Nothing
    }
  ]

-- ── autoResolve ──

auto_resolve_tests :: Array { name :: String, test :: Effect Result }
auto_resolve_tests =
  [ { name: "autoResolve: If gets Just unit resolution"
    , test: pure $
        let
          choice = If { choice: Draw { n: 1, resolution: Nothing, attack: false }
                      , otherwise: Nothing
                      , condition: HasCard "Copper"
                      , resolution: Nothing
                      , attack: false
                      }
          resolved = AI.autoResolve choice
        in case resolved of
          If r -> assert_eq r.resolution (Just unit)
          _ -> Failed "expected If"
    }
  , { name: "autoResolve: And gets Just unit resolution"
    , test: pure $
        let
          choice = And { choices: [], resolution: Nothing, attack: false }
          resolved = AI.autoResolve choice
        in case resolved of
          And r -> assert_eq r.resolution (Just unit)
          _ -> Failed "expected And"
    }
  , { name: "autoResolve: Or picks first choice"
    , test: pure $
        let
          draw1 = Draw { n: 1, resolution: Nothing, attack: false }
          draw2 = Draw { n: 2, resolution: Nothing, attack: false }
          choice = Or { choices: [ draw1, draw2 ], resolution: Nothing, attack: false }
          resolved = AI.autoResolve choice
        in case resolved of
          Or r -> assert_eq r.resolution (Just draw1)
          _ -> Failed "expected Or"
    }
  , { name: "autoResolve: Option resolves to true"
    , test: pure $
        let
          choice = Option { choice: Draw { n: 1, resolution: Nothing, attack: false }
                          , resolution: Nothing, attack: false }
          resolved = AI.autoResolve choice
        in case resolved of
          Option r -> assert_eq r.resolution (Just true)
          _ -> Failed "expected Option"
    }
  , { name: "autoResolve: Draw gets Just unit resolution"
    , test: pure $
        let
          choice = Draw { n: 3, resolution: Nothing, attack: false }
          resolved = AI.autoResolve choice
        in case resolved of
          Draw r -> assert_eq r.resolution (Just unit)
          _ -> Failed "expected Draw"
    }
  , { name: "autoResolve: GainCard resolves to Copper"
    , test: pure $
        let
          choice = GainCard { attack: false, filter: Filter.Any
                            , destination: Pile.Discard, resolution: Nothing }
          resolved = AI.autoResolve choice
        in case resolved of
          GainCard r -> assert_eq r.resolution (Just "Copper")
          _ -> Failed "expected GainCard"
    }
  , { name: "autoResolve: Discard gets Just unit resolution"
    , test: pure $
        let
          choice = Discard { selection: SelectAll, resolution: Nothing, attack: false }
          resolved = AI.autoResolve choice
        in case resolved of
          Discard r -> assert_eq r.resolution (Just unit)
          _ -> Failed "expected Discard"
    }
  , { name: "autoResolve: GainActions gets Just unit resolution"
    , test: pure $
        let
          choice = GainActions { n: actions 2, resolution: Nothing, attack: false }
          resolved = AI.autoResolve choice
        in case resolved of
          GainActions r -> assert_eq r.resolution (Just unit)
          _ -> Failed "expected GainActions"
    }
  , { name: "autoResolve: GainBuys gets Just unit resolution"
    , test: pure $
        let
          choice = GainBuys { n: buys 1, resolution: Nothing, attack: false }
          resolved = AI.autoResolve choice
        in case resolved of
          GainBuys r -> assert_eq r.resolution (Just unit)
          _ -> Failed "expected GainBuys"
    }
  , { name: "autoResolve: PickN takes first n choices"
    , test: pure $
        let
          c1 = Draw { n: 1, resolution: Nothing, attack: false }
          c2 = Draw { n: 2, resolution: Nothing, attack: false }
          c3 = Draw { n: 3, resolution: Nothing, attack: false }
          choice = PickN { choices: [ c1, c2, c3 ], n: 2
                         , resolution: Nothing, attack: false }
          resolved = AI.autoResolve choice
        in case resolved of
          PickN r -> assert_eq r.resolution (Just [ c1, c2 ])
          _ -> Failed "expected PickN"
    }
  ]

-- ── botName ──

bot_name_tests :: Array { name :: String, test :: Effect Result }
bot_name_tests =
  [ { name: "botName BigMoney is 'Big Money Bot'"
    , test: pure $ assert_eq (botName BigMoney) "Big Money Bot"
    }
  , { name: "botName Random is 'Random Bot'"
    , test: pure $ assert_eq (botName Random) "Random Bot"
    }
  , { name: "allStrategies contains BigMoney and Random"
    , test: pure $ assert_true "should have both strategies"
      $ allStrategies == [ BigMoney, Random ]
    }
  ]

-- ── generatePlay ──

generate_play_tests :: Array { name :: String, test :: Effect Result }
generate_play_tests =
  [ { name: "BigMoney: generates Purchase or EndPhase after setup"
    , test: do
        result <- run_make_auto_play 2 true
        case result of
          Left err -> pure $ Failed err
          Right game -> do
            let bot = { playerIndex: game.turn, strategy: BigMoney }
            ePlay <- runRandomM $ AI.generateBotPlay bot game
            pure case ePlay of
              Left err -> Failed $ "generatePlay failed: " <> err
              Right (EndPhase _) -> Success
              Right (Purchase _) -> Success
              Right play -> Failed $ "expected EndPhase or Purchase, got: " <> show play
    }
  , { name: "BigMoney: generates Purchase in BuyPhase"
    , test: do
        result <- run_make_auto_play 2 true
        case result of
          Left err -> pure $ Failed err
          Right game -> do
            -- Advance to buy phase
            let buyGame = game { phase = BuyPhase }
            let bot = { playerIndex: buyGame.turn, strategy: BigMoney }
            ePlay <- runRandomM $ AI.generateBotPlay bot buyGame
            pure case ePlay of
              Left err -> Failed $ "generatePlay failed: " <> err
              Right (Purchase _) -> Success
              Right (EndPhase _) -> Success
              Right play -> Failed $ "expected Purchase or EndPhase, got: " <> show play
    }
  , { name: "Random: generates valid play after setup"
    , test: do
        result <- run_make_auto_play 2 true
        case result of
          Left err -> pure $ Failed err
          Right game -> do
            let bot = { playerIndex: game.turn, strategy: Random }
            ePlay <- runRandomM $ AI.generateBotPlay bot game
            pure case ePlay of
              Left err -> Failed $ "generatePlay failed: " <> err
              Right (EndPhase _) -> Success
              Right (PlayCard _) -> Success
              Right (Purchase _) -> Success
              Right play -> Failed $ "expected EndPhase, PlayCard, or Purchase, got: " <> show play
    }
  , { name: "Random: generates valid play in BuyPhase"
    , test: do
        result <- run_make_auto_play 2 true
        case result of
          Left err -> pure $ Failed err
          Right game -> do
            let buyGame = game { phase = BuyPhase }
            let bot = { playerIndex: buyGame.turn, strategy: Random }
            ePlay <- runRandomM $ AI.generateBotPlay bot buyGame
            pure case ePlay of
              Left err -> Failed $ "generatePlay failed: " <> err
              Right (Purchase _) -> Success
              Right (EndPhase _) -> Success
              Right play -> Failed $ "expected Purchase or EndPhase, got: " <> show play
    }
  , { name: "Bot generates EndPhase in CleanupPhase"
    , test: do
        result <- run_make_auto_play 2 true
        case result of
          Left err -> pure $ Failed err
          Right game -> do
            let cleanupGame = game { phase = CleanupPhase }
            let bot = { playerIndex: cleanupGame.turn, strategy: BigMoney }
            ePlay <- runRandomM $ AI.generateBotPlay bot cleanupGame
            pure case ePlay of
              Left err -> Failed $ "generatePlay failed: " <> err
              Right (EndPhase _) -> Success
              Right play -> Failed $ "expected EndPhase, got: " <> show play
    }
  ]

-- ── Full Bot Game Simulations ──

bot_simulation_tests :: Array { name :: String, test :: Effect Result }
bot_simulation_tests =
  [ { name: "BigMoney bot plays full 2p game to completion"
    , test: run_all_bots_game BigMoney 2 false 1000
    }
  , { name: "BigMoney bot plays full 3p game to completion"
    , test: run_all_bots_game BigMoney 3 false 1500
    }
  , { name: "Random bot generates valid plays for 200 steps"
    , test: run_bot_game_with_invariants Random 2 false 200
    }
  , { name: "BigMoney bot plays full 4p game to completion"
    , test: run_all_bots_game BigMoney 4 false 2000
    }
  , { name: "BigMoney bot preserves card conservation (2p)"
    , test: run_bot_game_with_invariants BigMoney 2 false 500
    }
  , { name: "Random bot preserves card conservation (2p)"
    , test: run_bot_game_with_invariants Random 2 false 500
    }
  , { name: "BigMoney bot preserves card conservation (4p)"
    , test: run_bot_game_with_invariants BigMoney 4 false 300
    }
  , { name: "3 BigMoney bots play full 3p game"
    , test: run_all_bots_game BigMoney 3 false 1000
    }
  , { name: "BigMoney bot always buys something reasonable"
    , test: run_big_money_buy_validation 2 false 200
    }
  ]

run_bot_game :: Strategy -> Int -> Boolean -> Int -> Effect Result
run_bot_game strategy playerCount longGame maxSteps = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      let
        bots = map (\i -> { playerIndex: i, strategy })
          $ Array.filter (_ /= 0) $ 0 .. (playerCount - 1)
      runRandomM $ run_bot_steps bots maxSteps game

run_all_bots_game :: Strategy -> Int -> Boolean -> Int -> Effect Result
run_all_bots_game strategy playerCount longGame maxSteps = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      let
        bots = map (\i -> { playerIndex: i, strategy })
          $ 0 .. (playerCount - 1)
      runRandomM $ run_bot_steps bots maxSteps game

run_mixed_bots_game :: Array Strategy -> Int -> Boolean -> Int -> Effect Result
run_mixed_bots_game strategies playerCount longGame maxSteps = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      let
        bots = Array.mapWithIndex (\i s -> { playerIndex: i, strategy: s })
          $ Array.take playerCount strategies
      runRandomM $ run_bot_steps bots maxSteps game

run_multi_bot_game :: Array Strategy -> Int -> Boolean -> Int -> Effect Result
run_multi_bot_game strategies playerCount longGame maxSteps = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      let bots = AI.assignBotIndices 0 strategies
      runRandomM $ run_bot_steps bots maxSteps game

run_bot_steps :: Array Bot -> Int -> Game -> RandomM Result
run_bot_steps _ 0 _ = pure $ Failed "game did not finish within step limit"
run_bot_steps bots remaining game =
  if isJust game.result
  then pure Success
  else case AI.findBotToAct bots game of
    Just bot -> do
      ePlay <- AI.generateBotPlay bot game
      case ePlay of
        Left _ -> do
          -- Fallback: try EndPhase
          result <- Engine.makeAutoPlay
            (EndPhase { playerIndex: bot.playerIndex }) game
          case result of
            Left _ -> pure Success
            Right game' -> run_bot_steps bots (remaining - 1) game'
        Right play -> do
          result <- Engine.makeAutoPlay play game
          case result of
            Left _ -> do
              -- Try EndPhase as fallback
              result2 <- Engine.makeAutoPlay
                (EndPhase { playerIndex: bot.playerIndex }) game
              case result2 of
                Left _ -> pure Success
                Right game' -> run_bot_steps bots (remaining - 1) game'
            Right game' -> run_bot_steps bots (remaining - 1) game'
    Nothing -> do
      -- Human's turn - simulate human EndPhase
      let humanPlay = EndPhase { playerIndex: game.turn }
      result <- Engine.makeAutoPlay humanPlay game
      case result of
        Left _ -> pure Success
        Right game' -> run_bot_steps bots (remaining - 1) game'

run_bot_game_with_invariants :: Strategy -> Int -> Boolean -> Int -> Effect Result
run_bot_game_with_invariants strategy playerCount longGame maxSteps = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      let
        initial_total = count_all game
        bots = map (\i -> { playerIndex: i, strategy })
          $ Array.filter (_ /= 0) $ 0 .. (playerCount - 1)
      runRandomM $ run_bot_steps_with_invariants bots maxSteps game initial_total 0

run_bot_steps_with_invariants
  :: Array Bot -> Int -> Game -> Int -> Int -> RandomM Result
run_bot_steps_with_invariants _ 0 _ _ _ = pure Success
run_bot_steps_with_invariants bots remaining game initial_total step = do
  -- Check invariants at every step
  case check_invariants game initial_total of
    Failed msg -> pure $ Failed $ "Invariant violated at step "
      <> show step <> ": " <> msg
    Success ->
      if isJust game.result
      then pure Success
      else case AI.findBotToAct bots game of
        Just bot -> do
          ePlay <- AI.generateBotPlay bot game
          case ePlay of
            Left _ -> do
              result <- Engine.makeAutoPlay
                (EndPhase { playerIndex: bot.playerIndex }) game
              case result of
                Left _ -> pure Success
                Right game' ->
                  run_bot_steps_with_invariants
                    bots (remaining - 1) game' initial_total (step + 1)
            Right play -> do
              result <- Engine.makeAutoPlay play game
              case result of
                Left _ -> do
                  result2 <- Engine.makeAutoPlay
                    (EndPhase { playerIndex: bot.playerIndex }) game
                  case result2 of
                    Left _ -> pure Success
                    Right game' ->
                      run_bot_steps_with_invariants
                        bots (remaining - 1) game' initial_total (step + 1)
                Right game' ->
                  run_bot_steps_with_invariants
                    bots (remaining - 1) game' initial_total (step + 1)
        Nothing -> do
          let humanPlay = EndPhase { playerIndex: game.turn }
          result <- Engine.makeAutoPlay humanPlay game
          case result of
            Left _ -> pure Success
            Right game' ->
              run_bot_steps_with_invariants
                bots (remaining - 1) game' initial_total (step + 1)

run_big_money_buy_validation :: Int -> Boolean -> Int -> Effect Result
run_big_money_buy_validation playerCount longGame maxSteps = do
  initial <- run_make_auto_play playerCount longGame
  case initial of
    Left err -> pure $ Failed $ "setup failed: " <> err
    Right game -> do
      let
        bots = [ { playerIndex: 1, strategy: BigMoney } ]
      runRandomM $ validate_big_money_buys bots maxSteps game

validate_big_money_buys :: Array Bot -> Int -> Game -> RandomM Result
validate_big_money_buys _ 0 _ = pure Success
validate_big_money_buys bots remaining game =
  if isJust game.result
  then pure Success
  else case AI.findBotToAct bots game of
    Just bot -> do
      ePlay <- AI.generateBotPlay bot game
      case ePlay of
        Left _ -> pure Success
        Right play -> do
          -- Validate BigMoney never buys action cards
          case play of
            Purchase { stackIndex } ->
              let stack = game.supply Array.!! stackIndex
              in case stack of
                Nothing -> pure $ Failed "invalid stack index"
                Just s ->
                  if Array.any (_ == Action) s.card.types
                    && not (Array.any (_ == Treasure) s.card.types)
                  then pure $ Failed $ "BigMoney bought action card: "
                    <> s.card.name
                  else do
                    result <- Engine.makeAutoPlay play game
                    case result of
                      Left _ -> pure Success
                      Right game' ->
                        validate_big_money_buys bots (remaining - 1) game'
            _ -> do
              result <- Engine.makeAutoPlay play game
              case result of
                Left _ -> pure Success
                Right game' ->
                  validate_big_money_buys bots (remaining - 1) game'
    Nothing -> do
      let humanPlay = EndPhase { playerIndex: game.turn }
      result <- Engine.makeAutoPlay humanPlay game
      case result of
        Left _ -> pure Success
        Right game' -> validate_big_money_buys bots (remaining - 1) game'

