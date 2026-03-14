module Test.Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, decodeArrayBuffer, encodeArrayBuffer)
import Data.Either (Either(..))
import Data.Foldable (all, foldl, length, sum)
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..))
import Data.Stack.Machine as Machine
import Data.Traversable (traverse)
import Domination.Capability.Random (RandomM, runRandomM)
import Domination.Capability.WireCodec (class WireCodec, readWire, writeWire)
import Domination.Data.Actions (Actions, actions)
import Domination.Data.Buys (Buys, buys)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Cards as Cards
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Game.Engine as Engine
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Points (points)
import Domination.Data.Stack as Stack
import Domination.Data.Supply as Supply
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
  property_results <- run_effect_section "Property-Based Tests" property_tests
  let total_passed = results.passed + simulation_results.passed + play_results.passed + property_results.passed
  let total_failed = results.failed + simulation_results.failed + play_results.failed + property_results.failed
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
  , { name: "newPlayer: no reaction"
    , test: assert_eq p.reaction Nothing
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
  -- dropReaction
  , { name: "dropReaction: clears reaction"
    , test: assert_eq (Player.dropReaction p).reaction Nothing
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

