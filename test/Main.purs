{- |
This module contains test cases for a card game's network functionality, focusing on data serialization.

Key Testing Areas:
1. Binary serialization for network transmission
2. Game state consistency checks
3. Automated gameplay validation
4. Data conversion integrity

Technical Concepts:
* Effect: Represents side-effecting computations (IO, networking, etc).
  Learn more: https://pursuit.purescript.org/packages/purescript-effect
* Type Classes:
  - Eq: Value equality comparison
  - EncodeArrayBuffer/DecodeArrayBuffer: Binary format conversion
  - WireCodec: Network protocol serialization

Key concepts for non-functional programmers:
* Effect: Represents computations that interact with the outside world (like logging or network calls).
  Similar to Promise/async in JavaScript.

* Type Classes (indicated by 'class' in type signatures):
  - Eq: Allows comparing values for equality (like implementing .equals() in Java)
  - EncodeArrayBuffer/DecodeArrayBuffer: For converting data to/from binary format
  - WireCodec: For network transmission serialization

* Functional Programming Patterns:
  - Isomorphism: A pair of functions that can convert between two types without losing information
    (like a perfect two-way conversion between JSON and an object)
  - Lens: A way to access and modify nested data structures (similar to lodash.get/set)
  - traverse: Like map/forEach but handles nested effects properly
  - Maybe: Represents optional values (like Optional in Java/Swift)
  - Either: Represents success/failure results (Left = error, Right = success)

Testing Strategy:
Tests verify that game state can be:
1. Serialized to binary format
2. Transmitted over network
3. Deserialized without data loss
4. Maintain game rule consistency
-}

module Test.Main where

import Prelude

import Data.Array ((..))
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, decodeArrayBuffer, encodeArrayBuffer)
import Data.Either (Either(..))
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..))
import Data.Stack.Machine as Machine
import Data.Traversable (traverse_)
import Domination.Capability.Random (runRandomM)
import Domination.Capability.WireCodec (class WireCodec, readWire, writeWire)
import Domination.Data.Cards as Cards
import Domination.Data.Game as Game
import Domination.Data.Game.Engine (makeAutoPlay) as Dom
import Domination.Data.Play (Play(..))
import Domination.Data.Wire.Game (_toWire) as Dom
import Domination.Data.Wire.Play (_toWire) as Play
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (Result(..), assertEquals)

-- | Entry point for the test suite
main :: Effect Unit
main = do
  run_test test_expected_stack_machine_value
  traverse_ run_test test_cases

-- | Verifies that stack machine produces expected value
test_expected_stack_machine_value :: { name :: String, test :: Result }
test_expected_stack_machine_value =
  { name: "Stack machine should output 3"
  , test: assertEquals (Machine.exampleStackMachineComputation 1) 3
  }

-- | Runs a single test and logs its results
run_test :: { name :: String, test :: Result } -> Effect Unit
run_test test_case = do
  log $ "Testing: " <> test_case.name
  log $ show test_case.test

-- | Test suite covering core game functionality
test_cases :: Array { name :: String, test :: Result }
test_cases =
  (map
    (\player_count ->
      { name: "Wire serialization roundtrip with " <> show player_count <> " player game"
      , test: test_game_wire_serialization player_count
      })
    (1 .. 10))
  <>
  [ { name: "Game wire format isomorphism"
    , test: test_isomorphism Dom._toWire (Game.new 2 Cards.cardMap true)
    }
  , { name: "Play wire format isomorphism"
    , test: test_isomorphism Play._toWire (NewGame { playerCount: 2, supply: Cards.cardMap, longGame: true })
    }
  ]

-- | Verifies binary serialization integrity
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
  pure $ assertEquals deserialized_value $ Just original_value

-- | Tests network transmission safety by simulating a complete
-- | wire format serialization cycle. Essential for verifying that
-- | game state can be safely shared between players.
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
        Right received_value -> pure $ assertEquals received_value value_to_transmit

-- | Validates game state consistency during network transmission
test_game_wire_serialization :: Int -> Result
test_game_wire_serialization player_count =
  let initial_game = Game.new player_count Cards.cardMap true
      serialized = view Dom._toWire initial_game
      deserialized = review Dom._toWire serialized
  in assertEquals deserialized initial_game

-- | Comprehensive game turn simulation
example_game_simulation :: Effect Result
example_game_simulation = do
  let initial_game = Game.new 1 Cards.cardMap true
  game_state_after_turn <- runRandomM
    $ Dom.makeAutoPlay
      (NewGame { playerCount: 1, supply: Cards.cardMap, longGame: true })
      initial_game
  pure case game_state_after_turn of
    Left error -> Failed $ "Auto play failed: " <> show error
    Right final_state -> assertEquals final_state final_state

-- | Tests data conversion integrity using lenses
test_isomorphism
  :: forall a b
  . Eq a
  => Show a
  => (Iso' a b)
  -> a
  -> Result
test_isomorphism conversion_lens original_value =
  let converted = view conversion_lens original_value
      restored = review conversion_lens converted
  in assertEquals restored original_value

