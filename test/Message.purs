module Test.Message (message_tests) where

import Prelude

import Data.Array ((..), foldl, length, reverse, take)
import Effect (Effect)
import Message (RemoteMessage(..), rememberMessage, retainRecentMessages)
import Test.QuickCheck (Result(..))

type EffectTest = { name :: String, test :: Effect Result }

message_tests :: Array EffectTest
message_tests =
  [ { name: "message history keeps only the newest 250 entries"
    , test: pure $
        let
          messages = map
            (\n -> ChatMessage { username: "", message: "", chatNumber: n })
            (1 .. 300)
          history = foldl (flip rememberMessage) [] messages
        in
          check
            (length history == 250
              && history == take 250 (reverse messages)
              && retainRecentMessages messages == take 250 messages)
            "message history was not bounded to its newest entries"
    }
  ]

check :: Boolean -> String -> Result
check true _ = Success
check false message = Failed message
