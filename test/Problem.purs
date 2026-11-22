module Test.Problem (problem_tests, problemGen) where

import Prelude

import Data.Array (all, elem, foldl, length, nub, replicate)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String as String
import Domination.Data.Notices (Notices, dismiss, noNotices, notice, showing, summary)
import Domination.Data.Problem (Problem(..), ProblemKind(..), allProblemKinds, consequence, headline, loadProblem, maxMessageLength, message, problemKind, saveProblem)
import Domination.Data.StorageError (LoadError(..), SaveError(..), quotaErrorNames, saveError)
import Domination.Data.Undo (FailedSaves, UndoOutcome(..), historySlot, historySlots, recordSaveAttempt, undoOutcome)
import Effect (Effect)
import Test.QuickCheck (Result, assertEquals, (<?>))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf, vectorOf)
import Test.Room (all_success, quick_check)

type EffectTest = { name :: String, test :: Effect Result }

problem_tests :: Array EffectTest
problem_tests =
  [ { name: "every kind of problem has a problem of that kind"
    , test: pure $ assertEquals
        (Set.fromFoldable $ problemKind <$> everyProblem)
        (Set.fromFoldable allProblemKinds)
    }
  , { name: "∀ problem: it has a headline and a consequence, and its message fits the dialog"
    , test: quick_check 300 do
        problem <- problemGen
        pure $ (headline problem /= "" && consequence problem /= ""
            && String.length (message problem) <= maxMessageLength)
          <?> show problem
    }
  , { name: "∀ browser failure: a full storage is told from any other refusal"
    , test: quick_check 300 do
        name <- oneOf $ NEA.cons' (elements $ NEA.cons' "QuotaExceededError" quotaErrorNames) [ textGen 30 ]
        text <- textGen 400
        let saved = saveError { key: "game_state", chars: 10 } { name, message: text }
        pure $ case saved of
          QuotaExceeded _ -> (name `elem` quotaErrorNames) <?> "quota for " <> show name
          SaveRejected _ -> not (name `elem` quotaErrorNames) <?> "rejected for " <> show name
    }
  , { name: "∀ save error: it is a storage problem whose message names the key, never the value"
    , test: quick_check 300 do
        key <- textGen 20
        name <- elements $ NEA.cons' "SecurityError" quotaErrorNames
        text <- textGen 2000
        let problem = saveProblem $ saveError { key, chars: 123456 } { name, message: text }
        pure $ all_success
          [ assertEquals (problemKind problem) Storage
          , String.contains (String.Pattern $ "key: '" <> key <> "' (123456 chars)") (message problem)
              <?> message problem
          , (String.length (message problem) <= String.length key + 300) <?> message problem
          ]
    }
  , { name: "a missing saved game is expected; a broken or blocked one is a problem"
    , test: pure $ all_success
        [ assertEquals (loadProblem $ Absent { key: "game_state" }) Nothing
        , assertEquals (problemKind <$> loadProblem (Unreadable { key: "game_state", detail: "bad json" }))
            (Just SavedGame)
        , assertEquals (problemKind <$> loadProblem (LoadUnavailable { key: "game_state", detail: "SecurityError" }))
            (Just Storage)
        ]
    }
  , { name: "∀ problems: the dialog shows the first problem of each kind, in order"
    , test: quick_check 300 do
        problems <- arrayOf problemGen
        let
          notices = noticeAll problems
          kinds = problemKind <$> problems
        pure $ all_success
          [ assertEquals (problemKind <<< _.problem <$> notices.queue) (nub kinds)
          , assertEquals (map _.problem $ showing notices) (Array.head problems)
          ]
    }
  , { name: "∀ problems: each shown problem counts the later problems of its kind"
    , test: quick_check 300 do
        problems <- arrayOf problemGen
        let
          notices = noticeAll problems
          countOf kind = length $ Array.filter ((_ == kind) <<< problemKind) problems
        pure $ all (\entry -> entry.repeats + 1 == countOf (problemKind entry.problem)) notices.queue
          <?> show (map _.repeats notices.queue)
    }
  , { name: "∀ problems: dismissing silences that kind and shows the next one"
    , test: quick_check 300 do
        problems <- arrayOf problemGen
        later <- arrayOf problemGen
        let
          notices = noticeAll problems
          dismissed = dismiss notices
          shownKind = problemKind <<< _.problem <$> showing notices
          afterLater = foldl (flip notice) dismissed later
        pure $ case shownKind of
          Nothing -> assertEquals (showing dismissed) Nothing
          Just kind -> all_success
            [ assertEquals (length dismissed.queue) (length notices.queue - 1)
            , (not $ elem kind $ problemKind <<< _.problem <$> afterLater.queue)
                <?> "the dismissed kind came back"
            ]
    }
  , { name: "∀ problems: dismissing every problem closes the dialog"
    , test: quick_check 200 do
        problems <- arrayOf problemGen
        let
          notices = noticeAll problems
          closed = foldl (\n _ -> dismiss n) notices notices.queue
        pure $ assertEquals (isJust $ showing closed) false
    }
  , { name: "the summary counts repeats and waiting problems in words"
    , test: pure $ all_success
        [ assertEquals (summary { problem: RoomNotCreated, repeats: 0, waiting: 0 }) Nothing
        , assertEquals (summary { problem: RoomNotCreated, repeats: 1, waiting: 0 }) (Just "Happened 1 more time")
        , assertEquals (summary { problem: RoomNotCreated, repeats: 2, waiting: 1 })
            (Just "Happened 2 more times · 1 other problem waiting")
        , assertEquals (summary { problem: RoomNotCreated, repeats: 0, waiting: 3 }) (Just "3 other problems waiting")
        ]
    }
  , { name: "∀ step: the history slot is in the ring, and steps a ring apart share it"
    , test: quick_check 300 do
        step <- chooseInt (-1000) 1000
        pure $ all_success
          [ (historySlot step >= 0 && historySlot step < historySlots) <?> show (historySlot step)
          , assertEquals (historySlot (step + historySlots)) (historySlot step)
          ]
    }
  , { name: "Undo with no earlier step is refused"
    , test: quick_check 100 do
        i <- chooseInt (-5) 0
        loaded <- loadedGen i
        pure $ assertEquals (outcome { i, failedSaves: Map.empty } loaded) NothingToUndo
    }
  , { name: "∀ step: Undo restores the step before when it was saved"
    , test: quick_check 300 do
        i <- chooseInt 1 1000
        pure $ assertEquals
          (outcome { i, failedSaves: Map.empty } (Right { i: i - 1 }))
          (Restore { i: i - 1 })
    }
  , { name: "∀ step: Undo to a step outside this device's history is refused"
    , test: quick_check 300 do
        i <- chooseInt 1 1000
        older <- chooseInt 1 5
        let stale = outcome { i, failedSaves: Map.empty } (Right { i: i - 1 - older * historySlots })
        let absent = outcome { i, failedSaves: Map.empty } (Left $ Absent { key: "game_state_0" })
        pure $ all_success [ assertEquals stale OutsideHistory, assertEquals absent OutsideHistory ]
    }
  , { name: "∀ step whose save failed: Undo reports the lost history, whatever the slot holds"
    , test: quick_check 300 do
        i <- chooseInt 1 1000
        loaded <- loadedGen (i - 1)
        let failedSaves = recordSaveAttempt (i - 1) (Left "QuotaExceededError" :: Either String Unit) Map.empty
        pure $ assertEquals (outcome { i, failedSaves } loaded) (HistoryLost (i - 1))
    }
  , { name: "∀ step: a later successful save of the slot restores Undo"
    , test: quick_check 300 do
        i <- chooseInt 1 1000
        let
          failed = recordSaveAttempt (i - 1) (Left "QuotaExceededError" :: Either String Unit) Map.empty
          resaved = recordSaveAttempt (i - 1) (Right unit :: Either String Unit) failed
        pure $ assertEquals
          (outcome { i, failedSaves: resaved } (Right { i: i - 1 }))
          (Restore { i: i - 1 })
    }
  , { name: "∀ step: a failed save of a step a ring apart doesn't block Undo"
    , test: quick_check 300 do
        i <- chooseInt 1 1000
        let failedSaves = recordSaveAttempt (i - 1 - historySlots) (Left "x" :: Either String Unit) Map.empty
        pure $ assertEquals
          (outcome { i, failedSaves } (Right { i: i - 1 }))
          (Restore { i: i - 1 })
    }
  , { name: "Undo from a broken or blocked history slot is a problem to report"
    , test: pure do
        let unreadable = Unreadable { key: "game_state_3", detail: "bad json" }
        assertEquals
          (outcome { i: 4, failedSaves: Map.empty } (Left unreadable))
          (HistoryUnreadable unreadable)
    }
  ]

outcome :: { i :: Int, failedSaves :: FailedSaves } -> Either LoadError { i :: Int } -> UndoOutcome { i :: Int }
outcome = undoOutcome

noticeAll :: Array Problem -> Notices
noticeAll = foldl (flip notice) noNotices

loadedGen :: Int -> Gen (Either LoadError { i :: Int })
loadedGen i = oneOf $ NEA.cons'
  (pure $ Right { i })
  [ pure $ Left $ Absent { key: "game_state" }
  , (\detail -> Left $ Unreadable { key: "game_state", detail }) <$> textGen 20
  , (Right <<< { i: _ }) <$> chooseInt (-100) 100
  ]

everyProblem :: Array Problem
everyProblem =
  [ StorageFull { detail: "" }
  , StorageUnavailable { detail: "" }
  , SavedGameUnreadable { detail: "" }
  , UndoHistoryLost { step: 3 }
  , BotStuck { bot: "Big Money Bot (Player 2)", detail: "" }
  , MoveNotSent { detail: "" }
  , MoveNotReceived { detail: "" }
  , MailboxGameUnreadable { detail: "" }
  , RoomNotCreated
  , Unexpected { detail: "" }
  ]

problemGen :: Gen Problem
problemGen = do
  detail <- textGen 1200
  step <- chooseInt 0 1000
  bot <- textGen 10
  elements $ NEA.cons' (StorageFull { detail })
    [ StorageUnavailable { detail }
    , SavedGameUnreadable { detail }
    , UndoHistoryLost { step }
    , BotStuck { bot, detail }
    , MoveNotSent { detail }
    , MoveNotReceived { detail }
    , MailboxGameUnreadable { detail }
    , RoomNotCreated
    , Unexpected { detail }
    ]

textGen :: Int -> Gen String
textGen maxPieces = do
  count <- chooseInt 0 maxPieces
  fold <$> vectorOf count (elements pieces)
  where
    pieces = NEA.cons' "a"
      [ "Z", "7", " ", "'", ":", "&", "=", "\n", "é", "中", "🃏", fold (replicate 8 "x") ]
