-- | Undo restores the game from a ring of saved steps. Pressing Undo too
-- | early or too far back is a refusal; a step that should be there but
-- | whose save failed on this page is a fault.
module Domination.Data.Undo
  ( FailedSaves
  , UndoOutcome(..)
  , historySlot
  , historySlots
  , recordSaveAttempt
  , undoOutcome
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Domination.Data.StorageError (LoadError(..))

historySlots :: Int
historySlots = 10

historySlot :: Int -> Int
historySlot step = step `mod` historySlots

-- | For each history slot whose latest save failed, the step it was saving.
type FailedSaves = Map Int Int

recordSaveAttempt :: forall e. Int -> Either e Unit -> FailedSaves -> FailedSaves
recordSaveAttempt step = either
  (const $ Map.insert (historySlot step) step)
  (const $ Map.delete $ historySlot step)

data UndoOutcome a
  = Restore a
  | NothingToUndo
  | OutsideHistory
  | HistoryLost Int
  | HistoryUnreadable LoadError

derive instance eqUndoOutcome :: Eq a => Eq (UndoOutcome a)

instance showUndoOutcome :: Show a => Show (UndoOutcome a) where
  show = case _ of
    Restore a -> "(Restore " <> show a <> ")"
    NothingToUndo -> "NothingToUndo"
    OutsideHistory -> "OutsideHistory"
    HistoryLost step -> "(HistoryLost " <> show step <> ")"
    HistoryUnreadable e -> "(HistoryUnreadable " <> show e <> ")"

-- | What pressing Undo at step `i` does, given what was loaded from the
-- | history slot of step `i - 1`.
undoOutcome
  :: forall r
  . { i :: Int, failedSaves :: FailedSaves }
  -> Either LoadError { i :: Int | r }
  -> UndoOutcome { i :: Int | r }
undoOutcome { i, failedSaves } loaded = outcomeFor (i - 1)
  where
    outcomeFor target
      | target < 0 = NothingToUndo
      | Map.lookup (historySlot target) failedSaves == Just target = HistoryLost target
      | otherwise = case loaded of
          Left (Absent _) -> OutsideHistory
          Left e -> HistoryUnreadable e
          Right saved
            | saved.i == target -> Restore saved
            | otherwise -> OutsideHistory
