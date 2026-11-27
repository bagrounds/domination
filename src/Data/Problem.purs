-- | Faults: things Domination promised to do and couldn't, so the game may
-- | now be wrong or incomplete. Refusals (moves the rules don't allow yet),
-- | expected absences (no saved game yet) and temporary conditions (an
-- | unreachable server) are never problems, so they never open the dialog.
module Domination.Data.Problem
  ( Problem(..)
  , ProblemKind(..)
  , allProblemKinds
  , consequence
  , headline
  , loadProblem
  , maxMessageLength
  , message
  , problemKind
  , saveProblem
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Domination.Data.StorageError (LoadError(..), SaveError(..), printLoadError, printSaveError)

data Problem
  = StorageFull { detail :: String }
  | StorageUnavailable { detail :: String }
  | SavedGameUnreadable { detail :: String }
  | UndoHistoryLost { step :: Int }
  | BotStuck { bot :: String, detail :: String }
  | MoveNotSent { detail :: String }
  | MoveNotReceived { detail :: String }
  | MailboxGameUnreadable { detail :: String }
  | RoomNotCreated
  | Unexpected { detail :: String }

derive instance eqProblem :: Eq Problem
derive instance genericProblem :: Generic Problem _
instance showProblem :: Show Problem where
  show = genericShow

data ProblemKind
  = Storage
  | SavedGame
  | UndoHistory
  | Bot
  | Sending
  | Receiving
  | Mailbox
  | Rooms
  | Script

derive instance eqProblemKind :: Eq ProblemKind
derive instance ordProblemKind :: Ord ProblemKind
derive instance genericProblemKind :: Generic ProblemKind _
instance showProblemKind :: Show ProblemKind where
  show = genericShow

allProblemKinds :: Array ProblemKind
allProblemKinds = [ Storage, SavedGame, UndoHistory, Bot, Sending, Receiving, Mailbox, Rooms, Script ]

problemKind :: Problem -> ProblemKind
problemKind = case _ of
  StorageFull _ -> Storage
  StorageUnavailable _ -> Storage
  SavedGameUnreadable _ -> SavedGame
  UndoHistoryLost _ -> UndoHistory
  BotStuck _ -> Bot
  MoveNotSent _ -> Sending
  MoveNotReceived _ -> Receiving
  MailboxGameUnreadable _ -> Mailbox
  RoomNotCreated -> Rooms
  Unexpected _ -> Script

-- | A skipped save after a clear is no fault: the page reloads instead.
saveProblem :: SaveError -> Maybe Problem
saveProblem error = case error of
  QuotaExceeded _ -> Just $ StorageFull { detail: printSaveError error }
  SaveRejected _ -> Just $ StorageUnavailable { detail: printSaveError error }
  Superseded _ -> Nothing

-- | A saved game that isn't there yet is expected, not a problem.
loadProblem :: LoadError -> Maybe Problem
loadProblem error = case error of
  Absent _ -> Nothing
  Unreadable _ -> Just $ SavedGameUnreadable { detail: printLoadError error }
  LoadUnavailable _ -> Just $ StorageUnavailable { detail: printLoadError error }

headline :: Problem -> String
headline = case _ of
  StorageFull _ -> "Domination couldn't save: this browser's storage is full"
  StorageUnavailable _ -> "Domination couldn't use this browser's storage"
  SavedGameUnreadable _ -> "Domination couldn't read a saved game"
  UndoHistoryLost _ -> "Undo couldn't go back"
  BotStuck { bot } -> bot <> " couldn't play"
  MoveNotSent _ -> "Domination couldn't send to the other players"
  MoveNotReceived _ -> "Domination couldn't read a move from another player"
  MailboxGameUnreadable _ -> "Domination couldn't read the room's saved game"
  RoomNotCreated -> "Domination couldn't create a room"
  Unexpected _ -> "Domination hit an unexpected error"

consequence :: Problem -> String
consequence = case _ of
  StorageFull _ -> "What you just did wasn't saved, so Undo and reloading the page may not bring it back."
  StorageUnavailable _ -> "Your game may not be saved, so Undo and reloading the page may not bring it back."
  SavedGameUnreadable _ -> "That saved game wasn't loaded."
  UndoHistoryLost { step } -> "Saving step " <> show step <> " failed earlier, so Undo can't return to it. The game wasn't changed."
  BotStuck { bot } -> "The game may be stuck waiting for " <> bot <> "."
  MoveNotSent _ -> "The other players may not see your latest move or message."
  MoveNotReceived _ -> "A move was dropped, so your game may now differ from theirs."
  MailboxGameUnreadable _ -> "This device may not have caught up with the room."
  RoomNotCreated -> "No room was created. Try again."
  Unexpected _ -> "Something may not work until you reload the page."

-- | The exact error text, shortened so the dialog and the bug form stay small.
message :: Problem -> String
message = String.take maxMessageLength <<< case _ of
  StorageFull { detail } -> detail
  StorageUnavailable { detail } -> detail
  SavedGameUnreadable { detail } -> detail
  UndoHistoryLost { step } -> "Undo: the save of step " <> show step <> " failed, so it cannot be restored"
  BotStuck { bot, detail } -> "Bot error (" <> bot <> "): " <> detail
  MoveNotSent { detail } -> "Could not encode a message to send: " <> detail
  MoveNotReceived { detail } -> "problem receiving message: " <> detail
  MailboxGameUnreadable { detail } -> "Mailbox: " <> detail
  RoomNotCreated -> "CreateRoom: failed to generate a room code"
  Unexpected { detail } -> detail

maxMessageLength :: Int
maxMessageLength = 500
