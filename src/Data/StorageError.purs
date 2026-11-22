-- | Why a value could not be saved to or loaded from the browser's storage,
-- | kept precise enough to tell a missing value from a broken one and a full
-- | storage from any other refusal.
module Domination.Data.StorageError
  ( BrowserFailure
  , LoadError(..)
  , SaveError(..)
  , SaveFailure
  , describeBrowserFailure
  , maxBrowserErrorLength
  , printLoadError
  , printSaveError
  , quotaErrorNames
  , saveError
  ) where

import Prelude

import Data.Array (elem)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as String

-- | An exception thrown by the Web Storage API, by name and message.
type BrowserFailure = { name :: String, message :: String }

type SaveFailure = { key :: String, chars :: Int, detail :: String }

data SaveError
  = QuotaExceeded SaveFailure
  | SaveRejected SaveFailure

data LoadError
  = Absent { key :: String }
  | Unreadable { key :: String, detail :: String }
  | LoadUnavailable { key :: String, detail :: String }

derive instance eqSaveError :: Eq SaveError
derive instance genericSaveError :: Generic SaveError _
instance showSaveError :: Show SaveError where
  show = genericShow

derive instance eqLoadError :: Eq LoadError
derive instance genericLoadError :: Generic LoadError _
instance showLoadError :: Show LoadError where
  show = genericShow

-- | The names browsers give the exception for a full storage; Firefox used
-- | its own name before adopting the standard one.
quotaErrorNames :: Array String
quotaErrorNames = [ "QuotaExceededError", "NS_ERROR_DOM_QUOTA_REACHED" ]

saveError :: { key :: String, chars :: Int } -> BrowserFailure -> SaveError
saveError { key, chars } failure = constructor { key, chars, detail: describeBrowserFailure failure }
  where
    constructor
      | failure.name `elem` quotaErrorNames = QuotaExceeded
      | otherwise = SaveRejected

describeBrowserFailure :: BrowserFailure -> String
describeBrowserFailure { name, message } = String.take maxBrowserErrorLength $ name <> ": " <> message

maxBrowserErrorLength :: Int
maxBrowserErrorLength = 200

-- | Describes a failed save by its key and size, never its value: the value
-- | can be megabytes, and an error that carries it grows with every retry.
printSaveError :: SaveError -> String
printSaveError = case _ of
  QuotaExceeded failure -> printSaveFailure failure
  SaveRejected failure -> printSaveFailure failure

printSaveFailure :: SaveFailure -> String
printSaveFailure { key, chars, detail } = "Error saving to local storage."
  <> " key: '" <> key <> "'"
  <> " (" <> show chars <> " chars)"
  <> " error: '" <> String.take maxBrowserErrorLength detail <> "'"

printLoadError :: LoadError -> String
printLoadError = case _ of
  Absent { key } -> "Error loading item from storage at key: '" <> key <> "'"
  Unreadable { key, detail } -> "Error reading item from storage at key: '" <> key <> "' error: '" <> detail <> "'"
  LoadUnavailable { key, detail } -> "Error accessing local storage at key: '" <> key <> "' error: '" <> detail <> "'"
