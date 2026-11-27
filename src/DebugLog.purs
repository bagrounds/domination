module DebugLog
  ( BugReport
  , Fact
  , LogExcerpt
  , clear
  , consoleLogKey
  , currentReport
  , forget
  , install
  , problemEvent
  , renderReport
  , showVersion
  , syncJournalKey
  , wholeLog
  ) where

import Prelude

import Data.Array (length)
import Data.String (joinWith)
import Effect (Effect)
import Version (Version(..), version)

-- | The newest lines of a log, and how many older lines were left out.
type LogExcerpt = { lines :: Array String, omitted :: Int }

type Fact = { label :: String, value :: String }

type BugReport =
  { version :: Version
  , userAgent :: String
  , generatedAt :: String
  , facts :: Array Fact
  , journal :: LogExcerpt
  , entries :: LogExcerpt
  }

-- | Starts recording the console and page lifecycle. The logs stop saving
-- | once `stillCurrent` is false: the Storage panel deleted data this page
-- | holds, and the page is about to reload.
foreign import install :: Effect Boolean -> Effect Unit

-- | The localStorage keys of the console log and the sync journal.
foreign import consoleLogKey :: String

foreign import syncJournalKey :: String

-- | Empties the log stored at a key, so removing the key isn't undone by the
-- | next save; other keys are ignored.
foreign import forget :: String -> Effect Unit

-- | The window event announcing an uncaught error from Domination's own
-- | script; its detail is the error text.
foreign import problemEvent :: String

foreign import snapshot :: Effect (Array String)

foreign import journal :: Effect (Array String)

foreign import pageFacts :: Effect (Array Fact)

foreign import clear :: Effect Unit

foreign import userAgent :: Effect String

foreign import nowIso :: Effect String

wholeLog :: Array String -> LogExcerpt
wholeLog lines = { lines, omitted: 0 }

-- | The report as of now, with `extraFacts` after the page facts.
currentReport :: Array Fact -> Effect BugReport
currentReport extraFacts = { version, userAgent: _, generatedAt: _, facts: _, journal: _, entries: _ }
  <$> userAgent
  <*> nowIso
  <*> ((_ <> extraFacts) <$> pageFacts)
  <*> (wholeLog <$> journal)
  <*> (wholeLog <$> snapshot)

renderReport :: BugReport -> String
renderReport report = joinWith "\n" $
  [ "Domination bug report"
  , "Version: " <> showVersion report.version
  , "Generated: " <> report.generatedAt
  , "User agent: " <> report.userAgent
  ]
  <> map (\{ label, value } -> label <> ": " <> value) report.facts
  <> renderLog "Sync journal" report.journal
  <> renderLog "Console" report.entries

renderLog :: String -> LogExcerpt -> Array String
renderLog title { lines, omitted } = [ "", heading ] <> lines
  where
    total = length lines + omitted
    heading
      | omitted > 0 = title <> ": " <> show total <> ", oldest " <> show omitted <> " omitted"
      | otherwise = title <> ": " <> show total

showVersion :: Version -> String
showVersion (Version major minor patch) =
  "v" <> show major <> "." <> show minor <> "." <> show patch
