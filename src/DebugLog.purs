module DebugLog
  ( BugReport
  , LogExcerpt
  , clear
  , currentReport
  , install
  , renderReport
  , showVersion
  , wholeLog
  ) where

import Prelude

import Data.Array (length)
import Data.String (joinWith)
import Effect (Effect)
import Version (Version(..), version)

-- | The newest lines of a log, and how many older lines were left out.
type LogExcerpt = { lines :: Array String, omitted :: Int }

type BugReport =
  { version :: Version
  , userAgent :: String
  , generatedAt :: String
  , facts :: Array { label :: String, value :: String }
  , journal :: LogExcerpt
  , entries :: LogExcerpt
  }

foreign import install :: Effect Unit

foreign import snapshot :: Effect (Array String)

foreign import journal :: Effect (Array String)

foreign import pageFacts :: Effect (Array { label :: String, value :: String })

foreign import clear :: Effect Unit

foreign import userAgent :: Effect String

foreign import nowIso :: Effect String

wholeLog :: Array String -> LogExcerpt
wholeLog lines = { lines, omitted: 0 }

currentReport :: Effect BugReport
currentReport = { version, userAgent: _, generatedAt: _, facts: _, journal: _, entries: _ }
  <$> userAgent
  <*> nowIso
  <*> pageFacts
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
