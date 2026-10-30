module DebugLog
  ( BugReport
  , clear
  , currentReport
  , install
  , renderReport
  , showVersion
  ) where

import Prelude

import Data.Array (length)
import Data.String (joinWith)
import Effect (Effect)
import Version (Version(..), version)

type BugReport =
  { version :: Version
  , userAgent :: String
  , generatedAt :: String
  , entries :: Array String
  }

foreign import install :: Effect Unit

foreign import snapshot :: Effect (Array String)

foreign import clear :: Effect Unit

foreign import userAgent :: Effect String

foreign import nowIso :: Effect String

currentReport :: Effect BugReport
currentReport = { version, userAgent: _, generatedAt: _, entries: _ }
  <$> userAgent
  <*> nowIso
  <*> snapshot

renderReport :: BugReport -> String
renderReport report = joinWith "\n" $
  [ "Domination bug report"
  , "Version: " <> showVersion report.version
  , "Generated: " <> report.generatedAt
  , "User agent: " <> report.userAgent
  , "Entries: " <> show (length report.entries)
  , ""
  ] <> report.entries

showVersion :: Version -> String
showVersion (Version major minor patch) =
  "v" <> show major <> "." <> show minor <> "." <> show patch
