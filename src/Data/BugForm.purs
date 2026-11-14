-- | A link to the repository's GitHub bug form with the player's bug report
-- | prefilled in its diagnostics field, shortened to fit in a URL.
module Domination.Data.BugForm
  ( bugFormUrl
  , diagnosticsField
  , fitReport
  , prefilledUrl
  , signInRedirectLength
  , templateFile
  , urlBudget
  ) where

import Prelude

import Data.Array (length, reverse, scanl, takeEnd, takeWhile)
import Data.Foldable (sum)
import Data.String as String
import DebugLog (BugReport, LogExcerpt, renderReport)

foreign import formUrlEncode :: String -> String

foreign import encodeAsciiUriComponent :: String -> String

newIssueUrl :: String
newIssueUrl = "https://github.com/bagrounds/domination/issues/new"

templateFile :: String
templateFile = "bug_report.yml"

diagnosticsField :: String
diagnosticsField = "diagnostics"

-- | A signed-out player is sent to GitHub's sign-in page, which carries the
-- | issue URL, encoded once more, as `return_to`. In November 2026 that page
-- | answered `500` once the request's address and cookies together passed
-- | about 6.5 KB: a `return_to` of about 5,400 characters in a browser with
-- | GitHub's usual signed-out cookies. The budget leaves room for more cookies.
urlBudget :: Int
urlBudget = 4000

prefilledUrl :: String -> String
prefilledUrl diagnostics = newIssueUrl
  <> "?template=" <> templateFile
  <> "&" <> diagnosticsField <> "=" <> formUrlEncode diagnostics

-- | The length of an issue URL once GitHub's sign-in page carries it as
-- | `return_to`. Prefilled URLs are ASCII, so encoding them cannot fail.
signInRedirectLength :: String -> Int
signInRedirectLength = String.length <<< encodeAsciiUriComponent

bugFormUrl :: BugReport -> String
bugFormUrl = prefilledUrl <<< renderReport <<< fitReport urlBudget

-- | Keeps the report's header and the newest lines of each log that fit in
-- | the budget. Each log may use half of the room, and whatever the other
-- | log leaves unused.
-- |
-- | URL encoding works character by character, so each kept line adds
-- | exactly its own `lineCost` to the URL. A report with every line omitted
-- | prints each log's heading with its largest possible count, so it bounds
-- | the cost of everything but the kept lines.
fitReport :: Int -> BugReport -> BugReport
fitReport budget report = report { journal = journal, entries = entries }
  where
    room = budget - signInRedirectLength (prefilledUrl $ renderReport $ omitEverything report)
    journalRoom = max (room / 2) (room - linesCost (candidates room report.entries.lines))
    journal = newestWithin journalRoom report.journal
    entries = newestWithin (room - linesCost journal.lines) report.entries

omitEverything :: BugReport -> BugReport
omitEverything report = report
  { journal = newestWithin 0 report.journal
  , entries = newestWithin 0 report.entries
  }

newestWithin :: Int -> LogExcerpt -> LogExcerpt
newestWithin room { lines, omitted } =
  { lines: takeEnd kept lines, omitted: omitted + length lines - kept }
  where
    kept = length $ takeWhile (_ <= room) $ scanl (+) 0 $ lineCost <$> reverse (candidates room lines)

-- | Every line costs at least its separator, so no more than this many of
-- | the newest lines can fit.
candidates :: Int -> Array String -> Array String
candidates room = takeEnd (room / lineCost "")

linesCost :: Array String -> Int
linesCost = sum <<< map lineCost

lineCost :: String -> Int
lineCost line = signInRedirectLength $ formUrlEncode $ "\n" <> line
