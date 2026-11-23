-- | A link to the repository's GitHub bug form with the player's bug report
-- | prefilled in its diagnostics field, shortened to fit in a URL. A link for
-- | a problem also prefills the title and what happened.
module Domination.Data.BugForm
  ( bugFormUrl
  , diagnosticsField
  , fitReport
  , fitReportFor
  , prefilledUrl
  , problemFormUrl
  , problemUrl
  , titleField
  , whatHappenedField
  , signInRedirectLength
  , templateFile
  , urlBudget
  ) where

import Prelude

import Data.Array (length, reverse, scanl, takeEnd, takeWhile)
import Data.Foldable (foldMap, sum)
import Data.String as String
import Data.Tuple (Tuple(..))
import DebugLog (BugReport, LogExcerpt, renderReport)
import Domination.Data.Problem (Problem, consequence, headline, message)

foreign import formUrlEncode :: String -> String

foreign import encodeAsciiUriComponent :: String -> String

newIssueUrl :: String
newIssueUrl = "https://github.com/bagrounds/domination/issues/new"

templateFile :: String
templateFile = "bug_report.yml"

diagnosticsField :: String
diagnosticsField = "diagnostics"

titleField :: String
titleField = "title"

whatHappenedField :: String
whatHappenedField = "what-happened"

-- | A signed-out player is sent to GitHub's sign-in page, which carries the
-- | issue URL, encoded once more, as `return_to`. In November 2026 that page
-- | answered `500` once the request's address and cookies together passed
-- | about 6.5 KB: a `return_to` of about 5,400 characters in a browser with
-- | GitHub's usual signed-out cookies. The budget leaves room for more cookies.
urlBudget :: Int
urlBudget = 4000

issueUrl :: Array (Tuple String String) -> String
issueUrl fields = newIssueUrl <> "?template=" <> templateFile <> foldMap field fields
  where
    field (Tuple name value) = "&" <> name <> "=" <> formUrlEncode value

prefilledUrl :: String -> String
prefilledUrl diagnostics = issueUrl [ Tuple diagnosticsField diagnostics ]

problemUrl :: Problem -> String -> String
problemUrl problem diagnostics = issueUrl
  [ Tuple titleField $ "Bug: " <> headline problem
  , Tuple whatHappenedField $ problemDescription problem
  , Tuple diagnosticsField diagnostics
  ]

problemDescription :: Problem -> String
problemDescription problem = String.joinWith "\n"
  [ headline problem <> "."
  , consequence problem
  , ""
  , "Error: " <> message problem
  , ""
  , "What were you doing just before this happened?"
  ]

-- | The length of an issue URL once GitHub's sign-in page carries it as
-- | `return_to`. Prefilled URLs are ASCII, so encoding them cannot fail.
signInRedirectLength :: String -> Int
signInRedirectLength = String.length <<< encodeAsciiUriComponent

bugFormUrl :: BugReport -> String
bugFormUrl = prefilledUrl <<< renderReport <<< fitReport urlBudget

problemFormUrl :: Problem -> BugReport -> String
problemFormUrl problem = problemUrl problem <<< renderReport <<< fitReportFor (problemUrl problem) urlBudget

-- | Keeps the report's header and the newest lines of each log that fit in
-- | the budget. Each log may use half of the room, and whatever the other
-- | log leaves unused.
-- |
-- | URL encoding works character by character, so each kept line adds
-- | exactly its own `lineCost` to the URL. A report with every line omitted
-- | prints each log's heading with its largest possible count, so it bounds
-- | the cost of everything but the kept lines.
fitReport :: Int -> BugReport -> BugReport
fitReport = fitReportFor prefilledUrl

-- | `fitReport` for a link that `toUrl` builds from the rendered report.
fitReportFor :: (String -> String) -> Int -> BugReport -> BugReport
fitReportFor toUrl budget report = report { journal = journal, entries = entries }
  where
    room = budget - signInRedirectLength (toUrl $ renderReport $ omitEverything report)
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
