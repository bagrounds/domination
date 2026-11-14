module Test.BugForm (bug_form_tests) where

import Prelude

import Data.Array (length, replicate, takeEnd, (..))
import Data.Array as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import DebugLog (BugReport, LogExcerpt, renderReport, wholeLog)
import Domination.Data.BugForm (bugFormUrl, diagnosticsField, fitReport, prefilledUrl, signInRedirectLength, templateFile, urlBudget)
import Effect (Effect)
import Test.QuickCheck (Result(..), assertEquals, (<?>))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, frequency, vectorOf)
import Test.Room (all_success, quick_check)
import Data.Array.NonEmpty as NEA
import Data.Tuple (Tuple(..))
import Version (Version(..))

type EffectTest = { name :: String, test :: Effect Result }

foreign import queryParamFFI
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> String
  -> String
  -> Maybe String

foreign import readTextFile :: String -> Effect String

queryParam :: String -> String -> Maybe String
queryParam = queryParamFFI Just Nothing

bug_form_tests :: Array EffectTest
bug_form_tests =
  [ { name: "∀ report, budget: the prefilled link fits the budget whenever the report's header does"
    , test: quick_check 100 do
        report <- reportGen
        budget <- chooseInt 0 20000
        let size = urlLength $ fitReport budget report
        pure $ (size <= budget || urlLength (headerOnly report) > budget)
          <?> "link of " <> show size <> " for a budget of " <> show budget
    }
  , { name: "∀ report: the link's diagnostics field reads back as the shortened report"
    , test: quick_check 100 do
        report <- reportGen
        let url = bugFormUrl report
        pure $ all_success
          [ assertEquals (queryParam "diagnostics" url) (Just $ renderReport $ fitReport urlBudget report)
          , assertEquals (queryParam "template" url) (Just "bug_report.yml")
          , assertEquals (String.take 51 url) "https://github.com/bagrounds/domination/issues/new?"
          ]
    }
  , { name: "∀ report, budget: each log keeps its newest lines and counts the rest as omitted"
    , test: quick_check 100 do
        report <- reportGen
        budget <- chooseInt 0 20000
        let
          fitted = fitReport budget report
          keepsNewest original excerpt = excerpt.lines == takeEnd (length excerpt.lines) original.lines
            && excerpt.omitted + length excerpt.lines == length original.lines
        pure $ (keepsNewest report.journal fitted.journal && keepsNewest report.entries fitted.entries)
          <?> "fitted " <> show { journal: fitted.journal, entries: fitted.entries }
    }
  , { name: "∀ report that fits the budget: the link carries all of it"
    , test: quick_check 100 do
        report <- reportGen
        budget <- chooseInt 0 20000
        let fitted = fitReport budget report
        pure $
          if urlLength report <= budget
          then assertEquals
            (renderReport fitted /\ fitted.journal /\ fitted.entries)
            (renderReport report /\ report.journal /\ report.entries)
          else Success
    }
  , { name: "∀ shortened report: the link leaves no room for either log's next older line"
    , test: quick_check 100 do
        report <- reportGen
        budget <- chooseInt 0 20000
        let
          fitted = fitReport budget report
          nextCost original excerpt = maybe 0 lineCost
            $ Array.index original.lines (length original.lines - length excerpt.lines - 1)
          unused = budget - urlLength fitted
          slack = nextCost report.journal fitted.journal + nextCost report.entries fitted.entries
            + 2 * String.length ", oldest 99999 omitted"
          shortened = fitted.journal.omitted + fitted.entries.omitted > 0
        pure $ (not shortened || urlLength (headerOnly report) > budget || unused < slack)
          <?> show unused <> " unused, next lines and headings cost " <> show slack
    }
  , { name: "a full log of a long session links its header and both logs' newest lines"
    , test: pure do
        let
          report = sampleReport
            { journal = wholeLog $ numbered "Mailbox: delivered revision" <$> 1 .. 1500
            , entries = wholeLog $ numbered "WebSocket FFI:  sent message length:" <$> 1 .. 400
            }
          fitted = fitReport urlBudget report
          text = renderReport fitted
        all_success
          [ (urlLength fitted <= urlBudget) <?> "link of " <> show (urlLength fitted)
          , (length fitted.journal.lines > 10 && length fitted.entries.lines > 10)
              <?> "kept " <> show (length fitted.journal.lines) <> " and " <> show (length fitted.entries.lines)
          , assertEquals (Array.last fitted.entries.lines) (Array.last report.entries.lines)
          , String.contains (String.Pattern "Version: v0.44.0") text <?> text
          , String.contains (String.Pattern "Console: 400, oldest ") text <?> text
          ]
    }
  , { name: "lines cut inside an emoji still fit, and read back with a replacement character"
    , test: pure do
        let
          cutEmoji = CodeUnits.take 30 "2026-11-14T18:00:00.000Z LOG 🃏"
          report = sampleReport { journal = wholeLog $ replicate 2000 cutEmoji }
          fitted = fitReport urlBudget report
          decoded = queryParam "diagnostics" (bugFormUrl report)
        all_success
          [ (urlLength fitted <= urlBudget) <?> "link of " <> show (urlLength fitted)
          , (length fitted.journal.lines > 10) <?> "kept " <> show (length fitted.journal.lines)
          , (maybe false (String.contains (String.Pattern "LOG \xFFFD")) decoded) <?> show decoded
          ]
    }
  , { name: "the issue form the link opens has the diagnostics field"
    , test: do
        form <- readTextFile $ ".github/ISSUE_TEMPLATE/" <> templateFile
        pure $ String.contains (String.Pattern $ "id: " <> diagnosticsField <> "\n") form
          <?> "no " <> diagnosticsField <> " field in " <> templateFile
    }
  , { name: "a short console leaves the rest of the room to the journal"
    , test: pure do
        let
          report = sampleReport
            { journal = wholeLog $ numbered "Clock: tick" <$> 1 .. 1500
            , entries = wholeLog [ "2026-11-14T18:00:00.000Z ERROR boom" ]
            }
          fitted = fitReport urlBudget report
        all_success
          [ assertEquals fitted.entries report.entries
          , (urlLength fitted > urlBudget - 200) <?> "link of " <> show (urlLength fitted)
          ]
    }
  ]

infixr 6 Tuple as /\

urlLength :: BugReport -> Int
urlLength = signInRedirectLength <<< prefilledUrl <<< renderReport

lineCost :: String -> Int
lineCost line = urlLength' ("\n" <> line) - urlLength' ""
  where
    urlLength' = signInRedirectLength <<< prefilledUrl

headerOnly :: BugReport -> BugReport
headerOnly report = report { journal = omitAll report.journal, entries = omitAll report.entries }
  where
    omitAll :: LogExcerpt -> LogExcerpt
    omitAll { lines, omitted } = { lines: [], omitted: omitted + length lines }

numbered :: String -> Int -> String
numbered text n = "2026-11-14T18:00:00.000Z LOG " <> text <> " " <> show n

sampleReport :: BugReport
sampleReport =
  { version: Version 0 44 0
  , userAgent: "Mozilla/5.0 (Linux; Android 14) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Mobile Safari/537.36"
  , generatedAt: "2026-11-14T18:00:00.000Z"
  , facts: [ { label: "Page", value: "/domination/#room=abc123" } ]
  , journal: wholeLog []
  , entries: wholeLog []
  }

reportGen :: Gen BugReport
reportGen = do
  userAgent <- textGen 300
  facts <- arrayOf ({ label: _, value: _ } <$> textGen 20 <*> textGen 60)
  journal <- logGen
  entries <- logGen
  pure sampleReport { userAgent = userAgent, facts = Array.take 6 facts, journal = journal, entries = entries }

logGen :: Gen LogExcerpt
logGen = do
  count <- chooseInt 0 150
  wholeLog <$> vectorOf count lineGen

lineGen :: Gen String
lineGen = frequency $ NEA.cons' (Tuple 9.0 (textGen 120)) [ Tuple 1.0 (textGen 1200) ]

textGen :: Int -> Gen String
textGen maxPieces = do
  count <- chooseInt 0 maxPieces
  fold <$> vectorOf count (elements pieces)
  where
    pieces = NEA.cons' "a"
      [ "Z", "7", " ", "-", ".", "_", "~", "*", ":", "/", "%", "+", "&", "=", "#", "?"
      , "\n", "\t", "\"", "<", "é", "☃", "中", "🃏", fold (replicate 8 "x")
      ]
