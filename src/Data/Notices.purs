-- | What the problem dialog shows: one problem at a time, the first of each
-- | kind, with later problems of that kind counted rather than shown, and
-- | problems of other kinds waiting their turn. Dismissing a problem silences
-- | its kind for the rest of the page load.
module Domination.Data.Notices
  ( Entry
  , Notices
  , Showing
  , dismiss
  , noNotices
  , notice
  , showing
  , summary
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (any, catMaybes, head, length, snoc, uncons)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Domination.Data.Problem (Problem, ProblemKind, problemKind)

type Entry = { problem :: Problem, repeats :: Int }

type Notices =
  { queue :: Array Entry
  , quiet :: Set ProblemKind
  }

type Showing = { problem :: Problem, repeats :: Int, waiting :: Int }

noNotices :: Notices
noNotices = { queue: [], quiet: Set.empty }

notice :: Problem -> Notices -> Notices
notice problem notices = noticeOf (problemKind problem)
  where
    noticeOf kind
      | Set.member kind notices.quiet = notices
      | any (sameKind kind) notices.queue = notices { queue = countRepeat kind <$> notices.queue }
      | otherwise = notices { queue = snoc notices.queue { problem, repeats: 0 } }
    sameKind kind entry = problemKind entry.problem == kind
    countRepeat kind entry
      | sameKind kind entry = entry { repeats = entry.repeats + 1 }
      | otherwise = entry

dismiss :: Notices -> Notices
dismiss notices = maybe notices silence (uncons notices.queue)
  where
    silence { head: shown, tail } =
      { queue: tail, quiet: Set.insert (problemKind shown.problem) notices.quiet }

showing :: Notices -> Maybe Showing
showing { queue } = head queue <#> \{ problem, repeats } ->
  { problem, repeats, waiting: length queue - 1 }

-- | "Happened 1 more time · 1 other problem waiting", or nothing when the
-- | shown problem happened once and nothing else is waiting.
summary :: Showing -> Maybe String
summary { repeats, waiting } = case catMaybes [ counted repeats repeatsText, counted waiting waitingText ] of
  [] -> Nothing
  parts -> Just $ joinWith " · " parts
  where
    counted n text = text n <$ guard (n > 0)
    repeatsText n = "Happened " <> show n <> " more " <> plural n "time"
    waitingText n = show n <> " other " <> plural n "problem" <> " waiting"
    plural n word
      | n == 1 = word
      | otherwise = word <> "s"
