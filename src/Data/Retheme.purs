module Domination.Data.Retheme
  ( RenameTarget(..)
  , Renameable
  , Change
  , Proposals
  , RenameSection
  , gameTerms
  , termRenameables
  , cardRenameables
  , renameSections
  , allRenameables
  , fieldLabel
  , mappingLabel
  , proposal
  , proposeRename
  , proposedChange
  , proposedChanges
  , normalizeWhitespace
  , changeCount
  , renderChange
  , renameMapping
  , previewCard
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (length, mapMaybe)
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (joinWith, trim)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (snd)
import Domination.Data.Card (Card)
import Domination.Data.Cards as Cards
import Util (justIf)

data RenameTarget
  = Term String
  | CardName String
  | CardText String
  | ReactionText String

derive instance genericRenameTarget :: Generic RenameTarget _
derive instance eqRenameTarget :: Eq RenameTarget
derive instance ordRenameTarget :: Ord RenameTarget

instance showRenameTarget :: Show RenameTarget where
  show = genericShow
instance encodeJsonRenameTarget :: EncodeJson RenameTarget where
  encodeJson = genericEncodeJson
instance decodeJsonRenameTarget :: DecodeJson RenameTarget where
  decodeJson = genericDecodeJson

type Renameable = { target :: RenameTarget, current :: String }

type Change = { renameable :: Renameable, proposed :: String }

type Proposals = Map RenameTarget String

type RenameSection = { title :: String, renameables :: Array Renameable }

gameTerms :: Array String
gameTerms =
  [ "Domination"
  , "Game"
  , "Player"
  , "Turn"
  , "Action"
  , "Buy"
  , "Card"
  , "Money"
  , "Victory Point"
  , "Treasure"
  , "Victory"
  , "Curse"
  , "Attack"
  , "Reaction"
  , "Hand"
  , "Deck"
  , "Discard"
  , "Trash"
  , "Supply"
  , "Kingdom"
  , "Action Phase"
  , "Buy Phase"
  , "Cleanup Phase"
  ]

termRenameables :: Array Renameable
termRenameables = gameTerm <$> gameTerms
  where
    gameTerm term = { target: Term term, current: term }

cardRenameables :: Card -> Array Renameable
cardRenameables card =
  [ { target: CardName card.name, current: card.name } ]
    <> foldMap specialText card.special
    <> foldMap reactionText card.reaction
  where
    specialText { description } =
      [ { target: CardText card.name, current: description } ]
    reactionText reaction =
      [ { target: ReactionText card.name, current: snd reaction } ]

renameSections :: Array RenameSection
renameSections =
  [ { title: "Terms", renameables: termRenameables }
  , { title: "Cards", renameables: Cards.cardMap >>= cardRenameables }
  ]

allRenameables :: Array Renameable
allRenameables = renameSections >>= _.renameables

fieldLabel :: RenameTarget -> String
fieldLabel = case _ of
  Term term -> term
  CardName _ -> "Name"
  CardText _ -> "Text"
  ReactionText _ -> "Reaction"

mappingLabel :: RenameTarget -> String
mappingLabel = case _ of
  Term term -> term
  CardName name -> name
  CardText name -> name <> " text"
  ReactionText name -> name <> " reaction"

proposal :: RenameTarget -> Proposals -> String
proposal target = fromMaybe "" <<< Map.lookup target

proposeRename :: RenameTarget -> String -> Proposals -> Proposals
proposeRename target = case _ of
  "" -> Map.delete target
  text -> Map.insert target text

proposedChange :: Proposals -> Renameable -> Maybe Change
proposedChange proposals renameable =
  { renameable, proposed: _ }
    <$> justIf isChange (normalizeWhitespace $ proposal renameable.target proposals)
  where
    isChange proposed = proposed /= "" && proposed /= renameable.current

proposedChanges :: Proposals -> Array Renameable -> Array Change
proposedChanges = mapMaybe <<< proposedChange

normalizeWhitespace :: String -> String
normalizeWhitespace = trim <<< replace whitespaceRuns " "
  where
    whitespaceRuns = unsafeRegex "\\s+" global

changeCount :: Proposals -> Int
changeCount proposals = length $ proposedChanges proposals allRenameables

renderChange :: Change -> String
renderChange { renameable, proposed } =
  "- " <> mappingLabel renameable.target <> " → " <> proposed

renameMapping :: Proposals -> String
renameMapping proposals = joinWith "\n" $ renameSections >>= renderSection
  where
    renderSection { title, renameables } =
      foldMap (sectionLines title)
        $ NEA.fromArray
        $ proposedChanges proposals renameables
    sectionLines title changes =
      [ title <> ":" ] <> NEA.toArray (renderChange <$> changes)

previewCard :: Proposals -> Card -> Card
previewCard proposals card = card
  { name = proposedOr (CardName card.name) card.name
  , special = previewDescription <$> card.special
  , reaction = map (proposedOr $ ReactionText card.name) <$> card.reaction
  }
  where
    proposedOr target current =
      maybe current _.proposed $ proposedChange proposals { target, current }
    previewDescription special = special
      { description = proposedOr (CardText card.name) special.description }
