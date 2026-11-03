module Test.Retheme (retheme_tests) where

import Prelude

import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (all, filter, foldr, length, nub)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Cards as Cards
import Domination.Data.Retheme (Proposals, RenameTarget(..), Renameable, allRenameables, cardRenameables, changeCount, mappingLabel, normalizeWhitespace, previewCard, proposal, proposeRename, renameMapping)
import Effect (Effect)
import Test.QuickCheck (Result, assertEquals, (<?>))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf, vectorOf)
import Test.Room (all_success, quick_check)

type EffectTest = { name :: String, test :: Effect Result }

retheme_tests :: Array EffectTest
retheme_tests =
  renameable_tests
    <> proposal_tests
    <> whitespace_tests
    <> mapping_tests
    <> preview_tests

-- ── Generators ──

renameableGen :: Gen Renameable
renameableGen = elements $ fromMaybe (NEA.singleton fallback) $ NEA.fromArray allRenameables
  where
    fallback = { target: Term "", current: "" }

cardGen :: Gen Card
cardGen = elements $ fromMaybe (NEA.singleton $ Card._card Cards.copper) $ NEA.fromArray Cards.cardMap

wordGen :: Gen String
wordGen = do
  n <- chooseInt 1 20
  fromCharArray <$> vectorOf n (elements $ NEA.cons' 'a' $ toCharArray "bcxyzABCXYZ éñ'-.\n\t")

proposalTextGen :: String -> Gen String
proposalTextGen current = oneOf $ NEA.cons' (pure "")
  [ pure "   "
  , pure current
  , pure $ " " <> current <> " "
  , wordGen
  , (\w -> " " <> w <> "  ") <$> wordGen
  ]

proposalGen :: Gen (Tuple RenameTarget String)
proposalGen = do
  { target, current } <- renameableGen
  Tuple target <$> proposalTextGen current

proposalsGen :: Gen Proposals
proposalsGen = foldr propose Map.empty <$> arrayOf proposalGen
  where
    propose (Tuple target text) = proposeRename target text

-- ── Helpers ──

isChange :: Renameable -> String -> Boolean
isChange { current } text = normalizeWhitespace text /= "" && normalizeWhitespace text /= current

changeLine :: Renameable -> String -> String
changeLine { target } text = "- " <> mappingLabel target <> " → " <> normalizeWhitespace text

mappingLines :: Proposals -> Array String
mappingLines = filter (_ /= "") <<< split (Pattern "\n") <<< renameMapping

changeLines :: Proposals -> Array String
changeLines = filter (String.contains (Pattern "- ") <<< String.take 2) <<< mappingLines

-- ── Renameables ──

renameable_tests :: Array EffectTest
renameable_tests =
  [ { name: "every renameable element has a unique target"
    , test: pure $ assertEquals (length $ nub targets) (length targets)
    }
  , { name: "every registered card can be renamed"
    , test: pure $ all_success $ Cards.cardMap <#> \card ->
        (CardName card.name `elem` targets) <?> ("missing card name: " <> card.name)
    }
  , { name: "a card offers its name, special text and reaction text"
    , test: pure $ all_success
        [ assertEquals (_.target <$> cardRenameables (Card._card Cards.copper))
            [ CardName "Copper" ]
        , assertEquals (_.target <$> cardRenameables (Card._card Cards.secretChamber))
            [ CardName "Secret Chamber", CardText "Secret Chamber", ReactionText "Secret Chamber" ]
        ]
    }
  ]
  where
    targets = _.target <$> allRenameables

-- ── Proposals ──

proposal_tests :: Array EffectTest
proposal_tests =
  [ { name: "∀ target, text: proposal reads back what was proposed"
    , test: quick_check 300 do
        Tuple target text <- proposalGen
        proposals <- proposalsGen
        pure $ assertEquals (proposal target $ proposeRename target text proposals) text
    }
  , { name: "∀ target: proposing a blank name forgets the proposal"
    , test: quick_check 300 do
        Tuple target text <- proposalGen
        proposals <- proposalsGen
        let cleared = proposeRename target "" $ proposeRename target text proposals
        pure $ assertEquals (Map.lookup target cleared) Nothing
    }
  , { name: "∀ proposals: proposals survive a JSON round trip"
    , test: quick_check 200 do
        proposals <- proposalsGen
        pure $ assertEquals (decodeJson $ encodeJson proposals) (Right proposals)
    }
  ]

-- ── Whitespace ──

whitespace_tests :: Array EffectTest
whitespace_tests =
  [ { name: "∀ text: normalizing whitespace is idempotent and leaves no line breaks"
    , test: quick_check 300 do
        text <- wordGen
        let normalized = normalizeWhitespace text
        pure $ all_success
          [ assertEquals (normalizeWhitespace normalized) normalized
          , (not $ String.contains (Pattern "\n") normalized) <?> show normalized
          ]
    }
  ]

-- ── Mapping ──

mapping_tests :: Array EffectTest
mapping_tests =
  [ { name: "no proposals: nothing to copy"
    , test: pure $ all_success
        [ assertEquals (renameMapping Map.empty) ""
        , assertEquals (changeCount Map.empty) 0
        ]
    }
  , { name: "∀ proposals: the mapping has one line per proposed change"
    , test: quick_check 300 do
        proposals <- proposalsGen
        pure $ assertEquals (length $ changeLines proposals) (changeCount proposals)
    }
  , { name: "∀ renameable, text: the mapping lists it iff the normalized text is a new name"
    , test: quick_check 500 do
        renameable <- renameableGen
        text <- proposalTextGen renameable.current
        let lines = mappingLines $ proposeRename renameable.target text Map.empty
        pure $ assertEquals
          (changeLine renameable text `elem` lines)
          (isChange renameable text)
    }
  , { name: "∀ proposals: every line belongs to a section header or a change"
    , test: quick_check 300 do
        proposals <- proposalsGen
        pure $ all (\line -> line `elem` [ "Terms:", "Cards:" ] || String.take 2 line == "- ")
          (mappingLines proposals)
          <?> renameMapping proposals
    }
  , { name: "the mapping is grouped by terms then cards, in form order"
    , test: pure $ assertEquals
        ( renameMapping
          $ proposeRename (CardText "Chapel") " Burn up to 4 scrolls\n  from your hand. "
          $ proposeRename (CardName "Copper") "Spark"
          $ proposeRename (CardName "Silver") "Silver"
          $ proposeRename (Term "Action") "Cast"
          $ Map.empty
        )
        ( String.joinWith "\n"
          [ "Terms:"
          , "- Action → Cast"
          , "Cards:"
          , "- Copper → Spark"
          , "- Chapel text → Burn up to 4 scrolls from your hand."
          ]
        )
    }
  ]

-- ── Preview ──

preview_tests :: Array EffectTest
preview_tests =
  [ { name: "∀ card: previewing without proposals shows the card unchanged"
    , test: pure $ all_success $ Cards.cardMap <#> \card ->
        assertEquals (previewCard Map.empty card) card
    }
  , { name: "∀ card, text: the preview shows a proposed name iff it is a new name"
    , test: quick_check 300 do
        card <- cardGen
        text <- proposalTextGen card.name
        let
          renameable = { target: CardName card.name, current: card.name }
          preview = previewCard (proposeRename renameable.target text Map.empty) card
          expected = if isChange renameable text then normalizeWhitespace text else card.name
        pure $ assertEquals preview.name expected
    }
  , { name: "the preview shows proposed card and reaction text"
    , test: pure $ let
        preview = previewCard
          ( proposeRename (CardText "Secret Chamber") "Hide 2 scrolls."
            $ proposeRename (ReactionText "Secret Chamber") "Shield yourself."
            $ Map.empty
          )
          (Card._card Cards.secretChamber)
        in all_success
          [ assertEquals (_.description <$> preview.special) (Just "Hide 2 scrolls.")
          , assertEquals ((\(Tuple _ text) -> text) <$> preview.reaction) (Just "Shield yourself.")
          ]
    }
  ]
