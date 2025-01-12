{-|
Module for defining cards in the game.

This module provides data types and functions for defining and working with cards in the game.
It includes definitions for different types of cards, lenses for accessing card properties, and utility functions.

Key Components:
- Card: Represents a card in the game.
- CardSpec: Represents a card specification with additional requirements.
- Lenses: Functional getters and setters for card properties.
- Utility Functions: Functions for working with cards.

Technical Concepts:
* Lenses: Functional getters and setters for nested data structures.
* Newtype: A wrapper around an existing type to create a distinct type.
-}
module Domination.Data.Card where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as Array
import Data.Foldable (elem, foldr)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter (view)
import Data.Lens.Lens (Lens', lens')
import Data.Lens.Prism (Prism', is, prism')
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Domination.Data.Actions (Actions)
import Domination.Data.Buys (Buys)
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice)
import Domination.Data.Filter (Filter(..))
import Domination.Data.Filter as Filter
import Domination.Data.Points (Points)
import Domination.Data.Reaction (Reaction)
import Domination.Data.Target (Target)
import Util (justIf)

type Card =
  { types :: Array CardType
  , name :: String
  , cost :: Int
  , victoryPoints :: Points
  , treasure :: Int
  , buys :: Buys
  , cards :: Int
  , actions :: Actions
  , special :: Maybe Special
  , reaction :: Maybe Reaction
  }

data CardSpec
  = IndependentCard Card
  | CardWithRequirements { card :: Card, requirements :: (Array Card) }

derive instance genericCardSpec :: Generic CardSpec _
derive instance eqCardSpec :: Eq CardSpec

instance showCardSpec :: Show CardSpec where
  show x = genericShow x
instance encodeJsonCardSpec :: EncodeJson CardSpec where
  encodeJson x = genericEncodeJson x
instance decodeJsonCardSpec :: DecodeJson CardSpec where
  decodeJson x = genericDecodeJson x

_card :: CardSpec -> Card
_card (IndependentCard c) = c
_card (CardWithRequirements { card: c }) = c

_requirements :: CardSpec -> Array Card
_requirements (IndependentCard _) = []
_requirements (CardWithRequirements { requirements: rs }) = rs

independentCard :: Card -> CardSpec
independentCard = IndependentCard

cardWithRequirements :: Card -> Array CardSpec -> CardSpec
cardWithRequirements c specs = CardWithRequirements
  { card: c, requirements: newRequirements }
  where
    specList :: List CardSpec
    specList = Array.toUnfoldable specs

    newRequirements :: Array Card
    newRequirements = List.toUnfoldable $ transitiveRequirements specList

    transitiveRequirements :: List CardSpec -> List Card
    transitiveRequirements Nil = Nil
    transitiveRequirements (Cons (IndependentCard c1) css) = Cons c1 $ transitiveRequirements css
    transitiveRequirements (Cons (CardWithRequirements { card: c2, requirements }) css) = List.concat $ Cons l1 $ Cons l2 Nil
      where
        l1 :: List Card
        l1 = Cons c2 (Array.toUnfoldable requirements)
        l2 :: List Card
        l2 = transitiveRequirements css

_types :: Lens' Card (Array CardType)
_types = prop (SProxy :: SProxy "types")
_name :: Lens' Card String
_name = prop (SProxy :: SProxy "name")
_cost :: Lens' Card Int
_cost = prop (SProxy :: SProxy "cost")
_victoryPoints :: Lens' Card Points
_victoryPoints = prop (SProxy :: SProxy "victoryPoints")
_treasure :: Lens' Card Int
_treasure = prop (SProxy :: SProxy "treasure")
_buys :: Lens' Card Buys
_buys = prop (SProxy :: SProxy "buys")
_cards :: Lens' Card Int
_cards = prop (SProxy :: SProxy "cards")
_actions :: Lens' Card Actions
_actions = prop (SProxy :: SProxy "actions")
_maybeSpecial :: Lens' Card (Maybe Special)
_maybeSpecial = prop (SProxy :: SProxy "special")
_reaction :: Traversal' Card Reaction
_reaction = prop (SProxy :: SProxy "reaction") <<< _Just
_special :: Traversal' Card Special
_special = prop (SProxy :: SProxy "special") <<< _Just

_ofType :: CardType -> Prism' Card Card
_ofType cardType = prism' identity $ justIf $ elem cardType <<< _.types

hasType :: CardType -> Card -> Boolean
hasType t = is $ _ofType t

isAction :: Card -> Boolean
isAction = hasType Action

isTreasure :: Card -> Boolean
isTreasure = hasType Treasure

isVictory :: Card -> Boolean
isVictory = hasType Victory

isReaction :: Card -> Boolean
isReaction = hasType Reaction

value :: Array Card -> Int
value = foldr (+) 0 <<< map _.treasure

cost :: Array Card -> Int
cost = foldr (+) 0 <<< map _.cost

card :: Card
card =
  { types: []
  , name: ""
  , cost: zero
  , victoryPoints: zero
  , treasure: zero
  , buys: zero
  , cards: zero
  , actions: zero
  , special: Nothing
  , reaction: Nothing
  }

treasure :: Card
treasure = card { types = [Treasure] }

victory :: Card
victory = card { types = [Victory] }

action :: Card
action = card { types = [Action] }

actionAttack :: Card
actionAttack = card { types = [ Action, Attack ] }

actionVictory :: Card
actionVictory = card { types = [ Action, Victory ] }

reaction :: Card
reaction = card { types = [ Reaction ] }

actionReaction :: Card
actionReaction = card { types = [ Action, Reaction ] }

type Special =
  { target :: Target
  , command :: Command
  , description :: String
  }

_target :: Lens' Special Target
_target = prop (SProxy :: SProxy "target")
_command :: Lens' Special Command
_command = prop (SProxy :: SProxy "command")
_description :: Lens' Special String
_description = prop (SProxy :: SProxy "description")

data Command
  = Choose Choice

_choice :: Lens' Command Choice
_choice = lens' f
  where
    f (Choose choice) = Tuple choice Choose


derive instance genericCommand :: Generic Command _
derive instance eqCommand :: Eq Command

instance showCommand :: Show Command where
  show x = genericShow x
instance encodeJsonCommand :: EncodeJson Command where
  encodeJson x = genericEncodeJson x
instance decodeJsonCommand :: DecodeJson Command where
  decodeJson x = genericDecodeJson x

positivePoints :: Card -> Points
positivePoints = max zero <<< _.victoryPoints

negativePoints :: Card -> Points
negativePoints = min zero <<< _.victoryPoints

passFilter :: Filter -> Card -> Boolean
passFilter = case _ of
  HasName name -> _.name >>> (_ == name)
  HasType cardType -> hasType cardType
  CostUpTo cost' -> (_ <= cost') <<< (view _cost)
  Any -> const true
  Filter.And f1 f2 -> passFilter f1 && passFilter f2

