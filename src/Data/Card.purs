module Domination.Data.Card where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Foldable (elem, foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (Prism', is, prism')
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Domination.Data.Actions (Actions)
import Domination.Data.Buys (Buys)
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice)
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

derive instance genericCommand :: Generic Command _
derive instance eqCommand :: Eq Command

instance showCommand :: Show Command where
  show x = genericShow x
instance encodeJsonCommand :: EncodeJson Command where
  encodeJson x = genericEncodeJson x
instance decodeJsonCommand :: DecodeJson Command where
  decodeJson x = genericDecodeJson x

_toChoose :: Prism' Command Choice
_toChoose = prism' Choose case _ of
  Choose s -> Just s

positivePoints :: Card -> Points
positivePoints = max zero <<< _.victoryPoints

negativePoints :: Card -> Points
negativePoints = min zero <<< _.victoryPoints

