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
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice)
import Domination.Data.SelectCards (SelectCards)
import Domination.Data.Target (Target)
import Util (justIf)

type Card =
  { types :: Array CardType
  , name :: String
  , cost :: Int
  , victoryPoints :: Int
  , treasure :: Int
  , buys :: Int
  , cards :: Int
  , actions :: Int
  , specials :: Array Special
  }

_types :: Lens' Card (Array CardType)
_types = prop (SProxy :: SProxy "types")
_name :: Lens' Card String
_name = prop (SProxy :: SProxy "name")
_cost :: Lens' Card Int
_cost = prop (SProxy :: SProxy "cost")
_victoryPoints :: Lens' Card Int
_victoryPoints = prop (SProxy :: SProxy "victoryPoints")
_treasure :: Lens' Card Int
_treasure = prop (SProxy :: SProxy "treasure")
_buys :: Lens' Card Int
_buys = prop (SProxy :: SProxy "buys")
_cards :: Lens' Card Int
_cards = prop (SProxy :: SProxy "cards")
_actions :: Lens' Card Int
_actions = prop (SProxy :: SProxy "actions")
_specials :: Lens' Card (Array Special)
_specials = prop (SProxy :: SProxy "specials")

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

value :: Array Card -> Int
value = foldr (+) 0 <<< map _.treasure

cost :: Array Card -> Int
cost = foldr (+) 0 <<< map _.cost

card :: Card
card =
  { types: []
  , name: ""
  , cost: 0
  , victoryPoints: 0
  , treasure: 0
  , buys: 0
  , cards: 0
  , actions: 0
  , specials: []
  }

treasure :: Card
treasure = card { types = [Treasure] }

victory :: Card
victory = card { types = [Victory] }

action :: Card
action = card { types = [Action] }

actionAttack :: Card
actionAttack = card { types = [ Action, Attack ] }

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
  = Gain Card -- the dependency on Card prevents us from splitting Command into another module
  | Draw Int
  | Discard SelectCards
  | Choose Choice

derive instance genericCommand :: Generic Command _
derive instance eqCommand :: Eq Command

instance showCommand :: Show Command where
  show x = genericShow x
instance encodeJsonCommand :: EncodeJson Command where
  encodeJson x = genericEncodeJson x
instance decodeJsonCommand :: DecodeJson Command where
  decodeJson x = genericDecodeJson x

_toGain :: Prism' Command Card
_toGain = prism' Gain case _ of
  Gain c -> Just c
  _ -> Nothing
_toDraw :: Prism' Command Int
_toDraw = prism' Draw case _ of
  Draw n -> Just n
  _ -> Nothing
_toDiscard :: Prism' Command SelectCards
_toDiscard = prism' Discard case _ of
  Discard s -> Just s
  _ -> Nothing
_toChoose :: Prism' Command Choice
_toChoose = prism' Choose case _ of
  Choose s -> Just s
  _ -> Nothing

