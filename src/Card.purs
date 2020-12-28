module Card
  ( Card
  , Special(..)
  , Target(..)
  , Command(..)
  , SelectCards(..)
  , card
  , action
  , actionAttack
  , treasure
  , victory
  , cost
  , value
  , isAction
  , isTreasure
  , isVictory
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (elem)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import CardType (CardType(..))

value :: Array Card -> Int
value = foldr (+) 0 <<< map _.treasure

cost :: Array Card -> Int
cost = foldr (+) 0 <<< map _.cost

isAction :: Card -> Boolean
isAction = elem Action <<< _.types

isTreasure :: Card -> Boolean
isTreasure = elem Treasure <<< _.types

isVictory :: Card -> Boolean
isVictory = elem Victory <<< _.types

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

card :: Card
card = { types: [], name: "", cost: 0, victoryPoints: 0, treasure: 0, buys: 0, cards: 0, actions: 0, specials: [] }
treasure :: Card
treasure = card { types = [Treasure] }
victory :: Card
victory = card { types = [Victory] }
action :: Card
action = card { types = [Action] }
actionAttack :: Card
actionAttack = card { types = [ Action, Attack ] }

type Special = { target :: Target, command :: Command, description :: String }
data Target = Self | Everyone | EveryoneElse
derive instance genericTarget :: Generic Target _
derive instance eqTarget :: Eq Target
instance showTarget :: Show Target where
  show = genericShow
instance encodeJsonTarget :: EncodeJson Target where
  encodeJson = genericEncodeJson
instance decodeJsonTarget :: DecodeJson Target where
  decodeJson = genericDecodeJson

data Command = Gain Card | Draw Int | Discard SelectCards
derive instance genericCommand :: Generic Command _
derive instance eqCommand :: Eq Command
instance showCommand :: Show Command where show x = genericShow x
instance encodeJsonCommand :: EncodeJson Command where
  encodeJson x = genericEncodeJson x
instance decodeJsonCommand :: DecodeJson Command where
  decodeJson x = genericDecodeJson x

data SelectCards = SelectAll -- | SelectOne Int | SelectSome (Array Int) | SelectNone
derive instance genericSelectCards :: Generic SelectCards _
derive instance eqSelectCards :: Eq SelectCards
instance showSelectCards :: Show SelectCards where show x = genericShow x
instance encodeJsonSelectCards :: EncodeJson SelectCards where
  encodeJson = genericEncodeJson
instance decodeJsonSelectCards :: DecodeJson SelectCards where
  decodeJson = genericDecodeJson
