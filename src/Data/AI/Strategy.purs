module Domination.Data.AI.Strategy where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Strategy
  = BigMoney
  | Random

derive instance genericStrategy :: Generic Strategy _
derive instance eqStrategy :: Eq Strategy
derive instance ordStrategy :: Ord Strategy

instance showStrategy :: Show Strategy where
  show = genericShow

instance encodeJsonStrategy :: EncodeJson Strategy where
  encodeJson = genericEncodeJson

instance decodeJsonStrategy :: DecodeJson Strategy where
  decodeJson = genericDecodeJson

botName :: Strategy -> String
botName BigMoney = "Big Money Bot"
botName Random = "Random Bot"

allStrategies :: Array Strategy
allStrategies = [ BigMoney, Random ]
