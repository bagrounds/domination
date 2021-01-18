module Domination.Data.Choice where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Domination.Data.SelectCards (SelectCards)

data Choice
  = TrashUpTo
    { n :: Int
    , resolution :: (Maybe (Array Int))
    , attack :: Boolean
    }
  | DiscardDownTo
    { n :: Int
    , resolution :: (Maybe (Array Int))
    , attack :: Boolean
    }
  | GainCards
    { n :: Int
    , cardName :: String
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | Discard
    { selection :: SelectCards
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | Draw
    { n :: Int
    , resolution :: Maybe Unit
    , attack :: Boolean
    }

isAttack :: Choice -> Boolean
isAttack = case _ of
  DiscardDownTo { attack } -> attack
  TrashUpTo { attack } -> attack
  GainCards { attack } -> attack
  Discard { attack } -> attack
  Draw { attack } -> attack

derive instance genericChoice :: Generic Choice _
derive instance eqChoice :: Eq Choice
instance showChoice :: Show Choice where show = genericShow
instance encodeJsonChoice :: EncodeJson Choice where
  encodeJson a = genericEncodeJson a
instance decodeJsonChoice :: DecodeJson Choice where
  decodeJson a = genericDecodeJson a

