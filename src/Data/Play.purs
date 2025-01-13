--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| This module defines data types and functions for handling game state in a card game.
--|
--| ### Key Concepts
--| * **Data types**: Understanding the `Play` data type and its variants (`NewGame`, `EndPhase`, etc.).
--| * **Lens**: Familiarity with the `Lens'` concept, specifically the `_playerIndex'` lens.
--| * **Traversal**: Understanding how to use the `wander` function to create a traversal for traversing nested fields in the `Play` data type.

module Domination.Data.Play where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens.Internal.Wander (wander)
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Domination.Data.Card (Card)
import Domination.Data.Choice (Choice)
import Domination.Data.Reaction (Reaction)

data Play
  = NewGame
    { playerCount :: Int
    , supply :: Array Card
    , longGame :: Boolean
    }
  | EndPhase { playerIndex :: Int }
  | PlayCard { playerIndex :: Int, cardIndex :: Int }
  | Purchase { playerIndex :: Int,  stackIndex :: Int }
  | ResolveChoice { playerIndex :: Int, choice :: Choice }
  | React { playerIndex :: Int, reaction :: Maybe Reaction }

_playerIndex' :: forall r. Lens' { playerIndex :: Int | r } Int
_playerIndex' = prop (SProxy :: SProxy "playerIndex")

_playerIndex :: Traversal' Play Int
_playerIndex = wander \f s -> case s of
  NewGame _ -> pure s
  EndPhase x -> EndPhase <$> traverseOf _playerIndex' f x
  PlayCard x -> PlayCard <$> traverseOf _playerIndex' f x
  Purchase x -> Purchase <$> traverseOf _playerIndex' f x
  ResolveChoice x -> ResolveChoice <$> traverseOf _playerIndex' f x
  React x -> React <$> traverseOf _playerIndex' f x

derive instance genericPlay :: Generic Play _
derive instance eqPlay :: Eq Play
instance encodeJsonPlay :: EncodeJson Play where
  encodeJson = genericEncodeJson
instance decodeJsonPlay :: DecodeJson Play where
  decodeJson = genericDecodeJson
instance showPlay :: Show Play where
  show = genericShow
