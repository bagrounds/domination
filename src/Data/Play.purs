module Domination.Data.Play where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Lens.Lens (Lens', lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Domination.Data.Choice (Choice)
import Domination.Data.Reaction (Reaction)
import Type.Proxy (Proxy(..))

data Play
  = EndPhase { playerIndex :: Int }
  | PlayCard { playerIndex :: Int, cardIndex :: Int }
  | Purchase { playerIndex :: Int,  stackIndex :: Int }
  | ResolveChoice { playerIndex :: Int, choice :: Choice }
  | React { playerIndex :: Int, reaction :: Maybe Reaction }
  | DoneReacting { playerIndex :: Int }

_playerIndex' :: forall r. Lens' { playerIndex :: Int | r } Int
_playerIndex' = prop (Proxy :: Proxy "playerIndex")

_playerIndex :: Lens' Play Int
_playerIndex = lens' \s -> case s of
  EndPhase x -> Tuple
    x.playerIndex
    (EndPhase <<< x { playerIndex = _ })
  PlayCard x -> Tuple
    x.playerIndex
    (PlayCard <<< x { playerIndex = _ })
  Purchase x -> Tuple
    x.playerIndex
    (Purchase <<< x { playerIndex = _ })
  ResolveChoice x -> Tuple
    x.playerIndex
    (ResolveChoice <<< x { playerIndex = _ })
  React x -> Tuple
    x.playerIndex
    (React <<< x { playerIndex = _ })
  DoneReacting x -> Tuple
    x.playerIndex
    (DoneReacting <<< x { playerIndex = _ })

derive instance genericPlay :: Generic Play _
derive instance eqPlay :: Eq Play
instance encodeJsonPlay :: EncodeJson Play where
  encodeJson = genericEncodeJson
instance decodeJsonPlay :: DecodeJson Play where
  decodeJson = genericDecodeJson
instance showPlay :: Show Play where
  show = genericShow

