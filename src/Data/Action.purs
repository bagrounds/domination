module Domination.Data.Action where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.))
import Data.Lens.Iso (Iso', iso)
import Domination.Data.WireInt (WireInt, _WireInt)
import Domination.UI.Icons as Icons
import Halogen.HTML (text) as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Elements (span_) as HH
import Util ((.^))

newtype Action = Action WireInt

derive newtype instance eqAction :: Eq Action
derive instance genericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
instance encodeJsonAction :: EncodeJson Action where
  encodeJson = genericEncodeJson
instance decodeJsonAction :: DecodeJson Action where
  decodeJson = genericDecodeJson
derive newtype instance encodeArrayBufferAction
  :: EncodeArrayBuffer Action
derive newtype instance decodeArrayBufferAction
  :: DecodeArrayBuffer Action
derive newtype instance dynamicByteLengthAction
  :: DynamicByteLength Action

_wireInt :: Iso' Action WireInt
_wireInt = iso (\(Action w) -> w) Action

_int :: Iso' Action Int
_int = iso to from where
  to (Action wireInt) = wireInt .^ _WireInt
  from = (_ ^. _WireInt) >>> (_ .^ _wireInt)

renderHTML :: forall w i. Action -> HTML w i
renderHTML action = HH.span_
  [ HH.text $ show (action ^. _int)
  , Icons.actions
  ]

