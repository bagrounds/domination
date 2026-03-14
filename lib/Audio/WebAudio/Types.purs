module Audio.WebAudio.Types
  ( AudioContext
  , AudioParam
  , DestinationNode
  , GainNode
  , OscillatorNode
  , connect
  ) where

import Prelude (Unit)
import Effect (Effect)

foreign import data AudioContext :: Type
foreign import data AudioParam :: Type
foreign import data DestinationNode :: Type
foreign import data GainNode :: Type
foreign import data OscillatorNode :: Type

foreign import connectImpl :: forall a b. a -> b -> Effect Unit

connect :: forall a b. a -> b -> Effect Unit
connect = connectImpl
