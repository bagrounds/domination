module Audio.WebAudio.AudioParam (Value, setValue) where

import Prelude (Unit)
import Effect (Effect)
import Audio.WebAudio.Types (AudioParam)

type Value = Number

foreign import setValue :: Value -> AudioParam -> Effect Unit
