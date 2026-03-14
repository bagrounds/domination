module Audio.WebAudio.Oscillator (setFrequency, startOscillator, stopOscillator) where

import Prelude (Unit, (=<<))
import Effect (Effect)
import Audio.WebAudio.Types (AudioParam, OscillatorNode)
import Audio.WebAudio.AudioParam (setValue)

foreign import frequency :: OscillatorNode -> Effect AudioParam

setFrequency :: Number -> OscillatorNode -> Effect Unit
setFrequency num node =
  setValue num =<< frequency node

foreign import startOscillator :: Number -> OscillatorNode -> Effect Unit
foreign import stopOscillator :: Number -> OscillatorNode -> Effect Unit
