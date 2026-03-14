module Audio.WebAudio.BaseAudioContext
  ( newAudioContext
  , destination
  , currentTime
  , resume
  , createGain
  , createOscillator
  ) where

import Prelude (Unit)
import Effect (Effect)
import Audio.WebAudio.Types (AudioContext, DestinationNode, GainNode, OscillatorNode)

foreign import newAudioContext :: Effect AudioContext
foreign import destination :: AudioContext -> Effect DestinationNode
foreign import currentTime :: AudioContext -> Effect Number
foreign import resume :: AudioContext -> Effect Unit
foreign import createGain :: AudioContext -> Effect GainNode
foreign import createOscillator :: AudioContext -> Effect OscillatorNode
