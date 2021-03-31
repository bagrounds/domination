module Domination.Capability.Audio where

import Prelude hiding (compose)

import Audio.WebAudio.BaseAudioContext (createGain, createOscillator, currentTime, destination, resume)
import Audio.WebAudio.BaseAudioContext as BaselineAudioContext
import Audio.WebAudio.GainNode (setGain)
import Audio.WebAudio.Oscillator (setFrequency, startOscillator, stopOscillator)
import Audio.WebAudio.Types (AudioContext, connect)
import Control.Monad.Trans.Class (lift)
import Data.Array (drop, head)
import Data.Maybe (Maybe(..))
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)

class Monad m <= Audio m where
  note :: AudioContext -> Number -> Number -> Number -> m Unit
  newAudioContext :: m AudioContext

instance audioHalogenM
  :: Audio m => Audio (HalogenM st act slots msg m) where
  note c f s = lift <<< note c f s
  newAudioContext = lift newAudioContext

instance audioAppM :: Audio AppM where
  note c f s = liftEffect <<< note' c f s
  newAudioContext = liftEffect BaselineAudioContext.newAudioContext

newtype AudioM a = AudioM (Effect a)

derive newtype instance functorAudioM :: Functor AudioM
derive newtype instance applyAudioM :: Apply AudioM
derive newtype instance applicativeAudioM :: Applicative AudioM
derive newtype instance bindAudioM :: Bind AudioM
derive newtype instance monadAudioM :: Monad AudioM
derive newtype instance monadEffectAudioM :: MonadEffect AudioM

instance audioAudioM :: Audio AudioM where
  note c f s = liftEffect <<< note' c f s
  newAudioContext = liftEffect BaselineAudioContext.newAudioContext

runAudioM :: AudioM ~> Effect
runAudioM (AudioM m) = liftEffect m

data Sound
  = Acknowledge
  | Purchase
  | Error
  | Attacked

beep :: forall m. Audio m => AudioContext -> Sound -> m Unit
beep context = case _ of
  Acknowledge -> note context c5 0.0 0.05
  Purchase -> marioCoin context
  Error -> note context ab4 0.0 0.05
  Attacked -> suspense context

note'
  :: AudioContext
  -> Number
  -> Number
  -> Number
  -> Effect Unit
note' context freq start stop = do
  note'' freq start stop context

note''
  :: Number
  -> Number
  -> Number
  -> AudioContext
  -> Effect Unit
note'' freq start stop audioContext = do
  oscillatorNode <- createOscillator audioContext
  setFrequency freq oscillatorNode
  gainNode <- createGain audioContext
  setGain 0.1 gainNode
  connect oscillatorNode gainNode
  now <- currentTime audioContext
  startOscillator (now + start) oscillatorNode
  stopOscillator (now + stop) oscillatorNode
  dest <- destination audioContext
  connect gainNode dest
  resume audioContext

compose
  :: forall m
  . Audio m
  => AudioContext
  -> Array
    { frequency :: Number
    , duration :: Number
    , start :: Number
    }
  -> m Unit
compose context notes =
  case head notes of
    Nothing -> pure unit
    Just { frequency, duration, start } -> do
      let tail = drop 1 notes
      note context frequency start (start + duration)
      compose context tail

arpeggio
  :: forall m
  . Audio m
  => AudioContext
  -> Array Number
  -> Number
  -> Number
  -> Number
  -> m Unit
arpeggio context notes on off start =
  case head notes of
    Nothing -> pure unit
    Just head -> do
      let tail = drop 1 notes
      note context head start (start + on)
      arpeggio context tail on off (start + on + off)

c5_1'4'5 :: forall m. Audio m => AudioContext -> m Unit
c5_1'4'5 context = do
  arpeggio context c5Major 0.1 0.0 0.0
  arpeggio context f5Major 0.1 0.0 0.3
  arpeggio context g5Major 0.1 0.0 0.6

marioCoin :: forall m. Audio m => AudioContext -> m Unit
marioCoin context = compose context
  [ { frequency: b5, duration: 0.1, start: 0.0 }
  , { frequency: e6, duration: 0.2, start: 0.1 }
  ]

suspense :: forall m. Audio m => AudioContext -> m Unit
suspense context = compose context
  [ { frequency: eb4, duration: 0.1, start: 0.0 }
  , { frequency: c4, duration: 0.1, start: 0.2 }
  , { frequency: fH4, duration: 0.3, start: 0.4 }
  ]

a4Minor :: Array Number
a4Minor = [a4, c5, e5]

a4Major :: Array Number
a4Major = [a4, cH5, e5]

c5Major :: Array Number
c5Major = [c5, e5, g5]

f5Major :: Array Number
f5Major = [f5, a5, c6]

g5Major :: Array Number
g5Major = [g5, b5, d6]

fH3 :: Number
fH3 = 185.00
c4 :: Number
c4 = 261.63
eb4 :: Number
eb4 = 311.13
ab4 :: Number
ab4 = 415.30
a4 :: Number
a4 = 440.0
b4 :: Number
b4 = 493.88
fH4 :: Number
fH4 = 369.99
c5 :: Number
c5 = 523.25
cH5 :: Number
cH5 = 554.37
d5 :: Number
d5 = 587.33
e5 :: Number
e5 = 659.25
f5 :: Number
f5 = 698.46
g5 :: Number
g5 = 783.99
a5 :: Number
a5 = 880.00
b5 :: Number
b5 = 987.77
c6 :: Number
c6 = 1046.50
e6 :: Number
e6 = 1318.51
g6 :: Number
g6 = 1567.98
a6 :: Number
a6 = 1760.00
b6 :: Number
b6 = 1975.53

d6 :: Number
d6 = 1174.66

c7 :: Number
c7 = 2093.00
d7 :: Number
d7 = 2349.32

