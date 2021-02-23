module Domination.Capability.Audio where

import Prelude

import Audio.WebAudio.AudioParam (setValueAtTime)
import Audio.WebAudio.BaseAudioContext (createOscillator, destination, newAudioContext)
import Audio.WebAudio.Oscillator (frequency, startOscillator, stopOscillator)
import Audio.WebAudio.Types (connect)
import Control.Monad.Trans.Class (lift)
import Data.Array (drop, head)
import Data.Maybe (Maybe(..))
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)

class Monad m <= Audio m where
  note :: Number -> Number -> Number -> m Unit

instance audioHalogenM
  :: Audio m => Audio (HalogenM st act slots msg m) where
  note f s = lift <<< note f s

instance audioAppM :: Audio AppM where
  note f s = liftEffect <<< note' f s

newtype AudioM a = AudioM (Effect a)

derive newtype instance functorAudioM :: Functor AudioM
derive newtype instance applyAudioM :: Apply AudioM
derive newtype instance applicativeAudioM :: Applicative AudioM
derive newtype instance bindAudioM :: Bind AudioM
derive newtype instance monadAudioM :: Monad AudioM
derive newtype instance monadEffectAudioM :: MonadEffect AudioM

instance audioAudioM :: Audio AudioM where
  note f s = liftEffect <<< note' f s

runAudioM :: AudioM ~> Effect
runAudioM (AudioM m) = liftEffect m

beep :: forall m. Audio m => m Unit
beep = note a4 0.0 0.05

note'
  :: Number
  -> Number
  -> Number
  -> Effect Unit
note' freq start stop = do
  audioContext <- newAudioContext
  oscillatorNode <- createOscillator audioContext
  f <- frequency oscillatorNode
  value <- setValueAtTime freq start f
  dest <- destination audioContext
  connect oscillatorNode dest
  startOscillator start oscillatorNode
  stopOscillator stop oscillatorNode

arpeggio
  :: forall m
  . Audio m
  => Array Number
  -> Number
  -> Number
  -> Number
  -> m Unit
arpeggio notes on off start =
  case head notes of
    Nothing -> pure unit
    Just head -> do
      let tail = drop 1 notes
      note head start (start + on)
      arpeggio tail on off (start + on + off)

c5_1'4'5 :: forall m. Audio m => m Unit
c5_1'4'5 = do
  arpeggio c5Major 0.1 0.0 0.0
  arpeggio f5Major 0.1 0.0 0.3
  arpeggio g5Major 0.1 0.0 0.6

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

a4 :: Number
a4 = 440.0
b4 :: Number
b4 = 493.88
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

