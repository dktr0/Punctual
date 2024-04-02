module WebAudio where

import Prelude (Unit)
import Effect (Effect)

foreign import data WebAudioContext :: Type

foreign import defaultWebAudioContext :: Effect WebAudioContext

foreign import resumeWebAudioContext :: WebAudioContext -> Effect Unit

foreign import data WebAudioNode :: Type

-- foreign import _monoGainNode :: WebAudioContext -> Number -> Effect WebAudioNode

foreign import gainNode :: WebAudioContext -> Number -> Effect WebAudioNode

foreign import _analyserNode :: WebAudioContext -> Int -> Number -> Effect WebAudioNode

foreign import _defaultAudioInputNode :: WebAudioContext -> Effect WebAudioNode

foreign import _connect :: WebAudioNode -> WebAudioNode -> Effect Unit

foreign import _disconnect :: WebAudioNode -> WebAudioNode -> Effect Unit

