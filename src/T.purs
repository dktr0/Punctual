module T where

import Prelude (bind,discard,pure,($),Unit,(<>),(+),show)
import Control.Monad.State (State, put, get, gets, runState, modify_)
import Data.Map (Map)
import Data.Tuple (Tuple)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList, cons, singleton)

import Signal (Signal(..),Sample,showVideo)
import SignalInfo (emptySignalInfo)

type TextureMap = { imgs :: Map String Int, vids :: Map String Int, gdms :: Map String Int }

type TState = {

  -- state shared by audio and video transpilation
  _time :: Sample,
  _beat :: Sample,
  _etime :: Sample,
  _ebeat :: Sample,
  
  -- state specific to WebGL shaders
  _fx :: Sample,
  _fy :: Sample,
  _webGl2 :: Boolean,
  _textureMap :: TextureMap,
  allocatedShaderVariables :: Int,
  shader :: String,
  
  -- state specific to Web audio worklets
  allocatedAudioFloats :: Int,
  allocatedAudioInts :: Int,
  audio :: String  
  }

runT :: forall a. Boolean -> TextureMap -> T a -> Tuple a TState
runT _webGl2 _textureMap x = runState x {
  _time: Signal { audio: Right "time", video: Right "_time", signalInfo: emptySignalInfo },
  _beat: Signal { audio: Right "beat", video: Right "_beat", signalInfo: emptySignalInfo },
  _etime: Signal { audio: Right "eTime", video: Right "_etime", signalInfo: emptySignalInfo },
  _ebeat: Signal { audio: Right "eBeat", video: Right "_ebeat", signalInfo: emptySignalInfo },
  _fx: Signal { audio: Left 0.0, video: Right "fx", signalInfo: emptySignalInfo },
  _fy: Signal { audio: Left 0.0, video: Right "fy", signalInfo: emptySignalInfo }, 
  _webGl2,
  _textureMap,
  allocatedShaderVariables: 0,
  shader: "",
  allocatedAudioFloats: 0,
  allocatedAudioInts: 0,
  audio: ""
  }
-- note: above assumes the following fragment shader headers (probably not in place right now):
-- float fx = (gl_FragCoord.x/res.x)*2.-1.;
-- float fy = (gl_FragCoord.y/res.y)*2.-1.;
-- TODO: need to also add new definitions to Punctual, afx and afy for accessing absolute fx and fy

-- a monad that can read audio worklet and fragment shader context, and write/accumulate code to both
type T = State TState


-- reading context

time :: T Sample
time = gets _._time

beat :: T Sample
beat = gets _._beat

etime :: T Sample
etime = gets _._etime

ebeat :: T Sample
ebeat = gets _._ebeat

fx :: T Sample
fx = gets _._fx

fy :: T Sample
fy = gets _._fy

webGl2 :: T Boolean
webGl2 = gets _._webGl2

textureMap :: T TextureMap
textureMap = gets _._textureMap


-- writing/accumulating worklet/shader code

writeAudio :: String -> T Unit
writeAudio x = modify_ $ \s -> s { audio = s.audio <> x } 

writeShader :: String -> T Unit
writeShader x = modify_ $ \s -> s { shader = s.shader <> x } 


-- allocating worklet and shader variables

allocateAudioFloat :: T String
allocateAudioFloat = do
  s <- get
  put $ s { allocatedAudioFloats = s.allocatedAudioFloats + 1 }
  pure $ "f[" <> show s.allocatedAudioFloats <> "]"

allocateAudioFloats :: Int -> T Int
allocateAudioFloats n = do
  s <- get
  put $ s { allocatedAudioFloats = s.allocatedAudioFloats + n }
  pure s.allocatedAudioFloats

allocateShaderVariable :: T String
allocateShaderVariable = do
  s <- get
  put $ s { allocatedShaderVariables = s.allocatedShaderVariables + 1 }
  pure $ "_" <> show s.allocatedShaderVariables

allocateAudioInt :: T String
allocateAudioInt = do
  s <- get
  put $ s { allocatedAudioInts = s.allocatedAudioInts + 1 }
  pure $ "i[" <> show s.allocatedAudioInts <> "]"


-- assignment to worklet/shader variables

assign :: Sample -> T Sample
assign (Signal x) = do
  a <- case x.audio of
         Left c -> pure (Left c)
         Right s -> do
           f <- allocateAudioFloat
           writeAudio $ f <> "=" <> s <> ";\n"
           pure (Right f)
  v <- case x.video of
         Left c -> pure (Left c)
         Right s -> do
           f <- allocateShaderVariable
           writeShader $ "float " <> f <> "=" <> s <> ";\n"
           pure (Right f)
  pure $ Signal { audio: a, video: v, signalInfo: x.signalInfo }

assignAudioInt :: String -> T String
assignAudioInt s = do
  i <- allocateAudioInt
  writeAudio $ i <> "=" <> s <> ";\n"
  pure i

assignShaderVec3 :: String -> T String
assignShaderVec3 s = do
  v3 <- allocateShaderVariable
  writeShader $ "vec3 " <> v3 <> "=" <> s <> ";\n"
  pure v3

assignShaderVec4 :: String -> T String
assignShaderVec4 s = do
  v4 <- allocateShaderVariable
  writeShader $ "vec4 " <> v4 <> "=" <> s <> ";\n"
  pure v4


-- embedding calculations in the monad with modified state

withModifiedState :: forall a. (TState -> TState) -> T a -> T a
withModifiedState f ta = do
  cachedState <- get
  modify_ f
  r <- ta
  put cachedState
  pure r

withFx :: forall a. Sample -> T a -> T a
withFx newFx ta = withModifiedState (\s -> s { _fx = newFx }) ta

withFy :: forall a. Sample -> T a -> T a
withFy newFy ta = withModifiedState (\s -> s { _fy = newFy }) ta

{-

-- ...not sure what the best form for this is yet...
withFxys :: forall a. Expr a => NonEmptyList Vec2 -> G (Matrix a) -> G (Matrix a)
withFxys fxys a = do
  cachedFxy <- _.fxy <$> get
  rs <- for fxys $ \newFxy -> do
    modify_ $ \s -> s { fxy = newFxy }
    a
  modify_ $ \s -> s { fxy = cachedFxy }
  pure $ concat rs

-- ...not sure what the best form for this is yet...
withAlteredTime :: forall a. Expr a => NonEmptyList { time :: Float, beat :: Float, etime :: Float, ebeat :: Float } -> G (Matrix a) -> G (Matrix a)
withAlteredTime xs a = do
  cached <- get
  rs <- for xs $ \x -> do
    modify_ $ \s -> s { time = x.time, beat = x.beat, etime = x.etime, ebeat = x.ebeat }
    a
  modify_ $ \s -> s { time = cached.time, beat = cached.beat, etime = cached.etime, ebeat = cached.ebeat }
  pure $ concat rs
-}

-- low-level WebGL texture functionality

texture :: String -> String -> T String
texture texName xy = do
  gl2 <- webGl2
  case gl2 of 
    true -> pure $ "texture(" <> texName <> "," <> xy <> ")"
    false -> pure $ "texture2D(" <> texName <> "," <> xy <> ")"

textureFFT :: T Sample
textureFFT = do
  x <- fx
  t <- texture "o" $ "vec2(" <> showVideo x <> ",0.)"
  assign $ Signal { audio: Left 0.0, video: Right $ t <> ".x", signalInfo: emptySignalInfo { fft = pure true } }

textureIFFT :: T Sample
textureIFFT = do
  x <- fx
  t <- texture "i" $ "vec2(" <> showVideo x <> ",0.)"
  assign $ Signal { audio: Left 0.0, video: Right $ t <> ".x",  signalInfo: emptySignalInfo { ifft = pure true } }

-- note: replacement for texture2Da from 0.5.2 (former module G)
-- note ALSO: this is not setting signalInfo in any way to reflect texture dependency (should it perhaps?)...
textureVec4 :: String -> T (NonEmptyList Sample)
textureVec4 texName = do
  x <- fx
  y <- fy
  t <- texture texName $ "vec2(" <> showVideo x <> "," <> showVideo y <> ")"
  v4 <- assignShaderVec4 t
  let c1 = Signal { audio: Left 0.0, video: Right $ v4 <> ".x", signalInfo: emptySignalInfo }
  let c2 = Signal { audio: Left 0.0, video: Right $ v4 <> ".y", signalInfo: emptySignalInfo }
  let c3 = Signal { audio: Left 0.0, video: Right $ v4 <> ".z", signalInfo: emptySignalInfo }
  let c4 = Signal { audio: Left 0.0, video: Right $ v4 <> ".w", signalInfo: emptySignalInfo }
  pure $ c1 `cons` (c2 `cons` (c3 `cons` singleton c4))

-- note: replacement for texture2D from 0.5.2 (former module G)
-- note ALSO: this is not setting signalInfo in any way to reflect texture dependency (should it perhaps?)...
textureVec3 :: String -> T (NonEmptyList Sample)
textureVec3 texName = do
  x <- fx
  y <- fy
  t <- texture texName $ "vec2(" <> showVideo x <> "," <> showVideo y <> ")"
  v4 <- assignShaderVec3 $ t <> ".xyz"
  let c1 = Signal { audio: Left 0.0, video: Right $ v4 <> ".x", signalInfo: emptySignalInfo }
  let c2 = Signal { audio: Left 0.0, video: Right $ v4 <> ".y", signalInfo: emptySignalInfo }
  let c3 = Signal { audio: Left 0.0, video: Right $ v4 <> ".z", signalInfo: emptySignalInfo }
  pure $ c1 `cons` (c2 `cons` singleton c3)
