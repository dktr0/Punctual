{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.AsyncProgram where

-- thanks to toji for the Javascript model of asynchronous shader compilation!
-- cf. https://github.com/toji/shader-perf/blob/gh-pages/async-program.js
-- the model is adapted here to the somewhat different challenge of repeatedly
-- updated shader programs (eg. where use might be attempted quickly right
-- after new shaders are provided with an already existing program still available)

import GHCJS.DOM.Types hiding (Text)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Map.Strict as Map
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import TextShow

import Sound.Punctual.GL

data AsyncProgram = AsyncProgram {
  nextProgram :: Maybe WebGLProgram,
  nextVertexShader :: Maybe WebGLShader,
  nextFragmentShader :: Maybe WebGLShader,
  nextProgramCountDown :: Int,
  activeProgram :: Maybe WebGLProgram,
  activeVertexShader :: Maybe WebGLShader,
  activeFragmentShader :: Maybe WebGLShader,
  uniformsMap :: Map Text WebGLUniformLocation,
  attribsMap :: Map Text Int
  }

emptyAsyncProgram :: AsyncProgram
emptyAsyncProgram = AsyncProgram {
  nextProgram = Nothing,
  nextVertexShader = Nothing,
  nextFragmentShader = Nothing,
  nextProgramCountDown = 0,
  activeProgram = Nothing,
  activeVertexShader = Nothing,
  activeFragmentShader = Nothing,
  uniformsMap = empty,
  attribsMap = empty
  }

updateAsyncProgram :: AsyncProgram -> Text -> Text -> GL AsyncProgram
updateAsyncProgram a vSrc fSrc = do
  when (isJust $ nextProgram a) $ do
    deleteProgram $ fromJust $ nextProgram a
    deleteShader $ fromJust $ nextVertexShader a
    deleteShader $ fromJust $ nextFragmentShader a
  p <- createProgram
  v <- createVertexShader
  attachShader p v
  shaderSource v vSrc
  compileShader v
  f <- createFragmentShader
  attachShader p f
  shaderSource f fSrc
  compileShader f
  linkProgram p
  Sound.Punctual.GL.flush
  psc <- khr_parallel_shader_compile
  let cd = if psc then 0 else 30 -- if we don't have parallel_shader_compile extension, wait 30 frames before using new program
  return $ a {
    nextProgram = Just p,
    nextVertexShader = Just v,
    nextFragmentShader = Just f,
    nextProgramCountDown = cd
  }

-- returns true if a new program is going to be used for the first time
-- eg. to indicate the new uniform/attrib locations exist
useAsyncProgram :: AsyncProgram -> [Text] -> [Text] -> GL (Bool,AsyncProgram)
useAsyncProgram a uniformNames attribNames = do
  -- first check if we have an updated program that might be ready
  let countDown = nextProgramCountDown a
  if ((isNothing $ nextProgram a) || countDown > 0) then do
    when (isJust $ activeProgram a) $ useProgram (fromJust (activeProgram a))
    return (False,a {
      nextProgramCountDown = max 0 (countDown - 1)
    })
  else do
    let nextProgram' = fromJust $ nextProgram a
    cs <- getProgramParameterCompletionStatus nextProgram'
    case cs of
      False -> do
        when (isJust $ activeProgram a) $ useProgram (fromJust (activeProgram a))
        return (False,a)
      True -> do
        ls <- linkStatus nextProgram'
        case ls of
          0 -> do -- link failed
            liftIO $ T.putStrLn "***WEBGL LINK FAILED"
            let vs = fromJust $ nextVertexShader a
            let fs = fromJust $ nextFragmentShader a
            vsStatus <- getShaderParameterCompileStatus vs
            vsLog <- getShaderInfoLog vs
            liftIO $ T.putStrLn $ " vertex shader status=" <> showt vsStatus <> " log: " <> vsLog
            fsStatus <- getShaderParameterCompileStatus fs
            fsLog <- getShaderInfoLog fs
            liftIO $ T.putStrLn $ " fragment shader status=" <> showt fsStatus <> " log: " <> fsLog
            pLog <- getProgramInfoLog nextProgram'
            liftIO $ T.putStrLn $ " program log: " <> pLog
            when (isJust $ activeProgram a) $ useProgram (fromJust (activeProgram a))
            return (False, a {
              nextProgram = Nothing,
              nextVertexShader = Nothing,
              nextFragmentShader = Nothing
            })
          _ -> do -- delete old program, use new program, query inform locations
            when (isJust $ activeProgram a) $ do
              deleteProgram $ fromJust $ activeProgram a
              deleteShader $ fromJust $ activeVertexShader a
              deleteShader $ fromJust $ activeFragmentShader a
            useProgram nextProgram'
            newUniformsMap <- mapM (getUniformLocation nextProgram') $ fromList $ fmap (\x -> (x,x)) uniformNames
            newAttribsMap <- mapM (getAttribLocation nextProgram') $ fromList $ fmap (\x -> (x,x)) attribNames
            return (True, a {
              activeProgram = Just nextProgram',
              activeVertexShader = nextVertexShader a,
              activeFragmentShader = nextFragmentShader a,
              nextProgram = Nothing,
              nextVertexShader = Nothing,
              nextFragmentShader = Nothing,
              uniformsMap = newUniformsMap,
              attribsMap = newAttribsMap
            })

uniform1fAsync :: AsyncProgram -> Text -> Double -> GL ()
uniform1fAsync a n v = do
  let loc = Map.lookup n $ uniformsMap a
  maybe (error "*** uniform not found in map ***") (\x -> uniform1f x v) loc

uniform2fAsync :: AsyncProgram -> Text -> Double -> Double -> GL ()
uniform2fAsync a n v1 v2 = do
  let loc = Map.lookup n $ uniformsMap a
  maybe (error "*** uniform not found in map ***") (\x -> uniform2f x v1 v2) loc
