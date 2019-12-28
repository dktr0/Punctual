{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Sound.Punctual.AsyncProgram where

-- thanks to toji for the Javascript model of asynchronous shader compilation!
-- cf. https://github.com/toji/shader-perf/blob/gh-pages/async-program.js
-- the model is adapted here to the somewhat different challenge of repeatedly
-- updated shader programs (eg. where use might be attempted quickly right
-- after new shaders are provided with an already existing program still available)

import GHCJS.Types
import GHCJS.DOM.Types hiding (Text)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Sound.Punctual.GL

data AsyncProgram = AsyncProgram {
  nextProgram :: Maybe WebGLProgram,
  nextVertexShader :: Maybe WebGLShader,
  nextFragmentShader :: Maybe WebGLShader,
  activeProgram :: Maybe WebGLProgram
  }

newAsyncProgram :: GL AsyncProgram
newAsyncProgram c = return $ AsyncProgram {
  nextProgram = Nothing,
  nextVertexShader = Nothing,
  nextFragmentShader = Nothing,
  activeProgram = Nothing
  }

updateAsyncProgram :: AsyncProgram -> Text -> Text -> GL AsyncProgram
updateAsyncProgram a vSrc fSrc = do
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
  return $ a {
    nextProgram = p,
    nextVertexShader = v,
    nextFragmentShader = f
  }

-- returns true if a new program is going to be used for the first time
-- eg. to indicate the new uniform/attrib locations exist
useAsyncProgram :: AsyncProgram -> GL (Bool,AsyncProgram)
useAsyncProgram a = do
  -- first check if we have an updated program that might be ready
  (newProgramUsed,a') <- if (isNothing $ nextProgram a) then (return (False,a)) else do
    let nextProgram' = fromJust $ nextProgram a
    
    ls <-
    case ls of
      0 ->

      otherwise ->

    -- three cases to consider:
    -- 1. if compilation/linking is still in progress, do nothing

    -- 2. if compilation/linking fails
    deleteProgram nextProgram' -- ? not clear if we can also delete the two shaders in this case?
    update so that nextProgram,nextFragmentShader, and nextVertexShader are all Nothing

    -- 2. if compilation/linking succeeds
    deleteShader $ fromJust $ nextVertexShader a
    deleteShader $ fromJust $ nextFragmentShader a
    when (isJust $ activeProgram a) $ deleteProgram $
    update so that activeProgram is nextProgram
    update so that nextProgram,nextFragmentShader, and nextVertexShader are all Nothing
    return (newProgramUsed,a')


  when (isJust $ activeProgram a) $ useProgram $ fromJust $ activeProgram a
  return (newProgramUsed,a')
