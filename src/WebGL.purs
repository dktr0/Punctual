module WebGL where

import Prelude ((<$>),bind,discard,pure,Unit,($),(<>),show,(-),unit,(+))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, new, write, read)
import Data.Maybe (Maybe(..))
import Data.Tempo (Tempo,origin,timeToCount)
import Effect.Now (nowDateTime)
import Data.Time.Duration (Milliseconds,Seconds)
import Data.DateTime (DateTime,diff)
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)
import Data.Rational (toNumber)
import Data.Map (Map, empty, lookup, insert, fromFoldable)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Set (toUnfoldable)
import Data.Unfoldable1 (range)
import Data.List (List,zip,length)

import Program (Program,programInfo)
import FragmentShader (fragmentShader)
import WebGLCanvas (WebGLBuffer, WebGLCanvas, WebGLProgram, attachShader, bindBufferArray, clearColor, clearColorBuffer, compileShader, createFragmentShader, createProgram, createVertexShader, deleteWebGLCanvas, drawDefaultTriangleStrip, enableVertexAttribArray, flush, getAttribLocation, linkProgram, newDefaultTriangleStrip, newWebGLCanvas, setUniform1f, setUniform2f, shaderSource, useProgram, vertexAttribPointer, viewport, WebGLTexture, activeTexture,bindTexture2D,setUniform1i,createTexture,WebGLContext, bindTexture, getFeedbackTexture, getOutputFrameBuffer, bindFrameBuffer, drawPostProgram)
import SharedResources (SharedResources,getTempo,getImage,updateWebcamTexture,Image,Video,getVideo)

type WebGL = {
  sharedResources :: SharedResources,
  glc :: WebGLCanvas,
  triangleStripBuffer :: WebGLBuffer,
  program :: Ref Program,
  shaderSrc :: Ref String,
  shader :: Ref WebGLProgram,
  imageTextures :: Ref (Map String WebGLTexture),
  videoTextures :: Ref (Map String WebGLTexture),
  imageTextureSlots :: Ref (Map String Int),
  videoTextureSlots :: Ref (Map String Int)
  }  

newWebGL :: SharedResources -> Program -> Program -> Effect (Maybe WebGL)
newWebGL sharedResources prog prevProg = do
  mglc <- newWebGLCanvas
  case mglc of 
    Just glc -> do
      triangleStripBuffer <- newDefaultTriangleStrip glc
      tempo <- getTempo sharedResources
      let (Tuple imgMap vidMap) = calculateTextureSlots prevProg prog
      Tuple shaderSrc' shader' <- updateFragmentShader glc tempo imgMap vidMap prevProg prog
      program <- new prog
      shaderSrc <- new shaderSrc'
      shader <- new shader'
      imageTextures <- new empty
      videoTextures <- new empty
      imageTextureSlots <- new imgMap
      videoTextureSlots <- new vidMap
      let webGL = {
        sharedResources,
        glc,
        triangleStripBuffer,
        program,
        shaderSrc,
        shader,
        imageTextures,
        videoTextures,
        imageTextureSlots,
        videoTextureSlots
        }
      pure $ Just webGL
    Nothing -> pure Nothing
    
updateWebGL :: WebGL -> Program -> Program -> Effect Unit
updateWebGL webGL program previousProgram = do
  tempo <- getTempo webGL.sharedResources
  let (Tuple imgMap vidMap) = calculateTextureSlots previousProgram program
  Tuple shaderSrc shader <- updateFragmentShader webGL.glc tempo imgMap vidMap previousProgram program 
  write program webGL.program
  write shaderSrc webGL.shaderSrc
  write shader webGL.shader
  write imgMap webGL.imageTextureSlots
  write vidMap webGL.videoTextureSlots
  
  
updateFragmentShader :: WebGLCanvas -> Tempo -> Map String Int -> Map String Int -> Program -> Program -> Effect (Tuple String WebGLProgram)
updateFragmentShader glc tempo imgMap vidMap oldProg newProg = do
  t0 <- nowDateTime
  let shaderSrc = fragmentShader glc.webGL2 tempo imgMap vidMap oldProg newProg
  t1 <- nowDateTime
  log $ " GLSL transpile time = " <> show (diff t1 t0 :: Milliseconds)
  glProg <- createProgram glc
  vShader <- createVertexShader glc
  attachShader glc glProg vShader
  shaderSource glc vShader "attribute vec4 p; void main() { gl_Position = p; }"
  compileShader glc vShader
  fShader <- createFragmentShader glc
  attachShader glc glProg fShader
  shaderSource glc fShader shaderSrc
  compileShader glc fShader
  linkProgram glc glProg
  flush glc
  pure $ Tuple shaderSrc glProg

  
calculateTextureSlots :: Program -> Program -> Tuple (Map String Int) (Map String Int)
calculateTextureSlots oldProg newProg = Tuple (fromFoldable $ zip imgURLs imgSlots) (fromFoldable $ zip vidURLs vidSlots)
  where
    oldProgInfo = programInfo oldProg
    newProgInfo = programInfo newProg
    imgURLs = (toUnfoldable $ (unwrap oldProgInfo).imgURLs <> (unwrap newProgInfo).imgURLs) :: List String
    imgSlots = range 4 15
    vidURLs = (toUnfoldable $ (unwrap oldProgInfo).vidURLs <> (unwrap newProgInfo).vidURLs) :: List String
    vidSlots = range (4 + length imgURLs) 15 
     
  
deleteWebGL :: WebGL -> Effect Unit
deleteWebGL webGL = deleteWebGLCanvas webGL.glc


drawWebGL :: WebGL -> DateTime -> Effect Unit
drawWebGL webGL now = do
  -- t0 <- nowDateTime
  let glc = webGL.glc
  shader <- read webGL.shader
  useProgram glc shader
  -- update time/tempo/resolution uniforms
  setUniform2f glc shader "res" 1920.0 1080.0
  tempo <- getTempo webGL.sharedResources
  setUniform1f glc shader "_time" $ unwrap (diff now (origin tempo) :: Seconds)
  eTime <- _.evalTime <$> read webGL.program
  setUniform1f glc shader "_etime" $ unwrap (diff now eTime :: Seconds)
  setUniform1f glc shader "_beat" $ toNumber $ timeToCount tempo now
  setUniform1f glc shader "_ebeat" $ toNumber $ timeToCount tempo now - timeToCount tempo eTime
  -- update audio analysis uniforms (TODO)
  -- update special textures (webcam, fft TODO, ifft TODO, feedback TODO)
  ft <- getFeedbackTexture glc
  bindTexture glc shader ft 0 "f"
  updateWebcamTexture webGL.sharedResources glc
  bindTexture glc shader glc.webcamTexture 3 "w"
  -- update image textures
  imgMap <- read webGL.imageTextureSlots
  _ <- traverseWithIndex (bindImageTexture webGL shader) imgMap  
  -- update video textures
  vidMap <- read webGL.videoTextureSlots
  _ <- traverseWithIndex (bindVideoTexture webGL shader) vidMap
  -- draw
  pLoc <- getAttribLocation glc shader "p"
  bindBufferArray glc webGL.triangleStripBuffer
  vertexAttribPointer glc pLoc
  enableVertexAttribArray glc pLoc
  viewport glc 0 0 1920 1080 -- placeholder, need to read current width and height
  -- clearColor glc 0.0 0.0 0.0 0.0
  -- clearColorBuffer glc
  ofb <- getOutputFrameBuffer glc
  bindFrameBuffer glc (Just ofb)
  drawDefaultTriangleStrip glc
  drawPostProgram glc
  -- t1 <- nowDateTime
  -- log $ " draw time = " <> show (diff t1 t0 :: Milliseconds)
  pure unit


bindImageTexture :: WebGL -> WebGLProgram -> String -> Int -> Effect Unit
bindImageTexture webGL shader url n = do
  mTexture <- getImageTexture webGL url
  case mTexture of
    Just t -> bindTexture webGL.glc shader t n ("t" <> show n)
    Nothing -> pure unit

getImageTexture :: WebGL -> String -> Effect (Maybe WebGLTexture)
getImageTexture webGL url = do
  imageTextures <- read webGL.imageTextures
  case lookup url imageTextures of
    Just t -> pure $ Just t
    Nothing -> do
      mImg <- getImage webGL.sharedResources url
      case mImg of
        Just img -> do
          t <- createTexture webGL.glc
          _imageToTexture webGL.glc.gl img t  
          write (insert url t imageTextures) webGL.imageTextures
          pure $ Just t
        Nothing -> pure Nothing
        
foreign import _imageToTexture :: WebGLContext -> Image -> WebGLTexture -> Effect Unit


bindVideoTexture :: WebGL -> WebGLProgram -> String -> Int -> Effect Unit
bindVideoTexture webGL shader url n = do
  mTexture <- getVideoTexture webGL url
  case mTexture of
    Just t -> bindTexture webGL.glc shader t n ("t" <> show n)
    Nothing -> pure unit

getVideoTexture :: WebGL -> String -> Effect (Maybe WebGLTexture)
getVideoTexture webGL url = do
  videoTextures <- read webGL.videoTextures
  case lookup url videoTextures of
    Just t -> do
      mVid <- getVideo webGL.sharedResources url
      case mVid of
        Nothing -> pure Nothing
        Just vid -> do
          _videoToTexture webGL.glc.gl vid t      
          pure $ Just t
    Nothing -> do
      mVid <- getVideo webGL.sharedResources url
      case mVid of
        Just vid -> do
          t <- createTexture webGL.glc
          _videoToTexture webGL.glc.gl vid t  
          write (insert url t videoTextures) webGL.videoTextures
          pure $ Just t
        Nothing -> pure Nothing
          
foreign import _videoToTexture :: WebGLContext -> Video -> WebGLTexture -> Effect Unit


  
  
  
  




