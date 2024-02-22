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
import Data.List (List,zip)

import Signal (SignalInfo(..))
import Program (Program,programInfo)
import FragmentShader (fragmentShader)
import WebGLCanvas (WebGLBuffer, WebGLCanvas, WebGLProgram, attachShader, bindBufferArray, clearColor, clearColorBuffer, compileShader, createFragmentShader, createProgram, createVertexShader, deleteWebGLCanvas, drawDefaultTriangleStrip, enableVertexAttribArray, flush, getAttribLocation, linkProgram, newDefaultTriangleStrip, newWebGLCanvas, setUniform1f, setUniform2f, shaderSource, useProgram, vertexAttribPointer, viewport, WebGLTexture, activeTexture,bindTexture2D,setUniform1i,createTexture,WebGLContext)
import SharedResources (SharedResources,getTempo,getImage,updateWebcamTexture,Image)

type WebGL = {
  sharedResources :: SharedResources,
  glc :: WebGLCanvas,
  triangleStripBuffer :: WebGLBuffer,
  program :: Ref Program,
  shaderSrc :: Ref String,
  shader :: Ref WebGLProgram,
  imageTextures :: Ref (Map String WebGLTexture),
  imgMap :: Ref (Map String Int)
  }  

newWebGL :: SharedResources -> Program -> Program -> Effect (Maybe WebGL)
newWebGL sharedResources prog prevProg = do
  mglc <- newWebGLCanvas
  case mglc of 
    Just glc -> do
      triangleStripBuffer <- newDefaultTriangleStrip glc
      tempo <- getTempo sharedResources
      let imgMap' = calculateImageMap prevProg prog
      Tuple shaderSrc' shader' <- updateFragmentShader glc tempo prevProg prog imgMap'
      program <- new prog
      shaderSrc <- new shaderSrc'
      shader <- new shader'
      imageTextures <- new empty
      imgMap <- new imgMap'
      let webGL = {
        sharedResources,
        glc,
        triangleStripBuffer,
        program,
        shaderSrc,
        shader,
        imageTextures,
        imgMap
        }
      pure $ Just webGL
    Nothing -> pure Nothing
    
updateWebGL :: WebGL -> Program -> Program -> Effect Unit
updateWebGL webGL program previousProgram = do
  tempo <- getTempo webGL.sharedResources
  let imgMap = calculateImageMap previousProgram program
  Tuple shaderSrc shader <- updateFragmentShader webGL.glc tempo previousProgram program imgMap
  write program webGL.program
  write shaderSrc webGL.shaderSrc
  write shader webGL.shader
  write imgMap webGL.imgMap
  
  
updateFragmentShader :: WebGLCanvas -> Tempo -> Program -> Program -> Map String Int -> Effect (Tuple String WebGLProgram)
updateFragmentShader glc tempo oldProg newProg imgMap = do
  t0 <- nowDateTime
  let shaderSrc = fragmentShader glc.webGL2 tempo oldProg newProg imgMap
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

  
calculateImageMap :: Program -> Program -> Map String Int
calculateImageMap oldProg newProg = fromFoldable $ zip imgURLs slots
-- fromFoldable :: forall f k v. Ord k => Foldable f => f (Tuple k v) -> Map k v
  where
    oldProgInfo = programInfo oldProg
    newProgInfo = programInfo newProg
    imgURLs = (toUnfoldable $ (unwrap oldProgInfo).imgURLs <> (unwrap newProgInfo).imgURLs) :: List String
    slots = range 4 15 -- :: List Int
    
    
  
  
deleteWebGL :: WebGL -> Effect Unit
deleteWebGL webGL = deleteWebGLCanvas webGL.glc


drawWebGL :: WebGL -> DateTime -> Effect Unit
drawWebGL webGL now = do
  t0 <- nowDateTime
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
  updateWebcamTexture webGL.sharedResources glc
  setTexture glc shader glc.webcamTexture 3 "w"
  -- update image textures
  imgMap <- read webGL.imgMap
  _ <- traverseWithIndex (setImageTexture webGL shader) imgMap  
  -- update video textures (TODO)
  -- draw
  pLoc <- getAttribLocation glc shader "p"
  bindBufferArray glc webGL.triangleStripBuffer
  vertexAttribPointer glc pLoc
  enableVertexAttribArray glc pLoc
  viewport glc 0 0 1920 1080 -- placeholder, need to read current width and height
  clearColor glc 0.0 0.0 0.0 0.0
  clearColorBuffer glc
  drawDefaultTriangleStrip glc
  t1 <- nowDateTime
  -- log $ " draw time = " <> show (diff t1 t0 :: Milliseconds)
  pure unit


-- bind a texture to a specified active texture slot and pass that location into a shader program by setting a uniform
-- (TODO: try doing this only once instead of every draw call...)
setTexture :: WebGLCanvas -> WebGLProgram -> WebGLTexture -> Int -> String -> Effect Unit
setTexture glc shader t n uName = do
  activeTexture glc n
  bindTexture2D glc t
  setUniform1i glc shader uName n

setImageTexture :: WebGL -> WebGLProgram -> String -> Int -> Effect Unit
setImageTexture webGL shader url n = do
  mTexture <- getImageTexture webGL url
  case mTexture of
    Just t -> setTexture webGL.glc shader t n ("t" <> show n)
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

