module WebGL where

import Prelude ((<$>),bind,discard,($),pure,Unit)
import Effect
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..),isJust)

foreign import data Canvas :: Type

foreign import createCanvas :: Effect Canvas

foreign import appendCanvasToDocumentBody :: Canvas -> Effect Unit

foreign import deleteCanvasFromDocumentBody :: Canvas -> Effect Unit

foreign import data WebGLContext :: Type

foreign import _getWebGL1Context :: Canvas -> Effect (Nullable WebGLContext)

getWebGL1Context :: Canvas -> Effect (Maybe WebGLContext)
getWebGL1Context c = toMaybe <$> _getWebGL1Context c

foreign import _getWebGL2Context :: Canvas -> Effect (Nullable WebGLContext)

getWebGL2Context :: Canvas -> Effect (Maybe WebGLContext)
getWebGL2Context c = toMaybe <$> _getWebGL2Context c

foreign import data WebGLExtension :: Type

foreign import _getExtension :: WebGLContext -> String -> Effect (Nullable WebGLExtension)

getExtension :: WebGLContext -> String -> Effect (Maybe WebGLExtension)
getExtension c n = toMaybe <$> _getExtension c n


type WebGL = {
  canvas :: Canvas,
  context :: WebGLContext,
  webGL2 :: Boolean,
  khr_parallel_shader_compile :: Boolean
  }  

newWebGL :: Effect (Maybe WebGL)
newWebGL = do
  canvas <- createCanvas
  m2 <- getWebGL2Context canvas
  case m2 of
    Just context -> do
      appendCanvasToDocumentBody canvas
      khr_parallel_shader_compile <- isJust <$> getExtension context "KHR_parallel_shader_compile"
      pure $ Just { canvas, context, webGL2: true, khr_parallel_shader_compile }
    Nothing -> do
      m1 <- getWebGL1Context canvas
      case m1 of 
        Just context -> do
          appendCanvasToDocumentBody canvas
          khr_parallel_shader_compile <- isJust <$> getExtension context "KHR_parallel_shader_compile"
          pure $ Just { canvas, context, webGL2: false, khr_parallel_shader_compile }
        Nothing -> pure Nothing

deleteWebGL :: WebGL -> Effect Unit
deleteWebGL x = deleteCanvasFromDocumentBody x.canvas

