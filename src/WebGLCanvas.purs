module WebGLCanvas where

import Prelude ((<$>),bind,discard,($),pure,Unit)
import Effect
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))

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

type WebGLCanvas = {
  canvas :: Canvas,
  webGLContext :: WebGLContext,
  webGL2 :: Boolean
  }  

getWebGLCanvas :: Effect (Maybe WebGLCanvas)
getWebGLCanvas = do
  canvas <- createCanvas
  m2 <- getWebGL2Context canvas
  case m2 of
    Just ctx -> do
      appendCanvasToDocumentBody canvas
      pure $ Just { canvas, webGLContext: ctx, webGL2: true }
    Nothing -> do
      m1 <- getWebGL1Context canvas
      case m1 of 
        Just ctx -> do
          appendCanvasToDocumentBody canvas
          pure $ Just { canvas, webGLContext: ctx, webGL2: false }
        Nothing -> pure Nothing

deleteWebGLCanvas :: WebGLCanvas -> Effect Unit
deleteWebGLCanvas x = deleteCanvasFromDocumentBody x.canvas

