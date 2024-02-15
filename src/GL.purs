module GL where

import Prelude ((<$>),bind,($),pure)
import Effect
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

foreign import data Canvas :: Type

foreign import createCanvas :: Effect Canvas

foreign import data WebGLContext :: Type

foreign import _getWebGL1Context :: Canvas -> Effect (Nullable WebGLContext)

getWebGL1Context :: Canvas -> Effect (Maybe WebGLContext)
getWebGL1Context c = toMaybe <$> _getWebGL1Context c

foreign import _getWebGL2Context :: Canvas -> Effect (Nullable WebGLContext)

getWebGL2Context :: Canvas -> Effect (Maybe WebGLContext)
getWebGL2Context c = toMaybe <$> _getWebGL2Context c

-- will get WebGL2 if possible, then try for WebGL 1 if not. Int represents which level of the API is supported.
getWebGLContext :: Canvas -> Effect (Maybe (Tuple WebGLContext Int))
getWebGLContext c = do
  m2 <- getWebGL2Context c
  case m2 of
    Just ctx -> pure $ Just $ Tuple ctx 2
    Nothing -> do
      m1 <- getWebGL1Context c
      case m1 of 
        Just ctx -> pure $ Just $ Tuple ctx 1
        Nothing -> pure Nothing

