{-# LANGUAGE FlexibleInstances, JavaScriptFFI #-}

module Sound.Punctual.GL where

type GL = ReaderT WebGLRenderingContext IO

gl :: GL WebGLRenderingContext
gl = ask

createProgram :: GL WebGLProgram
createProgram = gl >>= (liftIO . _createProgram)

foreign import javascript unsafe
  "$1.createProgram()"
  _createProgram :: WebGLRenderingContext -> IO WebGLProgram

createVertexShader :: GL WebGLShader
createVertexShader = gl >>= (liftIO . _createVertexShader)

foreign import javascript unsafe
  "$1.createShader($1.VERTEX_SHADER)"
  _createVertexShader :: WebGLRenderingContext -> IO WebGLShader

createFragmentShader :: GL WebGLShader
createFragmentShader = gl >>= (liftIO . _createFragmentShader)

foreign import javascript unsafe
  "$1.createShader($1.FRAGMENT_SHADER)"
  _createFragmentShader :: WebGLRenderingContext -> IO WebGLShader

attachShader :: WebGLProgram -> WebGLShader -> GL ()
attachShader p s = gl >>= (liftIO . _attachShader p s)

foreign import javascript unsafe
  "$3.attachShader($1,$2);"
  _attachShader :: WebGLProgram -> WebGLShader -> WebGLRenderingContext -> IO ()

shaderSource :: WebGLShader -> Text -> GL ()
shaderSource s t = gl >>= (liftIO . _shaderSource s t)

foreign import javascript unsafe
  "$3.shaderSource($1,$2);"
  _shaderSource :: WebGLShader -> Text -> WebGLRenderingContext -> IO ()

compileShader :: WebGLShader -> GL ()
compileShader s = gl >>= (liftIO . _compileShader s)

foreign import javascript unsafe
  "$2.compileShader($1);"
  _compileShader :: WebGLShader -> WebGLRenderingContext -> IO ()

linkProgram :: WebGLProgram -> GL ()
linkProgram p = gl >>= (liftIO . _linkProgram p)

foreign import javascript unsafe
  "$2.linkProgram($1);"
  _linkProgram :: WebGLProgram -> WebGLRenderingContext -> IO ()

linkStatus :: WebGLProgram -> GL Int
linkStatus p = gl >>= (liftIO . _linkStatus p)

foreign import javascript unsafe
  "$2.getProgramParameter($1,$2.LINK_STATUS)"
  _linkStatus :: WebGLProgram -> WebGLRenderingContext -> IO Int

useProgram :: WebGLProgram -> GL ()
useProgram p = gl >>= (liftIO . _useProgram p)

foreign import javascript unsafe
  "$2.useProgram($1);"
  _useProgram :: WebGLProgram -> WebGLRenderingContext -> IO ()

deleteProgram :: WebGLProgram -> GL ()
deleteProgram p = gl >>= (liftIO . _deleteProgram p)

foreign import javascript unsafe
  "$2.deleteProgram($1);"
  _deleteProgram :: WebGLProgram -> WebGLRenderingContext -> IO ()
