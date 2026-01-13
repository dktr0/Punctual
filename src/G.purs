module G where

import Prelude (Unit, bind, discard, pure, show, ($), (+), (<$>), (<>), (>>=), (>>>))
import Data.Map (Map)
import Data.List.NonEmpty (NonEmptyList)
import Control.Monad.State (State,get,put,runState,modify_)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))

import Matrix (Matrix, concat)
import Expr (class Expr, Float(..), Vec2(..), Vec3(..), Vec4(..), expr, showType, toExpr, toExprSafe, swizzleX, swizzleY)

type TextureMap = { imgs :: Map String Int, vids :: Map String Int, gdms :: Map String Int }
   
type GState = {
  webGl2 :: Boolean,
  textureMap :: TextureMap,
  allocation :: Int,
  code :: String,
  fxy :: Vec2,
  time :: Float,
  beat :: Float,
  etime :: Float,
  ebeat :: Float
  }

type G = State GState

runG :: forall a. Boolean -> TextureMap -> G a -> Tuple a GState
runG webGl2 textureMap x = runState x {
  webGl2,
  textureMap,
  allocation: 0,
  code: "",
  fxy: Vec2Expr "((gl_FragCoord.xy/res)*2.-1.)", 
  time: FloatExpr "_time",
  beat: FloatExpr "_beat",
  etime: FloatExpr "_etime",
  ebeat: FloatExpr "_ebeat"
  }

assign :: forall a. Expr a => a -> G a
assign x = do 
  n <- allocate
  let name = "_" <> show n      
  writeCode $ showType x <> " " <> name <> "=" <> toExpr x <> ";\n"
  pure $ expr name

allocate :: G Int
allocate = do
  s <- get
  put $ s { allocation = s.allocation + 1 }
  pure s.allocation
  
writeCode :: String -> G Unit
writeCode x = modify_ $ \s -> s { code = s.code <> x }

fx :: G String
fx = get >>= _.fxy >>> swizzleX >>> toExprSafe >>> pure

fy :: G String
fy = get >>= _.fxy >>> swizzleY >>> toExprSafe >>> pure

fxy :: G (Tuple String String)
fxy = do
  s <- get
  pure $ Tuple (toExprSafe $ swizzleX $ s.fxy) (toExprSafe $ swizzleY $ s.fxy)
 
withFxy :: forall a. Vec2 -> G a -> G a
withFxy newFxy a = do
  cachedFxy <- _.fxy <$> get
  modify_ $ \s -> s { fxy = newFxy }
  r <- a
  modify_ $ \s -> s { fxy = cachedFxy }
  pure r

withFxys :: forall a. Expr a => NonEmptyList Vec2 -> G (Matrix a) -> G (Matrix a)
withFxys fxys a = do
  cachedFxy <- _.fxy <$> get
  rs <- for fxys $ \newFxy -> do
    modify_ $ \s -> s { fxy = newFxy }
    a
  modify_ $ \s -> s { fxy = cachedFxy }
  pure $ concat rs

withAlteredTime :: forall a. Expr a => NonEmptyList { time :: Float, beat :: Float, etime :: Float, ebeat :: Float } -> G (Matrix a) -> G (Matrix a)
withAlteredTime xs a = do
  cached <- get
  rs <- for xs $ \x -> do
    modify_ $ \s -> s { time = x.time, beat = x.beat, etime = x.etime, ebeat = x.ebeat }
    a
  modify_ $ \s -> s { time = cached.time, beat = cached.beat, etime = cached.etime, ebeat = cached.ebeat }
  pure $ concat rs

textureFFT :: String -> Float -> G Float
textureFFT texName x = do
  s <- get
  case s.webGl2 of
    true -> assign $ FloatExpr $ "texture(" <> texName <> ",vec2(" <> toExpr x <> ",0.)).x"
    false -> assign $ FloatExpr $ "texture2D(" <> texName <> ",vec2(" <> toExpr x <> ",0.)).x" 

texture2D :: String -> Vec2 -> G Vec3
texture2D texName xy = do
  s <- get
  case s.webGl2 of
    true -> assign $ Vec3Expr $ "texture(" <> texName <> "," <> toExpr xy <> ").xyz"
    false -> assign $ Vec3Expr $ "texture2D(" <> texName <> "," <> toExpr xy <> ").xyz"

texture2Da :: String -> Vec2 -> G Vec4
texture2Da texName xy = do
  s <- get
  case s.webGl2 of
    true -> assign $ Vec4Expr $ "texture(" <> texName <> "," <> toExpr xy <> ")"
    false -> assign $ Vec4Expr $ "texture2D(" <> texName <> "," <> toExpr xy <> ")"
