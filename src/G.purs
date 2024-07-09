module G where

import Prelude (Unit,pure,(<>),otherwise,bind,discard,show,(+),($),(==),(<<<),(>>=),(<$>),(>=),(/),(-))
import Prelude as Prelude
import Data.Map (Map,insert,empty)
import Data.Maybe (Maybe(..))
import Data.List (List,(:))
import Data.List.NonEmpty (NonEmptyList)
import Control.Monad.State (State,get,put,runState,modify_)
import Data.Traversable (traverse,for)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (replicate1)

import Matrix
import Expr

type GState = {
  webGl2 :: Boolean,
  imgMap :: Map String Int,
  vidMap :: Map String Int,
  allocation :: Int,
  code :: String,
  fxy :: Vec2,
  time :: Float,
  beat :: Float,
  etime :: Float,
  ebeat :: Float
  }

type G = State GState

runG :: forall a. Boolean -> Map String Int -> Map String Int -> G a -> Tuple a GState
runG webGl2 imgMap vidMap x = runState x {
  webGl2,
  imgMap,
  vidMap,
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

withFxys :: forall a. Expr a => NonEmptyList Vec2 -> G (Matrix a) -> G (Matrix a)
withFxys fxys a = do
  cachedFxy <- _.fxy <$> get
  rs <- for fxys $ \fxy -> do
    modify_ $ \s -> s { fxy = fxy }
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

