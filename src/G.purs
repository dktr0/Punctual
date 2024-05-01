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

import Multi
import Expr

type GState = {
  webGl2 :: Boolean,
  imgMap :: Map String Int,
  vidMap :: Map String Int,
  allocation :: Int,
  code :: String,
  fxy :: Expr,
  time :: Expr,
  beat :: Expr,
  etime :: Expr,
  ebeat :: Expr
  }

type G = State GState

runG :: forall a. Boolean -> Map String Int -> Map String Int -> G a -> Tuple a GState
runG webGl2 imgMap vidMap x = runState x {
  webGl2,
  imgMap,
  vidMap,
  allocation: 0,
  code: "",
  fxy: Reference Vec2 "((gl_FragCoord.xy/res)*2.-1.)", 
  time: Reference Float "_time",
  beat: Reference Float "_beat",
  etime: Reference Float "_etime",
  ebeat: Reference Float "_ebeat"
  }

assign :: GLSLType -> String -> G Expr
assign t x = do
  n <- allocate
  let name = "_" <> show n
  writeCode $ show t <> " " <> show name <> "=" <> x <> ";\n"
  pure $ Reference t name

allocate :: G Int
allocate = do
  s <- get
  put $ s { allocation = s.allocation + 1 }
  pure s.allocation
  
writeCode :: String -> G Unit
writeCode x = modify_ $ \s -> s { code = s.code <> x } 

withFxys :: NonEmptyList Expr -> G Exprs -> G Exprs
withFxys fxys a = do
  cachedFxy <- _.fxy <$> get
  rs <- for fxys $ \fxy -> do
    modify_ $ \s -> s { fxy = fxy }
    a
  modify_ $ \s -> s { fxy = cachedFxy }
  pure $ concat rs

withAlteredTime :: NonEmptyList { time :: Expr, beat :: Expr, etime :: Expr, ebeat :: Expr } -> G Exprs -> G Exprs
withAlteredTime xs a = do
  cached <- get
  rs <- for xs $ \x -> do
    modify_ $ \s -> s { time = x.time, beat = x.beat, etime = x.etime, ebeat = x.ebeat }
    a
  modify_ $ \s -> s { time = cached.time, beat = cached.beat, etime = cached.etime, ebeat = cached.ebeat }
  pure $ concat rs

