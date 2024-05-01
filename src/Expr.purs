module Expr where

import Prelude (class Eq, class Ord, class Show,($),(<>),show,(+),(-),(*),(/),(<<<),flip,(==),(/=),(>),(>=),(<),(<=),map)
import Prelude as Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Number as Number

import Channels
import Multi (Multi)
import Number as Number

data GLSLType = Float | Vec2 | Vec3 | Vec4

derive instance Eq GLSLType
derive instance Ord GLSLType
derive instance Generic GLSLType _

instance Channels GLSLType where
  channels Float = 1
  channels Vec2 = 2
  channels Vec3 = 3
  channels Vec4 = 4
  
instance Show GLSLType where 
  show Float = "float"
  show Vec2 = "vec2"
  show Vec3 = "vec3"
  show Vec4 = "vec4"

data Expr =
  ConstantFloat Number |
  Reference GLSLType String
  
instance Channels Expr where
  channels (ConstantFloat _) = 1
  channels (Reference t _) = channels t
  
instance Show Expr where
  show (ConstantFloat x) = show x
  show (Reference _ x) = x

_swizzle :: String -> GLSLType -> Expr -> Expr
_swizzle _ _ (ConstantFloat x) = ConstantFloat x -- swizzling a ConstantFloat is a no-op (not sure about this, but for now...)
_swizzle m t (Reference _ x) = Reference t $ x <> "." <> m

swizzleX :: Expr -> Expr
swizzleX = _swizzle "x" Float

swizzleY :: Expr -> Expr
swizzleY = _swizzle "y" Float

swizzleZ :: Expr -> Expr
swizzleZ = _swizzle "z" Float

swizzleW :: Expr -> Expr
swizzleW = _swizzle "w" Float

swizzleXY :: Expr -> Expr
swizzleXY = _swizzle "xy" Vec2

swizzleYZ :: Expr -> Expr
swizzleYZ = _swizzle "yz" Vec2

swizzleZW :: Expr -> Expr
swizzleZW = _swizzle "zw" Vec2

swizzleXYZ :: Expr -> Expr
swizzleXYZ = _swizzle "xyz" Vec3

swizzleYZW :: Expr -> Expr
swizzleYZW = _swizzle "yzw" Vec3

swizzleXYY :: Expr -> Expr
swizzleXYY = _swizzle "xyy" Vec3

swizzleXYYY :: Expr -> Expr
swizzleXYYY = _swizzle "xyyy" Vec4

swizzleXYZZ :: Expr -> Expr
swizzleXYZZ = _swizzle "xyzz" Vec4


-- i.e. a named unary function where the type of the input is the type of the output
namedUnaryFunction :: String -> (Number -> Number) -> Expr -> Expr
namedUnaryFunction _ f (ConstantFloat x) = ConstantFloat $ f x
namedUnaryFunction f _ (Reference t x) = Reference t $ f <> "(" <> x <> ")"

sin :: Expr -> Expr
sin = namedUnaryFunction "sin" Number.sin

cos :: Expr -> Expr
cos = namedUnaryFunction "cos" Number.cos

abs :: Expr -> Expr
abs = namedUnaryFunction "abs" Number.abs

acos :: Expr -> Expr
acos = namedUnaryFunction "acos" Number.acos

ampdb :: Expr -> Expr
ampdb = (flip division) (ConstantFloat 10.0) <<< product (ConstantFloat 20.0) <<< log

log :: Expr -> Expr
log = namedUnaryFunction "log" Number.log

log2 :: Expr -> Expr
log2 = namedUnaryFunction "log2" Number.log2

log10 :: Expr -> Expr
log10 = namedUnaryFunction "log10" Number.log10

asin :: Expr -> Expr
asin = namedUnaryFunction "asin" Number.asin

atan :: Expr -> Expr
atan = namedUnaryFunction "atan" Number.atan

ceil :: Expr -> Expr
ceil = namedUnaryFunction "ceil" Number.ceil

cpsmidi :: Expr -> Expr
cpsmidi = add (ConstantFloat 69.0) <<< product (ConstantFloat 12.0) <<< log2 <<< flip division (ConstantFloat 440.0)

dbamp :: Expr -> Expr
dbamp = pow (ConstantFloat 10.0) <<< flip division (ConstantFloat 20.0)

exp :: Expr -> Expr
exp = namedUnaryFunction "exp" Number.exp

floor :: Expr -> Expr
floor = namedUnaryFunction "floor" Number.floor

midicps :: Expr -> Expr
midicps m = product (pow (division (difference m (ConstantFloat 69.0)) (ConstantFloat 12.0)) (ConstantFloat 2.0)) (ConstantFloat 440.0)

sign :: Expr -> Expr
sign = namedUnaryFunction "sign" Number.sign

sqrt :: Expr -> Expr
sqrt = namedUnaryFunction "sqrt" Number.sqrt

tan :: Expr -> Expr
tan = namedUnaryFunction "tan" Number.tan


-- for +-*/ the arguments (in GLSL) are the same type or either argument can be a float (regardless of the other argument)
-- this will produce invalid GLSL code (without warning/error) if different non-float GLSL types are mixed
arithmeticOperator :: String -> (Number -> Number -> Number) -> Expr -> Expr -> Expr
arithmeticOperator _ f (ConstantFloat x) (ConstantFloat y) = ConstantFloat (f x y)
arithmeticOperator o _ (ConstantFloat x) (Reference yType y) = Reference yType $ "(" <> show x <> o <> y <> ")"
arithmeticOperator o _ (Reference xType x) (ConstantFloat y) = Reference xType $ "(" <> x <> o <> show y <> ")"
arithmeticOperator o _ (Reference xType x) (Reference yType y) = Reference (Prelude.max xType yType) $ "(" <> x <> o <> y <> ")"

add :: Expr -> Expr -> Expr
add = arithmeticOperator "+" (+)

difference :: Expr -> Expr -> Expr
difference = arithmeticOperator "-" (-)

product :: Expr -> Expr -> Expr
product = arithmeticOperator "*" (*)

division :: Expr -> Expr -> Expr
division = arithmeticOperator "/" (/) -- TODO: this should be safe division to match the audio side!

-- arguments are same type or either can be a float, order is irrelevant (unlike in GLSL, where the second argument would have to be a float)
-- this will produce invalid GLSL code (without warning/error) if different non-float GLSL types are mixed
minOrMax :: String -> (Number -> Number -> Number) -> Expr -> Expr -> Expr
minOrMax _ f (ConstantFloat x) (ConstantFloat y) = ConstantFloat $ f x y
minOrMax f _ (ConstantFloat x) (Reference yType y) = Reference yType $ f <> "(" <> y <> "," <> show x <> ")"
minOrMax f _ (Reference xType x) (ConstantFloat y) = Reference xType $ f <> "(" <> x <> "," <> show y <> ")"
minOrMax f _ (Reference Float x) (Reference yType y) = Reference yType $ f <> "(" <> y <> "," <> x <> ")"
minOrMax f _ (Reference xType x) (Reference _ y) = Reference xType $ f <> "(" <> x <> "," <> y <> ")"

min :: Expr -> Expr -> Expr
min = minOrMax "min" Prelude.min

max :: Expr -> Expr -> Expr
max = minOrMax "max" Prelude.max


-- to polyfill pow and mod to the standard model, coerce unmatched float arguments to the other type (will simply be a cast)
-- this will produce invalid GLSL code (without warning/error) if different non-float GLSL types are mixed
powOrMod :: String -> (Number -> Number -> Number) -> Expr -> Expr -> Expr
powOrMod _ f (ConstantFloat x) (ConstantFloat y) = ConstantFloat $ f x y
powOrMod f _ (ConstantFloat x) (Reference Float y) = Reference Float $ f <> "(" <> show x <> "," <> y <> ")"
powOrMod f _ (ConstantFloat x) (Reference yType y) = Reference yType $ f <> "(" <> show yType <> "(" <> show x <> ")," <> y <> ")"
powOrMod f _ (Reference Float x) (ConstantFloat y) = Reference Float $ f <> "(" <> x <> "," <> show y <> ")"
powOrMod f _ (Reference xType x) (ConstantFloat y) = Reference xType $ f <> "(" <> x <> "," <> show xType <> "(" <> show y <> "))"
powOrMod f _ (Reference xType x) (Reference _ y) = Reference xType $ f <> "(" <> x <> "," <> y <> ")"

pow :: Expr -> Expr -> Expr
pow = powOrMod "pow" Number.pow

mod :: Expr -> Expr -> Expr
mod = powOrMod "mod" Prelude.mod


comparisonOperator :: String -> String -> (Number -> Number -> Number) -> Expr -> Expr -> Expr
-- both arguments floats
comparisonOperator _ _ f (ConstantFloat x) (ConstantFloat y) = ConstantFloat $ f x y
comparisonOperator f _ _ (ConstantFloat x) (Reference Float y) = Reference Float $ "float(" <> show x <> f <> y <> ")"
comparisonOperator f _ _ (Reference Float x) (ConstantFloat y) = Reference Float $ "float(" <> x <> f <> show y <> ")"
comparisonOperator f _ _ (Reference Float x) (Reference Float y) = Reference Float $ "float(" <> x <> f <> y <> ")"
-- only one argument is a float
comparisonOperator _ f _ (ConstantFloat x) (Reference yType y) = Reference yType $ show yType <> "(" <> f <> "(" <> show yType <> "(" <> show x <> ")," <> y <> "))"
comparisonOperator _ f _ (Reference Float x) (Reference yType y) = Reference yType $ show yType <> "(" <> f <> "(" <> show yType <> "(" <> x <> ")," <> y <> "))"
comparisonOperator _ f _ (Reference xType x) (ConstantFloat y) = Reference xType $ show xType <> "(" <> f <> "(" <> x <> "," <> show xType <> "(" <> show y <> ")))"
comparisonOperator _ f _ (Reference xType x) (Reference Float y) = Reference xType $ show xType <> "(" <> f <> "(" <> x <> "," <> show xType <> "(" <> y <> ")))"
-- no arguments are floats
comparisonOperator _ f _ (Reference xType x) (Reference _ y) = Reference xType $ show xType <> "(" <> f <> "(" <> x <> "," <> y <> "))"

booleanToNumber :: Boolean -> Number
booleanToNumber true = 1.0
booleanToNumber false = 0.0

equal :: Expr -> Expr -> Expr
equal = comparisonOperator "==" "equal" (map (map booleanToNumber) (==))

notEqual :: Expr -> Expr -> Expr
notEqual = comparisonOperator "!=" "notEqual" (map (map booleanToNumber) (/=))

greaterThan :: Expr -> Expr -> Expr
greaterThan = comparisonOperator ">" "greaterThan" (map (map booleanToNumber) (>))

greaterThanEqual :: Expr -> Expr -> Expr
greaterThanEqual = comparisonOperator ">=" "greaterThanEqual" (map (map booleanToNumber) (>=))

lessThan :: Expr -> Expr -> Expr
lessThan = comparisonOperator "<" "lessThan" (map (map booleanToNumber) (<))

lessThanEqual ::Expr -> Expr -> Expr
lessThanEqual = comparisonOperator "<=" "lessThanEqual" (map (map booleanToNumber) (<=))


-- miscellaneous functions over Expr:

-- caller should assign second argument because of multiple use in this definition
gate :: Expr -> Expr -> Expr
gate x y = product (lessThan x y) y

-- first two arguments outline the range and if they are not floats (constant or otherwise) invalid GLSL code would be generated
-- first two arguments are multiply used and should be assigned by caller
clip :: Expr -> Expr -> Expr -> Expr
clip r1 r2 x = uncheckedClip (min r1 r2) (max r1 r2) x

-- first two arguments outline the range and if they are not floats (constant or otherwise) invalid GLSL code would be generated
uncheckedClip :: Expr -> Expr -> Expr -> Expr
uncheckedClip (ConstantFloat r1) (ConstantFloat r2) (ConstantFloat x) = ConstantFloat $ Number.uncheckedClip r1 r2 x
uncheckedClip r1 r2 (ConstantFloat x) = Reference Float $ "clamp(" <> show x <> "," <> show r1 <> "," <> show r2 <> ")"
uncheckedClip r1 r2 (Reference xType x) = Reference xType $ "clamp(" <> show x <> "," <> show r1 <> "," <> show r2 <> ")"

-- all arguments are multiply used and should be assigned by the caller
-- expectation is that first two arguments are floats (constant or otherwise), unsure what results would be if this is violated
between :: Expr -> Expr -> Expr -> Expr
between r1 r2 x = product (lessThan (min r1 r2) x) (difference (ConstantFloat 1.0) (lessThan (max r1 r2) x))

-- expectation is that first two arguments are floats (constant or otherwise), unsure what results would be if this is violated
-- (no multiple use of arguments)
smoothStep :: Expr -> Expr -> Expr -> Expr
smoothStep (ConstantFloat e0) (ConstantFloat e1) (ConstantFloat x) = ConstantFloat $ Number.smoothStep e0 e1 x
smoothStep e0 e1 (ConstantFloat x) = Reference Float $ "smoothstep(" <> show e0 <> "," <> show e1 <> "," <> show x <> ")"
smoothStep e0 e1 (Reference xType x) = Reference xType $ "smoothstep(" <> show e0 <> "," <> show e1 <> "," <> show x <> ")"

{-
circle :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Any -> Any (no reuse of any arguments, so no inherent need to pre-assign them)
circle fxy xy d 
  | fxy.glslType /= Vec2 || xy.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in circle", glslType: Float, isSimple: false, deps: fxy.deps <> xy.deps <> d.deps }
  | otherwise = { string: s, glslType: d.glslType, isSimple: false, deps: fxy.deps <> xy.deps <> d.deps }
      where s = "smoothstep(1.5/(res.x+res.y),0.0,distance(" <> fxy.string <> "," <> xy.string <> ")-(" <> d.string <> "*0.5))"

point :: GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2
point fxy xy = circle fxy xy d
  where d = { string: "((1./res.x)+(1./res.y))", glslType: Float, isSimple: false, deps: empty }
 
      
vline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Float/Any Float/Any -> Float/Any (Float/Any arguments follow standard pattern, either matched or one is float)
vline fxy x w 
  | x.glslType /= Float && w.glslType /= Float && x.glslType /= w.glslType = { string: "!! Internal Punctual GLSL generation error in vline", glslType: Float, isSimple: false, deps: fxy.deps <> x.deps <> w.deps }
  | otherwise = { string: s, glslType: Prelude.max x.glslType w.glslType, isSimple: false, deps: fxy.deps <> x.deps <> w.deps }
      where
        a = "abs(" <> fxy.string <> ".x-" <> x.string <> ")-" <> w.string
        edge0 = explicitlyTypedZero w.glslType
        edge1 = "min(" <> w.string <> ",3./res.x)"
        s = "(1.-smoothstep(" <> edge0.string <> "," <> edge1 <> "," <> a <> "))"

hline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Float/Any Float/Any -> Float/Any (Float/Any arguments follow standard pattern, either matched or one is float)
hline fxy y w 
  | y.glslType /= Float && w.glslType /= Float && y.glslType /= w.glslType = { string: "!! Internal Punctual GLSL generation error in hline", glslType: Float, isSimple: false, deps: fxy.deps <> y.deps <> w.deps }
  | otherwise = { string: s, glslType: Prelude.max y.glslType w.glslType, isSimple: false, deps: fxy.deps <> y.deps <> w.deps }
      where
        a = "abs(" <> fxy.string <> ".y-" <> y.string <> ")-" <> w.string
        edge0 = explicitlyTypedZero w.glslType
        edge1 = "min(" <> w.string <> ",3./res.y)"
        s = "(1.-smoothstep(" <> edge0.string <> "," <> edge1 <> "," <> a <> "))"

line :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Vec2 Float -> Float (no reuse of any arguments)
line fxy xy1 xy2 w
  | fxy.glslType /= Vec2 || xy1.glslType /= Vec2 || xy2.glslType /= Vec2 || w.glslType /= Float = { string: "!! Internal Punctual GLSL generation error in line", glslType: Float, isSimple: false, deps: fxy.deps <> xy1.deps <> xy2.deps <> w.deps }
  | otherwise = { string: "line(" <> xy1.string <> "," <> xy2.string <> "," <> w.string <> "," <> fxy.string <> ")", glslType: Float, isSimple: false, deps: fxy.deps <> xy1.deps <> xy2.deps <> w.deps }

iline :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Vec2 Float -> Float (no reuse of any arguments)
iline fxy xy1 xy2 w
  | fxy.glslType /= Vec2 || xy1.glslType /= Vec2 || xy2.glslType /= Vec2 || w.glslType /= Float = { string: "!! Internal Punctual GLSL generation error in line", glslType: Float, isSimple: false, deps: fxy.deps <> xy1.deps <> xy2.deps <> w.deps }
  | otherwise = { string: "iline(" <> xy1.string <> "," <> xy2.string <> "," <> w.string <> "," <> fxy.string <> ")", glslType: Float, isSimple: false, deps: fxy.deps <> xy1.deps <> xy2.deps <> w.deps }

-- \float linlin(vec2 r1, vec2 r2, float x) { return r2.x+((r2.y-r2.x)*(x-r1.x)/(r1.y-r1.x));}\

linlin :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 Any -> Any, vec2 range arguments are re-used and should be pre-assigned
linlin r1 r2 x 
  | r1.glslType /= Vec2 || r2.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in linlin", glslType: Float, isSimple: false, deps: r1.deps <> r2.deps <> x.deps }
  | otherwise = { string: s, glslType: x.glslType, isSimple: false, deps: r1.deps <> r2.deps <> x.deps }
      where
        x' = "(" <> x.string <> "-" <> r1.string <> ".x)"
        r1size = "(" <> r1.string <> ".y-" <> r1.string <> ".x)"
        r2size = "(" <> r2.string <> ".y-" <> r2.string <> ".x)"
        s = "(" <> r2.string <> ".x+(" <> r2size <> "*" <> x' <> "/" <> r1size <> "))"

mix :: GLSLExpr -> GLSLExpr -> GLSLExpr -> GLSLExpr
mix x y a
  | x.glslType /= y.glslType = { string: "!! Internal Punctual GLSL generation error in mix", glslType: Float, isSimple: false, deps: x.deps <> y.deps <> a.deps }
  | x.glslType /= a.glslType && a.glslType /= Float = { string: "!! Internal Punctual GLSL generation error in mix", glslType: Float, isSimple: false, deps: x.deps <> y.deps <> a.deps }
  | otherwise = { string: s, glslType: t, isSimple: false, deps: x.deps <> y.deps <> a.deps }
      where
        s = "mix(" <> x.string <> "," <> y.string <> "," <> a.string <> ")"
        t = Prelude.max x.glslType a.glslType

seq :: NonEmptyList GLSLExpr -> GLSLExpr -> GLSLExpr -- all arguments are Float and return value is Float
seq steps y = { string: s, glslType: Float, isSimple: false, deps: fold (map _.deps steps) <> y.deps }
  where
    nSteps = length steps
    stepSize = 1.0 / toNumber nSteps
    firstStep = "(step(" <> y.string <> "," <> show stepSize <> ")*" <> (head steps).string <> ")"
    lastStep = "(step(" <> show (1.0-stepSize) <> "," <> y.string <> ")*" <> (last steps).string <> ")"
    middleStep n r = "((step(" <> show (stepSize*toNumber n) <> "," <> y.string <> ")-step(" <> show (stepSize*toNumber n + stepSize) <> "," <> y.string  <> "))*" <> r <> ")"
    middleStepExprs = case List.init (tail steps) of
                        Just xs -> xs
                        _ -> List.Nil
    middleStepNumbers = toList $ range 1 (nSteps - 2)
    middleStepValues = map _.string middleStepExprs
    middleSteps = List.zipWith middleStep middleStepNumbers middleStepValues
    s = intercalate "+" ((firstStep : middleSteps) `List.snoc` lastStep)

fadeIn :: Number -> Number -> GLSLExpr 
fadeIn t1 t2 = { string: "clamp((_etime-" <> t1s <> ")/(" <> t2s <> "-" <> t1s <> "),0.,1.)", glslType: Float, isSimple: false, deps: empty }
  where
    t1s = show t1
    t2s = show t2

fadeOut :: Number -> Number -> GLSLExpr
fadeOut t1 t2 = { string: "clamp((" <> t2s <> "-_etime)/(" <> t2s <> "-" <> t1s <> "),0.,1.)", glslType: Float, isSimple: false, deps: empty }
  where
    t1s = show t1
    t2s = show t2
    
prox :: GLSLExpr -> GLSLExpr -> GLSLExpr -- Vec2 Vec2 -> Float
prox a b
  | a.glslType /= Vec2 || b.glslType /= Vec2 = { string: "!! Internal Punctual GLSL generation error in prox", glslType: Float, isSimple: false, deps: a.deps <> b.deps }
  | otherwise = { string: "clamp((2.828427-distance(" <> a.string <> "," <> b.string <> "))/2.828427,0.,1.)", glslType: Float, isSimple: false, deps: a.deps <> b.deps }

bipolar :: GLSLExpr -> GLSLExpr -- Any -> Any
bipolar x = { string: "(" <> x.string <> "*2.-1.)", glslType: x.glslType, isSimple: x.isSimple, deps: x.deps }

unipolar :: GLSLExpr -> GLSLExpr -- Any -> Any
unipolar x = { string: "(" <> x.string <> "*0.5+0.5)", glslType: x.glslType, isSimple: x.isSimple, deps: x.deps }

tile :: GLSLExpr -> GLSLExpr -> GLSLExpr -- technically Any -> Any, even if Vec2 -> Vec2 is imagined use 
tile fxy ab = bipolar $ fract $ product (unipolar fxy) ab
  
fract :: GLSLExpr -> GLSLExpr
fract x = { string: "fract(" <> x.string <> ")", glslType: x.glslType, isSimple: x.isSimple, deps: x.deps }

pi :: GLSLExpr
pi = simpleFromString Float "PI"

px :: GLSLExpr
px = simpleFromString Float "(2./res.x)"

py :: GLSLExpr
py = simpleFromString Float "(2./res.y)"

pxy :: GLSLExpr
pxy = simpleFromString Vec2 "(2./res)"

aspect :: GLSLExpr
aspect = simpleFromString Float "(res.x/res.y)"

rgbhsv :: GLSLExpr -> GLSLExpr
rgbhsv x
  | x.glslType /= Vec3 = { string: "!! Internal Punctual GLSL generation error in rgbhsv", glslType: Float, isSimple: false, deps: x.deps }
  | otherwise = { string: "rgbhsv(" <> x.string <> ")", glslType: Vec3, isSimple: false, deps: x.deps }
  
hsvrgb :: GLSLExpr -> GLSLExpr
hsvrgb x
  | x.glslType /= Vec3 = { string: "!! Internal Punctual GLSL generation error in hsvrgb", glslType: Float, isSimple: false, deps: x.deps }
  | otherwise = { string: "hsvrgb(" <> x.string <> ")", glslType: Vec3, isSimple: false, deps: x.deps }
  
distance :: GLSLExpr -> GLSLExpr -> GLSLExpr
distance x y
  | x.glslType /= y.glslType = { string: "!! Internal Punctual GLSL generation error in distance", glslType: Float, isSimple: false, deps: x.deps <> y.deps }
  | otherwise = { string: "distance(" <> x.string <> "," <> y.string <> ")", glslType: x.glslType, isSimple: false, deps: x.deps <> y.deps }
-}


type Exprs = Multi Expr

