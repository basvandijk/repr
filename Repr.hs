{-# LANGUAGE OverloadedStrings #-}

module Repr
    ( Repr
    , value
    , renderer
    , Renderer
    , Precedence
    , Fixity(..)
    , render
    , (<?>)
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.String             ( IsString, fromString )
import Data.String.ToString    ( ToString, toString )
import Data.String.Combinators ( (<>)
                               , (<+>)
                               , between
                               , paren
                               , thenParen
                               , fromShow
                               , integer
                               , int
                               , hsep
                               )
import Data.DString            ( DString, fromShowS )
import Control.Applicative     ( liftA2 )


--------------------------------------------------------------------------------
-- Repr
--------------------------------------------------------------------------------

{-| @Repr a@ is a value of type @a@ paired with a way to render that
value to a string which will contain a representation of the value.

Note that @Repr a@ is overloaded for all the numeric classes provided that
@a@ has instances for the respected classes. This allows you to write a
numeric expression of type @Repr a@. For example:

@
*Repr> let r = 1.5 + 2 + (3 + (-4) * (5 - pi / sqrt 6)) :: Repr Double
@

You can extract the value of @r@:

@
*Repr> value r
17.281195923884734
@

And you can than render @r@ to its textual representation:

@
*Repr> render r
\"fromRational (3 % 2) + fromInteger 2 + (fromInteger 3 + negate (fromInteger 4) * (fromInteger 5 - pi / sqrt (fromInteger 6)))\"
@
-}
data Repr a = S { value    :: a        -- ^ Extract the value of the @Repr@.
                , renderer :: Renderer -- ^ Extrac the renderer of the @Repr@.
                }

{-| To render you need to supply the precedence and fixity of the
enclosing context.

For more documentation about precedence and fixity see:

<http://haskell.org/onlinereport/decls.html#sect4.4.2>

The reason the renderer returns a 'DString' instead of for example a 'String'
is that the rendering of numeric expression involves lots of left-factored
appends i.e.: @((a ++ b) ++ c) ++ d@. A 'DString' has a O(1) append operation
while a 'String' just has a O(n) append. So choosing a 'DString' is more
efficient.
-}
type Renderer = Precedence ->  Fixity -> DString

{-| The precedence of operators and function application.

 * Operators usually have a precedence in the range of 0 to 9.

 * Function application always has precedence 10.
-}
type Precedence = Int

-- | Fixity of operators.
data Fixity = Non -- ^ No fixity information.
            | L   -- ^ Left associative operator.
            | R   -- ^ Right associative operator.
              deriving Eq

{-| Render a /top-level/ value to a 'String'. Note that:

@
render r = 'toString' $ 'renderer' r 0 'Non'
@
-}
render :: Repr a -> String
render r = toString $ renderer r 0 Non

{-| @x \<?\> s@ annotates the rendering with the given string.

The output wil look like: @\"({\- s -\} ...)\"@ where @...@ is the rendering of @x@.

This combinator is handy when you want to render the ouput of a
function and you want to see how the parameters of the function
contribute to the result. For example, suppose you defined the
following function @f@:

@
f p0 p1 p2 = p0 ^ 2 + sqrt p1 * ([p2..] !! 10)
@

You can then apply @f@ to some parameters annotated with some descriptive
strings (the name of the parameter is usally a good idea):

@
f (1 \<?\> \"p0\") (2 \<?\> \"p1\") (3 \<?\> \"p2\")
@

The rendering will then look like:

@
\"({\- p0 -\} fromInteger 1) * ({\- p0 -\} fromInteger 1) + sqrt ({\- p1 -\} (fromInteger 2)) * enumFrom ({\- p2 -\} (fromInteger 3)) !! 10\"
@
-}
(<?>) :: Repr a -> DString -> Repr a
(S x rx) <?> s = S x $ \prec fixity -> paren (between "{- " " -}" s <+> rx prec fixity)


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Show (Repr a) where
    show = render

instance Num a => Num (Repr a) where
    fromInteger = from     fromInteger "fromInteger"
    (+)         = infx L 6 (+)         "+"
    (-)         = infx L 6 (-)         "-"
    (*)         = infx L 7 (*)         "*"
    negate      = app      negate      "negate"
    abs         = app      abs         "abs"
    signum      = app      signum      "signum"

instance Real a => Real (Repr a) where
    toRational = to toRational

instance Integral a => Integral (Repr a) where
    quot        = app2 quot    "quot"
    rem         = app2 rem     "rem"
    div         = app2 div     "div"
    mod         = app2 mod     "mod"
    quotRem     = tup  quotRem "quotRem"
    divMod      = tup  divMod  "divMod"
    toInteger   = to   toInteger

instance Fractional a => Fractional (Repr a) where
    (/)          = infx L 7 (*)         "/"
    recip        = app     recip        "recip"
    fromRational = from    fromRational "fromRational"

instance Floating a => Floating (Repr a) where
    pi      = constant pi      "pi"
    (**)    = infx R 8 (**)    "**"
    logBase = app2     logBase "logBase"
    exp     = app      exp     "exp"
    sqrt    = app      sqrt    "sqrt"
    log     = app      log     "log"
    sin     = app      sin     "sin"
    tan     = app      tan     "tan"
    cos     = app      cos     "cos"
    asin    = app      asin    "asin"
    atan    = app      atan    "atan"
    acos    = app      acos    "acos"
    sinh    = app      sinh    "sinh"
    tanh    = app      tanh    "tanh"
    cosh    = app      cosh    "cosh"
    asinh   = app      asinh   "asinh"
    atanh   = app      atanh   "atanh"
    acosh   = app      acosh   "acosh"

instance RealFrac a => RealFrac (Repr a) where
    properFraction (S x rx) =
        let (n, f) = properFraction x
        in (n, S f $ "snd" `apply` paren ("properFraction" <+> args [rx]))

instance RealFloat a => RealFloat (Repr a) where
    floatRadix     = to    floatRadix
    floatDigits    = to    floatDigits
    floatRange     = to    floatRange
    decodeFloat    = to    decodeFloat
    encodeFloat    = from2 encodeFloat    "encodeFloat"
    exponent       = to    exponent
    significand    = app   significand    "significand"
    scaleFloat i   = app   (scaleFloat i) ("scaleFloat" <+> int i)
    isNaN          = to    isNaN
    isInfinite     = to    isInfinite
    isDenormalized = to    isDenormalized
    isNegativeZero = to    isNegativeZero
    isIEEE         = to    isIEEE
    atan2          = app2  atan2 "atan2"

instance Enum a => Enum (Repr a) where
    succ     = app   succ   "succ"
    pred     = app   pred   "pred"
    toEnum   = from  toEnum "toEnum"
    fromEnum = to    fromEnum
    enumFrom       (S x rx)                   = enum "From"       (enumFrom       x)     [rx]
    enumFromThen   (S x rx) (S y ry)          = enum "FromThen"   (enumFromThen   x y)   [rx, ry]
    enumFromTo     (S x rx) (S y ry)          = enum "FromTo"     (enumFromTo     x y)   [rx, ry]
    enumFromThenTo (S x rx) (S y ry) (S z rz) = enum "FromThenTo" (enumFromThenTo x y z) [rx, ry, rz]

enum :: DString -> [a] -> [Renderer] -> [Repr a]
enum enumStr xs rxs = zipWith combine [0..] xs
    where
      combine i y = S y $ bin L 9 "!!" ("enum" <> enumStr <+> args rxs) (integer i)

instance Ord a => Ord (Repr a) where
    compare = to2  compare
    (<)     = to2  (<)
    (>=)    = to2  (>=)
    (>)     = to2  (>)
    (<=)    = to2  (<=)
    max     = app2 max "max"
    min     = app2 min "min"

instance Eq a => Eq (Repr a) where
    (==) = to2 (==)
    (/=) = to2 (/=)

instance IsString a => IsString (Repr a) where
    fromString = liftA2 constant fromString fromShow


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Construct a 'Repr' from a given value and string.
constant :: a -> DString -> Repr a
constant x xStr = S x $ \_ _ -> xStr

-- | Precedence of function application.
funAppPrec :: Precedence
funAppPrec = 10

{-| Given a function @f@ and the name of that function @fStr@ return
a function that takes a 'Show'able argument @x@ and returns a 'Repr'
that has @f x@ as value and @fStr@ prepended to the showed @x@ as
renderer .

For example:
@
*Repr> let r = from fromRational "fromRational" 13.4
*Repr> value r
13.4 -- fromRational (67 % 5)
*Repr> render r
"fromRational (67 % 5)"
@
-}
from :: Show a => (a -> b) -> DString -> (a -> Repr b)
from f fStr = \x -> S (f x) $ fStr `apply` fromShowS (showsPrec funAppPrec x)

-- | Same as 'from' with the difference that the given function has two arguments.
from2 :: (Show a, Show b) => (a -> b -> c) -> DString -> (a -> b -> Repr c)
from2 f fStr = \x y -> S (f x y) $ fStr `apply`(     fromShowS (showsPrec funAppPrec x)
                                                 <+> fromShowS (showsPrec funAppPrec y)
                                               )

-- | Return the converted value of the 'Repr'.
to :: (a -> b) -> (Repr a -> b)
to f = f . value

-- | Return the combined values of the 'Repr's.
to2 :: (a -> b -> c) -> (Repr a -> Repr b -> c)
to2 f = \x y -> f (value x) (value y)

{-| Given a function @f@ and the name of that function @fStr@ return
a function that takes a @Repr@ and returns a @Repr@ that has as value
@f@ applied to the value of the given @Repr@ and as renderer @fStr@
prepended to the renderer of the given @Repr@.

For example:
@
*Repr> let r = app sqrt "sqrt" 4
*Repr> value r
2.0 -- sqrt (fromInteger 4)
*Repr> render r
"sqrt (fromInteger 4)"
@
-}
app :: (a -> b) -> DString -> (Repr a -> Repr b)
app f fStr =
    \(S x rx) -> S (f x) $ fStr `apply` args [rx]

{-| Like 'app' but works for binary functions.

For example:
@
*Repr> let r = app2 quot "quot" 4 2
*Repr> value r
2 -- quot (fromInteger 4) (fromInteger 2)
*Repr> render r
"quot (fromInteger 4) (fromInteger 2)"
@
-}
app2 :: (a -> b -> c) -> DString -> (Repr a -> Repr b -> Repr c)
app2 f fStr =
    \(S x rx) (S y ry) -> S (f x y) $ fStr `apply` args [rx, ry]

{-|

For example:
@
*Repr> let r = infx L 6 (+) "+" 2 3
*Repr> value r
5 -- fromInteger 2 + fromInteger 3
*Repr> render r
"fromInteger 2 + fromInteger 3"
@
-}
infx :: Fixity -> Precedence -> (a -> b -> c) -> DString
     -> (Repr a -> Repr b -> Repr c)
infx opFix opPrec op opStr =
    \(S x rx) (S y ry) ->
        S (x `op` y) $ bin opFix opPrec opStr (rx opPrec L) (ry opPrec R)

bin :: Fixity -> Precedence -> DString -> DString -> DString -> Renderer
bin opFix opPrec opStr l r = \prec fixity -> (prec > opPrec ||
                                              (prec == opPrec &&
                                               fixity /= Non &&
                                               fixity /= opFix))
                                             `thenParen`
                                             (l <+> opStr <+> r)

apply :: DString -> DString -> Renderer
funStr `apply` argsStr = \prec _ -> (prec >= funAppPrec)
                                    `thenParen`
                                    (funStr <+> argsStr)

args :: [Renderer] -> DString
args = hsep . map (\rx -> rx funAppPrec Non)

tup :: (a -> b -> (c, d)) -> DString
    -> (Repr a -> Repr b -> (Repr c, Repr d))
tup f fStr =
    \(S x rx) (S y ry) -> let (q, r) = f x y
                              s = paren (fStr <+> args [rx, ry])
                          in ( S q $ "fst" `apply` s
                             , S r $ "snd" `apply` s
                             )


-- The End ---------------------------------------------------------------------
