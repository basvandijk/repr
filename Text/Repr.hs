{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Repr
    ( Repr
    , extract
    , renderer
    , Renderer
    , Precedence
    , Fixity(..)
    , (<?>)
    , pure
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
                               , brackets
                               , punctuate
                               , fromShow
                               , integer
                               , int
                               , hsep
                               )
import Data.DString            ( DString, fromShowS, toShowS )
import Data.Monoid             ( Monoid, mempty, mappend, mconcat )
import Data.Bits               ( Bits
                               , (.&.)
                               , (.|.)
                               , xor
                               , complement
                               , shift
                               , rotate
                               , bit
                               , setBit
                               , clearBit
                               , complementBit
                               , testBit
                               , bitSize
                               , isSigned
                               , shiftL
                               , shiftR
                               , rotateL
                               , rotateR
                               )
import Data.Fixed              ( HasResolution, resolution )
import Data.Ix                 ( Ix, range, index, inRange, rangeSize )
import Foreign.Storable        ( Storable
                               , sizeOf
                               , alignment
                               , peekElemOff
                               , pokeElemOff
                               , peekByteOff
                               , pokeByteOff
                               , peek
                               , poke
                               )
import Foreign.Ptr             ( castPtr )
import Data.Typeable           ( Typeable, typeOf)
import System.Random           ( Random, randomR, random )
import Control.Applicative     ( liftA2 )
import Control.Arrow           ( first )

#if MIN_VERSION_base(4,0,0)
import Control.Exception       ( Exception, toException, fromException )
#endif


--------------------------------------------------------------------------------
-- Repr
--------------------------------------------------------------------------------

{-| @Repr a@ is a value of type @a@ paired with a way to render that value to
its textual representation.

@Repr@s follow the property that given a @Repr@ @r@ if you evaluate the textual
representation of @r@ you should get the value or @r@.

Note that @Repr a@ has an instance for most classes in 'base' provided that @a@
has instances for the respected classes. This allows you to write a numeric
expression of type @Repr a@. For example:

@
*Repr> let r = 1.5 + 2 + (3 + (-4) * (5 - pi / sqrt 6)) :: Repr Double
@

You can extract the value of @r@:

@
*Repr> extract r
17.281195923884734
@

And you can render @r@ to its textual representation using 'show':

@
*Repr> show r
\"fromRational (3 % 2) + fromInteger 2 + (fromInteger 3 + negate (fromInteger 4) * (fromInteger 5 - pi / sqrt (fromInteger 6)))\"
@
-}
data Repr a = Repr { extract  :: a        -- ^ Extract the value of the @Repr@.
                   , renderer :: Renderer -- ^ Extract the renderer of the @Repr@.
                   }

{-| To render you need to supply the precedence and fixity of the
enclosing context.

For more documentation about precedence and fixity see:

<http://haskell.org/onlinereport/decls.html#sect4.4.2>

The reason the renderer returns a 'DString', instead of for example a 'String'
has to do with efficiency.  The rendering of expressions involves lots of
left-factored appends i.e.: @((a ++ b) ++ c) ++ d@. A 'DString', which is
equivalent to a 'ShowS', has a O(1) append operation while a 'String' has a O(n)
append.
-}
type Renderer = Precedence ->  Fixity -> DString

{-| The precedence of operators and function application.

 * Operators usually have a precedence in the range of 0 to 9.

 * Function application always has precedence 10.
-}
type Precedence = Int

-- | Precedence of function application.
funAppPrec :: Precedence
funAppPrec = 10

-- | Fixity of operators.
data Fixity = Non -- ^ No fixity information.
            | L   -- ^ Left associative operator.
            | R   -- ^ Right associative operator.
              deriving Eq

{-| @x \<?\> s@ annotates the rendering with the given string.

The rendering wil look like: @\"({\- s -\} ...)\"@ where @...@ is the rendering
of @x@.

This combinator is handy when you want to render the ouput of a function and you
want to see how the parameters of the function contribute to the result. For
example, suppose you defined the following function @f@:

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
(Repr x rx) <?> s = constant x $ paren $ between "{- " " -}" s <+> topLevel rx

{-| @pure x@ constructs a 'Repr' which has @x@ as value and the showed @x@
as rendering. For example:

@
*Repr> let r = pure [1,2,3]
*Repr> extract r
[1,2,3]
*Repr> show r
\"[1,2,3]\"
@
-}
pure :: Show a => a -> Repr a
pure x = Repr x $ \prec _ -> fromShowS $ showsPrec prec x


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Show (Repr a) where
    showsPrec prec r = toShowS $ renderer r prec Non

instance Read a => Read (Repr a) where
    readsPrec prec str =
        map (\(x, rst) -> ( constant x $
                              fromString $
                                take (length str - length rst)
                                     str
                          , rst
                          )
            ) $ readsPrec prec str

instance IsString a => IsString (Repr a) where
    fromString = liftA2 constant fromString fromShow

instance ToString a => ToString (Repr a) where
    toString = to toString

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
    (/)          = infx L 7 (*)          "/"
    recip        = app      recip        "recip"
    fromRational = from     fromRational "fromRational"

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
    properFraction (Repr x rx) =
        let (n, f) = properFraction x
        in (n, Repr f $ "snd" `apply` paren ("properFraction" <+> args [rx]))

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
    enumFrom       (Repr x rx) = enum "From"       (enumFrom       x)     [rx]
    enumFromThen   (Repr x rx)
                   (Repr y ry) = enum "FromThen"   (enumFromThen   x y)   [rx, ry]
    enumFromTo     (Repr x rx)
                   (Repr y ry) = enum "FromTo"     (enumFromTo     x y)   [rx, ry]
    enumFromThenTo (Repr x rx)
                   (Repr y ry)
                   (Repr z rz) = enum "FromThenTo" (enumFromThenTo x y z) [rx, ry, rz]

enum :: DString -> [a] -> [Renderer] -> [Repr a]
enum enumStr xs rxs = list xs (("enum" <> enumStr) `applies` rxs)

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

instance Bounded a => Bounded (Repr a) where
    minBound = constant minBound "minBound"
    maxBound = constant maxBound "maxBound"

instance Monoid a => Monoid (Repr a) where
    mempty  = constant mempty  "mempty"
    mappend = app2     mappend "mappend"
    mconcat reprs =
        let (xs, rs) = unzipReprs reprs
        in Repr (mconcat xs) ("mconcat" `apply` brackets (commas rs))

instance Bits a => Bits (Repr a) where
    (.&.)         = infx L 7 (.&.)         ".&."
    (.|.)         = infx L 5 (.|.)         ".|."
    xor           = app2     xor           "xor"
    complement    = app      complement    "complement"
    shift         = app2Show shift         "shift"
    rotate        = app2Show rotate        "rotate"
    bit           = from     bit           "bit"
    setBit        = app2Show setBit        "setBit"
    clearBit      = app2Show clearBit      "clearBit"
    complementBit = app2Show complementBit "complementBit"
    testBit x i   = testBit (extract x) i
    bitSize       = to       bitSize
    isSigned      = to       isSigned
    shiftL        = app2Show shiftL        "shiftL"
    shiftR        = app2Show shiftR        "shiftR"
    rotateL       = app2Show rotateL       "rotateL"
    rotateR       = app2Show rotateR       "rotateR"

instance HasResolution a => HasResolution (Repr a) where
    resolution = to resolution

instance Ix a => Ix (Repr a) where
    range (Repr b rb, Repr e re) =
        list (range (b, e)) ("range" `apply` paren (commas [rb, re]))

    index     (b, e) p = index     (extract b, extract e) (extract p)
    inRange   (b, e) p = inRange   (extract b, extract e) (extract p)
    rangeSize (b, e)   = rangeSize (extract b, extract e)

instance (Show a, Storable a) => Storable (Repr a) where
    sizeOf    = to sizeOf
    alignment = to alignment

    peekElemOff rPtr off = do
      x <- peekElemOff (castPtr rPtr) off
      return $ pure x <?> ("peekElemOff" <+> showFuncArg rPtr <+> showFuncArg off)

    peekByteOff ptr off = do
      x <- peekByteOff ptr off
      return $ pure x <?> ("peekByteOff" <+> showFuncArg ptr <+> showFuncArg off)

    peek rPtr = do
      x <- peek (castPtr rPtr)
      return $ pure x <?> ("peek" <+> showFuncArg rPtr)

    poke        rPtr     r = poke        (castPtr rPtr)     (extract r)
    pokeElemOff rPtr off r = pokeElemOff (castPtr rPtr) off (extract r)
    pokeByteOff  ptr off r = pokeByteOff ptr            off (extract r)

instance Typeable a => Typeable (Repr a) where
    typeOf = to typeOf

#if MIN_VERSION_base(4,0,0)
instance Exception a => Exception (Repr a) where
    toException = to toException
    fromException se =
        fmap (\x -> pure x <?> ( "fromJust"
                               <+> paren ( "fromException"
                                         <+> paren ( "toException"
                                                   <+> paren (showFuncArg x)
                                                   )
                                         )
                               )
             ) $ fromException se
#endif

instance (Random a, Show a) => Random (Repr a) where
    randomR (b, e) = first pure . randomR (extract b, extract e)
    random         = first pure . random


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

topLevel :: Renderer -> DString
topLevel r = r 0 Non

constant :: a -> DString -> Repr a
constant x xStr = Repr x $ \_ _ -> xStr

showFuncArg :: Show a => a -> DString
showFuncArg = fromShowS . showsPrec funAppPrec

from :: Show a => (a -> b) -> DString -> (a -> Repr b)
from f fStr =
    \x -> Repr (f x) $ fStr `apply` showFuncArg x

from2 :: (Show a, Show b) => (a -> b -> c) -> DString -> (a -> b -> Repr c)
from2 f fStr =
    \x y -> Repr (f x y) $ fStr `apply`(showFuncArg x <+> showFuncArg y)

to :: (a -> b) -> (Repr a -> b)
to f = f . extract

to2 :: (a -> b -> c) -> (Repr a -> Repr b -> c)
to2 f = \x y -> f (extract x) (extract y)

app :: (a -> b) -> DString -> (Repr a -> Repr b)
app f fStr =
    \(Repr x rx) -> Repr (f x) $ fStr `applies` [rx]

app2 :: (a -> b -> c) -> DString -> (Repr a -> Repr b -> Repr c)
app2 f fStr =
    \(Repr x rx) (Repr y ry) -> Repr (f x y) $ fStr `applies` [rx, ry]

app2Show :: Show b => (a -> b -> a) -> DString -> (Repr a -> b -> Repr a)
app2Show f fStr =
    \(Repr x rx) y ->
        Repr (f x y)
             (fStr `applies` [rx, \prec _ -> fromShowS $ showsPrec prec y])

infx :: Fixity -> Precedence -> (a -> b -> c) -> DString
     -> (Repr a -> Repr b -> Repr c)
infx opFix opPrec op opStr =
    \(Repr x rx) (Repr y ry) ->
        Repr (x `op` y) $ bin opFix opPrec opStr rx ry

bin :: Fixity -> Precedence -> DString -> Renderer -> Renderer -> Renderer
bin opFix opPrec opStr l r =
    \prec fixity -> (prec > opPrec ||
                     (prec == opPrec &&
                      fixity /= Non &&
                      fixity /= opFix))
                    `thenParen`
                    (l opPrec L <+> opStr <+> r opPrec R)

apply :: DString -> DString -> Renderer
fStr `apply` argsStr = \prec _ -> (prec >= funAppPrec)
                                  `thenParen`
                                  (fStr <+> argsStr)

applies :: DString -> [Renderer] -> Renderer
applies fStr rs = fStr `apply` args rs

args :: [Renderer] -> DString
args = hsep . map (\rx -> rx funAppPrec Non)

list :: [a] -> Renderer -> [Repr a]
list xs rXs = zipWith combine [0..] xs
    where
      combine ix x = Repr x $ bin L 9 "!!" rXs (\_ _ -> integer ix)

commas :: [Renderer] -> DString
commas = hsep . punctuate "," . map topLevel

unzipReprs :: [Repr a] -> ([a], [Renderer])
unzipReprs = foldr (\(Repr x r) ~(xs, rs) -> (x:xs, r:rs)) ([], [])

tup :: (a -> b -> (c, d)) -> DString
    -> (Repr a -> Repr b -> (Repr c, Repr d))
tup f fStr =
    \(Repr x rx) (Repr y ry) -> let (q, r) = f x y
                                    s = paren (fStr <+> args [rx, ry])
                                in ( Repr q $ "fst" `apply` s
                                   , Repr r $ "snd" `apply` s
                                   )


-- The End ---------------------------------------------------------------------
