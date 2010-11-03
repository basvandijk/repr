{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , OverloadedStrings
           , ScopedTypeVariables
           , DeriveDataTypeable
  #-}

module Text.Repr
    ( Repr
    , repr
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

-- from base:
import Prelude                 ( Enum(..)
                               , Bounded(..)
                               , Num(..)
                               , Real(..)
                               , Integral(..)
                               , Fractional(..)
                               , Floating(..)
                               , RealFrac(..)
                               , RealFloat(..)
                               , undefined
                               )
import Data.Eq                 ( Eq(..) )
import Data.Ord                ( Ord(..) )
import Data.String             ( IsString(..) )
import Data.Monoid             ( Monoid(..) )
import Data.Bits               ( Bits(..) )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Fixed              ( HasResolution(..) )
import Data.List               ( foldr, map, zipWith, take, length )
import Data.Int                ( Int )
import Data.Ix                 ( Ix(..) )
import Foreign.Storable        ( Storable(..) )
import Foreign.Ptr             ( castPtr )
import Data.Typeable           ( Typeable )
import Control.Applicative     ( liftA2 )
import Control.Monad           ( return )
import Control.Arrow           ( first )
import Text.Show               ( Show(..) )
import Text.Read               ( Read(..) )

#if MIN_VERSION_base(4,0,0)
import Control.Exception       ( Exception(..) )
#endif

#if __GLASGOW_HASKELL__ < 701
import Control.Monad           ( (>>=), fail )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode   ( (∘) )
import Data.Bool.Unicode       ( (∧), (∨) )

-- from random:
import System.Random           ( Random(..) )

-- from string-combinators:
import Data.String.Combinators ( (<>), (<+>)
                               , between, parens, thenParens, brackets
                               , punctuate, fromShow, integer, int, unwords
                               )
-- from dstring:
import Data.DString            ( DString, fromShowS, toShowS )


--------------------------------------------------------------------------------
-- Repr
--------------------------------------------------------------------------------

{-| @Repr &#945;@ is a value of type @&#945;@ paired with a way to render that value to
its textual representation.

@Repr@s follow the property that given a @Repr@ @r@ if you evaluate the textual
representation of @r@ you should get the value or @r@.

Note that @Repr &#945;@ has an instance for most classes in 'base' provided that @&#945;@
has instances for the respected classes. This allows you to write a numeric
expression of type @Repr &#945;@. For example:

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
\"fromRational (3 % 2) + 2 + (3 + negate 4 * (5 - pi / sqrt 6))\"
@
-}
data Repr α = Repr { extract  ∷ α        -- ^ Extract the value of the @Repr@.
                   , renderer ∷ Renderer -- ^ Extract the renderer of the @Repr@.
                   }
            deriving Typeable

-- | Construct a @Repr@ from the given value and its renderer.
repr ∷ α → Renderer → Repr α
repr = Repr

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
type Renderer = Precedence → Fixity → DString

{-| The precedence of operators and function application.

 * Operators usually have a precedence in the range of 0 to 9.

 * Function application always has precedence 10.
-}
type Precedence = Int

-- | Precedence of function application.
funAppPrec ∷ Precedence
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
(<?>) ∷ Repr α → DString → Repr α
(Repr x rx) <?> s = constant x $ parens $ between "{- " " -}" s <+> topLevel rx

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
pure ∷ Show α ⇒ α → Repr α
pure x = Repr x $ \prec _ → fromShowS $ showsPrec prec x


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Show (Repr α) where
    showsPrec prec r = toShowS $ renderer r prec Non

instance Read α ⇒ Read (Repr α) where
    readsPrec prec str =
        map (\(x, rst) → ( constant x $
                              fromString $
                                take (length str - length rst)
                                     str
                          , rst
                          )
            ) $ readsPrec prec str

instance IsString α ⇒ IsString (Repr α) where
    fromString = liftA2 constant fromString fromShow

instance Num α ⇒ Num (Repr α) where
    fromInteger n = repr (fromInteger n) $ \p _ → fromShowS $ showsPrec p n
    (+)           = infx L 6 (+)         "+"
    (-)           = infx L 6 (-)         "-"
    (*)           = infx L 7 (*)         "*"
    negate        = app      negate      "negate"
    abs           = app      abs         "abs"
    signum        = app      signum      "signum"

instance Real α ⇒ Real (Repr α) where
    toRational = to toRational

instance Integral α ⇒ Integral (Repr α) where
    quot      = app2 quot    "quot"
    rem       = app2 rem     "rem"
    div       = app2 div     "div"
    mod       = app2 mod     "mod"
    quotRem   = tup  quotRem "quotRem"
    divMod    = tup  divMod  "divMod"
    toInteger = to   toInteger

instance Fractional α ⇒ Fractional (Repr α) where
    (/)          = infx L 7 (*)          "/"
    recip        = app      recip        "recip"
    fromRational = from     fromRational "fromRational"

instance Floating α ⇒ Floating (Repr α) where
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

instance RealFrac α ⇒ RealFrac (Repr α) where
    properFraction (Repr x rx) =
        let (n, f) = properFraction x
        in (n, Repr f $ "snd" `apply` parens ("properFraction" <+> args [rx]))
    truncate = to truncate
    round    = to round
    ceiling  = to ceiling
    floor    = to floor

instance RealFloat α ⇒ RealFloat (Repr α) where
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

instance Enum α ⇒ Enum (Repr α) where
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

enum ∷ DString → [α] → [Renderer] → [Repr α]
enum enumStr xs rxs = list xs (("enum" <> enumStr) `applies` rxs)

instance Ord α ⇒ Ord (Repr α) where
    compare = to2  compare
    (<)     = to2  (<)
    (>=)    = to2  (>=)
    (>)     = to2  (>)
    (<=)    = to2  (<=)
    max     = app2 max "max"
    min     = app2 min "min"

instance Eq α ⇒ Eq (Repr α) where
    (==) = to2 (==)
    (/=) = to2 (/=)

instance Bounded α ⇒ Bounded (Repr α) where
    minBound = constant minBound "minBound"
    maxBound = constant maxBound "maxBound"

instance Monoid α ⇒ Monoid (Repr α) where
    mempty  = constant mempty  "mempty"
    mappend = app2     mappend "mappend"
    mconcat reprs =
        let (xs, rs) = unzipReprs reprs
        in Repr (mconcat xs) ("mconcat" `apply` brackets (commas rs))

instance Bits α ⇒ Bits (Repr α) where
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
    testBit       = to       testBit
    bitSize       = to       bitSize
    isSigned      = to       isSigned
    shiftL        = app2Show shiftL        "shiftL"
    shiftR        = app2Show shiftR        "shiftR"
    rotateL       = app2Show rotateL       "rotateL"
    rotateR       = app2Show rotateR       "rotateR"

#if MIN_VERSION_base(4,2,0)
instance HasResolution α ⇒ HasResolution (Repr α) where
    resolution (_ ∷ p (Repr α)) = resolution (undefined ∷ p α)
#else
instance HasResolution α ⇒ HasResolution (Repr α) where
    resolution = to resolution
#endif

instance Ix α ⇒ Ix (Repr α) where
    range (Repr b rb, Repr e re) =
        list (range (b, e)) ("range" `apply` parens (commas [rb, re]))

    index     (b, e) p = index     (extract b, extract e) (extract p)
    inRange   (b, e) p = inRange   (extract b, extract e) (extract p)
    rangeSize (b, e)   = rangeSize (extract b, extract e)

instance (Show α, Storable α) ⇒ Storable (Repr α) where
    sizeOf    = to sizeOf
    alignment = to alignment

    peekElemOff rPtr off = do
      x ← peekElemOff (castPtr rPtr) off
      return $ pure x <?> ("peekElemOff" <+> showFuncArg rPtr <+> showFuncArg off)

    peekByteOff ptr off = do
      x ← peekByteOff ptr off
      return $ pure x <?> ("peekByteOff" <+> showFuncArg ptr <+> showFuncArg off)

    peek rPtr = do
      x ← peek (castPtr rPtr)
      return $ pure x <?> ("peek" <+> showFuncArg rPtr)

    poke        rPtr     r = poke        (castPtr rPtr)     (extract r)
    pokeElemOff rPtr off r = pokeElemOff (castPtr rPtr) off (extract r)
    pokeByteOff  ptr off r = pokeByteOff ptr            off (extract r)

#if MIN_VERSION_base(4,0,0)
instance Exception α ⇒ Exception (Repr α) where
    toException = to toException
    fromException se =
        fmap (\x → pure x <?> ( "fromJust"
                              <+> parens ( "fromException"
                                        <+> parens ( "toException"
                                                  <+> parens (showFuncArg x)
                                                  )
                                        )
                              )
             ) $ fromException se
#endif

instance (Random α, Show α) ⇒ Random (Repr α) where
    randomR (b, e) = first pure ∘ randomR (extract b, extract e)
    random         = first pure ∘ random


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

topLevel ∷ Renderer → DString
topLevel r = r 0 Non

constant ∷ α → DString → Repr α
constant x xStr = repr x $ \_ _ → xStr

showFuncArg ∷ Show α ⇒ α → DString
showFuncArg = fromShowS ∘ showsPrec funAppPrec

from ∷ Show α ⇒ (α → β) → DString → (α → Repr β)
from f fStr =
    \x → repr (f x) $ fStr `apply` showFuncArg x

from2 ∷ (Show α, Show β) ⇒ (α → β → γ) → DString → (α → β → Repr γ)
from2 f fStr =
    \x y → repr (f x y) $ fStr `apply`(showFuncArg x <+> showFuncArg y)

to ∷ (α → β) → (Repr α → β)
to f = f ∘ extract

to2 ∷ (α → β → γ) → (Repr α → Repr β → γ)
to2 f = \x y → f (extract x) (extract y)

app ∷ (α → β) → DString → (Repr α → Repr β)
app f fStr =
    \(Repr x rx) → repr (f x) $ fStr `applies` [rx]

app2 ∷ (α → β → γ) → DString → (Repr α → Repr β → Repr γ)
app2 f fStr =
    \(Repr x rx) (Repr y ry) → repr (f x y) $ fStr `applies` [rx, ry]

app2Show ∷ Show β ⇒ (α → β → α) → DString → (Repr α → β → Repr α)
app2Show f fStr =
    \(Repr x rx) y →
        repr (f x y)
             (fStr `applies` [rx, \prec _ → fromShowS $ showsPrec prec y])

infx ∷ Fixity → Precedence → (α → β → γ) → DString
     → (Repr α → Repr β → Repr γ)
infx opFix opPrec op opStr =
    \(Repr x rx) (Repr y ry) →
        repr (x `op` y) $ bin opFix opPrec opStr rx ry

bin ∷ Fixity → Precedence → DString → Renderer → Renderer → Renderer
bin opFix opPrec opStr l r =
    \prec fixity → (prec > opPrec ∨
                     (prec == opPrec ∧
                       fixity /= Non ∧
                       fixity /= opFix))
                   `thenParens`
                   (l opPrec L <+> opStr <+> r opPrec R)

apply ∷ DString → DString → Renderer
fStr `apply` argsStr = \prec _ → (prec >= funAppPrec)
                                 `thenParens`
                                 (fStr <+> argsStr)

applies ∷ DString → [Renderer] → Renderer
applies fStr rs = fStr `apply` args rs

args ∷ [Renderer] → DString
args = unwords ∘ map (\rx → rx funAppPrec Non)

list ∷ [α] → Renderer → [Repr α]
list xs rXs = zipWith combine [0..] xs
    where
      combine ix x = repr x $ bin L 9 "!!" rXs (\_ _ → integer ix)

commas ∷ [Renderer] → DString
commas = unwords ∘ punctuate "," ∘ map topLevel

unzipReprs ∷ [Repr α] → ([α], [Renderer])
unzipReprs = foldr (\(Repr x r) ~(xs, rs) → (x:xs, r:rs)) ([], [])

tup ∷ (α → β → (γ, δ)) → DString
    → (Repr α → Repr β → (Repr γ, Repr δ))
tup f fStr =
    \(Repr x rx) (Repr y ry) → let (q, r) = f x y
                                   s = parens (fStr <+> args [rx, ry])
                               in ( repr q $ "fst" `apply` s
                                  , repr r $ "snd" `apply` s
                                  )


-- The End ---------------------------------------------------------------------
