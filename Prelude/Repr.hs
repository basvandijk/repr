{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Prelude.Repr
-- Copyright   :  (c) 2009–2011 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- The numeric functions from the 'Prelude' lifted into @'Repr's@. 
--
--------------------------------------------------------------------------------

module Prelude.Repr where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import qualified Prelude ( subtract
                         , even
                         , odd
                         , gcd
                         , lcm
                         , (^)
                         , (^^) 
                         , fromIntegral
                         , realToFrac
                         )

import Prelude   ( Num, Integral, Fractional, Real )
import Data.Bool ( Bool )

-- from repr:
import Text.Repr ( Repr, Fixity(R), to, app, app2, infx )


--------------------------------------------------------------------------------
-- Numeric functions
--------------------------------------------------------------------------------

-- | Lifts @Prelude.'Prelude.subtract'@ into @'Repr's@
subtract ∷ Num α ⇒ Repr α → Repr α → Repr α
subtract = app2 Prelude.subtract "subtract"

-- | Lifts @Prelude.'Prelude.even'@ into a 'Repr'
even ∷ Integral α ⇒ Repr α → Bool
even = to Prelude.even

-- | Lifts @Prelude.'Prelude.odd'@ into a 'Repr'
odd ∷ Integral α ⇒ Repr α → Bool
odd = to Prelude.odd

-- | Lifts @Prelude.'Prelude.gcd'@ into @'Repr's@
gcd ∷ Integral α ⇒ Repr α → Repr α → Repr α
gcd = app2 Prelude.gcd "gcd"

-- | Lifts @Prelude.'Prelude.lcm'@ into @'Repr's@
lcm ∷ Integral α ⇒ Repr α → Repr α → Repr α
lcm = app2 Prelude.lcm "lcm"

-- | Lifts @Prelude.'Prelude.^'@ into @'Repr's@
(^) ∷ (Num α, Integral β) ⇒ Repr α → Repr β → Repr α
(^) = infx R 8 (Prelude.^) "^"

-- | Lifts @Prelude.'Prelude.^^'@ into @'Repr's@
(^^) ∷ (Fractional α, Integral β) ⇒ Repr α → Repr β → Repr α
(^^) = infx R 8 (Prelude.^^) "^^"

-- | Lifts @Prelude.'Prelude.fromIntegral'@ into @'Repr's@
fromIntegral ∷ (Integral α, Num β) ⇒ Repr α → Repr β
fromIntegral = app Prelude.fromIntegral "fromIntegral"

-- | Lifts @Prelude.'Prelude.realToFrac'@ into @'Repr's@
realToFrac ∷ (Real α, Fractional β) ⇒ Repr α → Repr β
realToFrac = app Prelude.realToFrac "realToFrac"


-- The End ---------------------------------------------------------------------
