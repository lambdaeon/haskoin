module FieldElement
  ( FieldElement
  , minShow
  , pow
  ) where


import Data.Proxy
import GHC.TypeLits
import GHC.Real (divZeroError, Ratio(..))
import Data.List (sort)


-- | Newtype wrapper of a field element, with its
--   "prime" (or modulus) defined at the type level.
--   Since the data constructor is not exposed, the
--   wrapped numbers are guaranteed to be between @0@
--   and @m - 1@.
newtype FieldElement (m :: Nat) = FE Integer
instance KnownNat m => Show (FieldElement m) where
  -- {{{
  show (FE x) =
       "FieldElement"
    ++ "_"
    ++ show (natVal (Proxy :: Proxy m))
    ++ "("
    ++ show x
    ++ ")"
  -- }}}
instance Eq   (FieldElement m) where
  -- {{{
  FE x == FE y = x == y
  -- }}}
instance Ord  (FieldElement m) where
  -- {{{
  compare (FE x) (FE y) = compare x y
  -- }}}
-- | All operations are essentially the same as typical arithmetics.
--   The difference being that basically a @flip mod m@ is applied
--   at the end.
instance KnownNat m => Num (FieldElement m) where
  -- {{{
  fromInteger x = FE $ mod x          (natVal (Proxy :: Proxy m))
  FE x  +  FE y = FE $ mod (x + y)    (natVal (Proxy :: Proxy m))
  FE x  *  FE y = FE $ mod (x * y)    (natVal (Proxy :: Proxy m))
  abs    (FE x) = FE $ mod (abs x)    (natVal (Proxy :: Proxy m))
  signum (FE x) = FE $ mod (signum x) (natVal (Proxy :: Proxy m))
  negate (FE x) = FE $ mod (negate x) (natVal (Proxy :: Proxy m))
  -- }}}
instance KnownNat m => Bounded (FieldElement m) where
  -- {{{
  minBound = FE 0
  maxBound = FE (natVal (Proxy :: Proxy m) - 1)
  -- }}}
instance KnownNat m => Enum (FieldElement m) where
  -- {{{
  toEnum          = fromInteger . toInteger
  fromEnum (FE x) = fromInteger x
  -- }}}
instance KnownNat m => Real (FieldElement m) where
  -- {{{
  toRational (FE x) = x :% 1
  -- }}}
instance KnownNat m => Integral (FieldElement m) where
  -- {{{
  toInteger (FE x)   = x
  quotRem   (FE x) b =
    if b == minBound then
      divZeroError
    else
      let
        FE yInv = b ^ (natVal (Proxy :: Proxy m) - 2)
      in
      (fromInteger (x * yInv), minBound)
  -- }}}


-- | Minimal representation of the field element.
minShow :: FieldElement m -> String
minShow (FE x) = show x


-- | A more efficient power function, which can also handle
--   negative exponents. This is based on Fermat's Little Theorem
--   which states \(a^{(m-1)}=1\). Meaning \(m-1\) can be
--   added (or reduced) to (or from) the given exponent enough
--   times until it's positive (or smaller than @m@). This multiple
--   addition/subraction is equivalent to performing a @mod@ with @m@.
pow :: forall (m :: Nat) b . (KnownNat m, Integral b)
    => FieldElement m
    -> b
    -> FieldElement m
pow a exp =
  -- {{{
  a ^ (toInteger exp `mod` (natVal (Proxy :: Proxy m) - 1))
  -- }}}


-- {{{ EXERCISES 
-- make19 :: Integer -> FieldElement 19
-- make19 = fromInteger
-- ex5 k  = sort $ make19 . (k *) <$> [0..18]
-- 
-- -- p = 7, 11, 17, 31
-- ex70 = [(i :: FieldElement 7 ) ^ 6  | i <- [1..6] ]
-- ex71 = [(i :: FieldElement 11) ^ 10 | i <- [1..10]]
-- ex72 = [(i :: FieldElement 17) ^ 16 | i <- [1..16]]
-- ex73 = [(i :: FieldElement 31) ^ 30 | i <- [1..30]]
-- 
-- ex80 :: FieldElement 31
-- ex80 = 3 `div` 24
-- ex81 :: FieldElement 31
-- -- ex81 = 1 `div` (17 ^ 3)
-- ex81 = 17 `pow` (-3)
-- ex82 :: FieldElement 31
-- -- ex82 = (1 `div` (4 ^ 4)) * 11
-- ex82 = (4 `pow` (-4)) * 11
-- }}}






