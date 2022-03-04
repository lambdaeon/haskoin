{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module EllipticCurve
  ( Point
  , fromCoords
  ) where


import Data.Proxy
import GHC.TypeLits


data Point (a :: Nat) (b :: Nat)
  = Point Integer Integer
  | Inf
instance (KnownNat a, KnownNat b) => Show (Point a b) where
  -- {{{
  show  Inf        = "Point(infinity)"
  show (Point x y) =
       "Point"
    ++ "("
    ++ show x
    ++ ","
    ++ show y
    ++ ")"
    ++ "_"
    ++ show (natVal (Proxy :: Proxy a))
    ++ "_"
    ++ show (natVal (Proxy :: Proxy b))
  -- }}}
instance Eq (Point a b) where
  -- {{{
  Inf       == Inf       = True
  Point x y == Point p q = x == p && y == q
  _         == _         = False
  -- }}}
instance KnownNat a => Monoid (Point a b) where
  -- {{{
  mempty = Inf
  -- }}}
instance KnownNat a => Semigroup (Point a b) where
  -- {{{
  p1            <> Inf           = p1
  Inf           <> p2            = p2
  (Point x1 y1) <> (Point x2 y2) =
    -- {{{
    let
      fromS s =
        let
          x3 = s ^ 2 - x1 - x2
        in
        (x3, s * (x1 - x3) - y1)
    in
    if x1 == x2 then
      -- {{{
      if y1 /= y2 || y1 == 0 then
        -- {{{
        Inf
        -- }}}
      else
        -- {{{
        let
          s  = (3 * x1 ^ 2 + natVal (Proxy :: Proxy a)) `div` (2 * y1)
          (x3, y3) = fromS s
        in
        Point x3 y3
        -- }}}
      -- }}}
    else
      -- {{{
      let
        (x3, y3) = fromS $ (y2 - y1) `div` (x2 - x1)
      in
      Point x3 y3
      -- }}}
    -- }}}
  -- }}}


fromCoords :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b)
           => Integer
           -> Integer
           -> Maybe (Point a b)
fromCoords x y =
  -- {{{
  let
    a = natVal (Proxy :: Proxy a)
    b = natVal (Proxy :: Proxy b)
  in
  if y ^ 2 == x ^ 3 + a * x + b then
    Just $ Point x y
  else
    Nothing
  -- }}}


-- ex1 :: [Maybe (Point 5 7)]
-- ex1 =
--   [ fromCoords   2    4
--   , fromCoords (-1) (-1)
--   , fromCoords  18   77
--   , fromCoords   5    7
--   ]
-- 
-- 
-- ex4 :: Point 5 7
-- ex4 = Point 2 5 <> Point (-1) (-1)
-- 
-- ex6 :: Point 5 7
-- ex6 = Point (-1) (-1) <> Point (-1) (-1)
-- -- Book's solution is mistakingly for y = 1.





