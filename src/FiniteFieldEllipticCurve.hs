{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances    #-}


module FiniteFieldEllipticCurve
  ( Point
  , fromCoords
  , unsafeFromCoords
  , getX
  , getY
  , scaleBy
  , findOrderFrom
  ) where


import Data.Proxy
import GHC.TypeLits
import Data.Group
import FieldElement (FieldElement)
import qualified FieldElement as FE


data Point (p :: Nat) (a :: Nat) (b :: Nat)
  = Point (FieldElement p) (FieldElement p)
  | Inf
instance (KnownNat p, KnownNat a, KnownNat b) => Show (Point p a b) where
  -- {{{
  show  Inf        = "Point(infinity)"
  show (Point x y) =
       "Point"
    ++ "("
    ++ FE.minShow x
    ++ ","
    ++ FE.minShow y
    ++ ")"
    ++ "_"
    ++ show (natVal (Proxy :: Proxy a))
    ++ "_"
    ++ show (natVal (Proxy :: Proxy b))
  -- }}}
instance Eq (Point p a b) where
  -- {{{
  Inf       == Inf       = True
  Point x y == Point p q = x == p && y == q
  _         == _         = False
  -- }}}
instance (KnownNat p, KnownNat a, KnownNat b) => Semigroup (Point p a b) where
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
          feA = fromInteger $ natVal (Proxy :: Proxy a)
          s  = (3 * x1 ^ 2 + feA) `div` (2 * y1)
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
instance (KnownNat p, KnownNat a, KnownNat b) => Monoid (Point p a b) where
  -- {{{
  mempty = Inf
  -- }}}
instance (KnownNat p, KnownNat a, KnownNat b) => Group (Point p a b) where
  -- {{{
  invert  Inf        = Inf
  invert (Point x y) = Point x (-y)
  -- }}}
instance (KnownNat p, KnownNat a, KnownNat b) => Abelian (Point p a b)


getX :: Point p a b -> Maybe (FieldElement p)
getX  Inf        = Nothing
getX (Point x _) = Just x
getY  Inf        = Nothing
getY (Point _ y) = Just y

fromCoords :: forall (p :: Nat) (a :: Nat) (b :: Nat) . (KnownNat p, KnownNat a, KnownNat b)
           => FieldElement p
           -> FieldElement p
           -> Maybe (Point p a b)
fromCoords x y =
  -- {{{
  let
    a = natVal (Proxy :: Proxy a)
    b = natVal (Proxy :: Proxy b)
  in
  if y ^ 2 == x ^ 3 + fromInteger a * x + fromInteger b then
    Just $ Point x y
  else
    Nothing
  -- }}}

unsafeFromCoords = Point

findOrderFrom :: (KnownNat p, KnownNat a, KnownNat b) => Point p a b -> Integer
findOrderFrom Inf = 0
findOrderFrom p   =
  -- {{{
  let
    go n p'  =
      case p' of
        Inf ->
          n
        _   ->
          go (n + 1) (p <> p')
  in
  go 1 p
  -- }}}


scaleBy :: (KnownNat p, KnownNat a, KnownNat b) => Integer -> Point p a b -> Point p a b
scaleBy = flip pow






