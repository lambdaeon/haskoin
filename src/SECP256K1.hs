module SECP256K1 where


import qualified Data.ByteString.Lazy     as LBS
import qualified FieldElement             as FE
import qualified FiniteFieldEllipticCurve as FFEC
import           Utils


-- | Type alias of a field element with prime `n`,
--   representing Bitcoin's finite cyclic group from
--   its generator point.
type S256Order = FE.FieldElement 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141


-- | Order of Bitcoin's generator point.
n  :: Integer
n  = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141


-- | Prime of the field element that points on Bitcoin's
--   curve use.
p  :: Integer
p  = 2 ^ 256 - 2 ^ 32 - 977


-- | Type alias of a field element with prime `p`.
type S256Field = FE.FieldElement 115792089237316195423570985008687907853269984665640564039457584007908834671663


-- | Type alias for points on Bitcoin's elliptic curve.
type S256Point =
  FFEC.Point 115792089237316195423570985008687907853269984665640564039457584007908834671663
             0
             7

-- | Coefficients that Bitcoin's elliptic curve uses.
a, b :: Integer
a = 0
b = 7


-- | The x coordinate of Bitcoin's generator point.
gx :: S256Field
gx = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798


-- | The y coordinate of Bitcoin's generator point.
gy :: S256Field
gy = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8


-- | Bitcoin's generator point.
generator :: S256Point
generator = FFEC.unsafeFromCoords gx gy


-- | Efficient square root function, applicable to any
--   finite element which the remainder of its prime
--   divided by 4 is 3 (refer to the book for a detailed
--   explanation).
s256FieldSqrt :: S256Field -> S256Field
s256FieldSqrt v = v `FE.pow` ((p + 1) `div` 4)





