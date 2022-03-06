{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}


module SECP256K1.S256Field where


import           Prelude                  hiding (sqrt)
import qualified FieldElement             as FE
import qualified FiniteFieldEllipticCurve as FFEC
import           SECP256K1.Constants


--                                                        p
--                               v----------------------------------------------------------------------------v
type S256Field = FE.FieldElement 115792089237316195423570985008687907853269984665640564039457584007908834671663


sqrt :: S256Field -> S256Field
sqrt v = v `FE.pow` ((p + 1) `div` 4)
