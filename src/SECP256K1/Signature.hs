{-# LANGUAGE RecordWildCards #-}


module SECP256K1.Signature where


import           Data.Function            ((&))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified FieldElement             as FE
import qualified FiniteFieldEllipticCurve as FFEC
import           Utils
import           SECP256K1.Constants
import           SECP256K1.S256Field
import           SECP256K1.S256Point


data Signature = Signature
  { r :: S256Field
  , s :: S256Order
  } deriving (Eq, Show)


toDER :: Signature -> ByteString
toDER Signature {..} =
  -- {{{
  let
    prependLength w8s =
      -- {{{
      let
        [lenW80, lenW81] = -- not perfect... but ok here.
          -- {{{
          length w8s
          & flip div 2
          & fromIntegral
          & integralTo32Bytes
          & BS.take 2
          & BS.unpack
          -- }}}
      in
      lenW80 : lenW81 : w8s
      -- }}}
    fromInitBS w8s =
      -- {{{
      case w8s of
        fstW8 : sndW8 : _ ->
          -- {{{
          let
            tier1 =
              -- {{{
              if (fstW8 * 16 + sndW8) >= 0x80 then
                48 : 48 : w8s
              else
                w8s
              -- }}}
          in
          48 : 50 : prependLength tier1
          -- }}}
        _ ->
          -- {{{
          w8s
          -- }}}
      -- }}}
    rBS = fromInitBS $ BS.unpack $ integralTo32Bytes r
    sBS = fromInitBS $ BS.unpack $ integralTo32Bytes s
    word8s = 51 : 48 : prependLength (rBS ++ sBS)
  in
  BS.pack word8s
  -- }}}









