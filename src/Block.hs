module Block
  ( Block (..)
  , getId
  , bip9
  , bip91
  , bip141
  , confirm
  , difficulty
  , sampleBS
  ) where


import           BlockBits                   (BlockBits (..))
import qualified BlockBits
import           Data.Bits                   ((.&.), setBit, zeroBits)
import qualified Data.Bits                   as Bits
import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import qualified Extension.ByteString.Lazy   as LBS
import           Extension.ByteString.Parser  
import qualified Text.Megaparsec             as P
import           TxIn                        (TxIn (..))
import qualified TxIn
import           TxOut                       (TxOut (..))
import qualified TxOut
import           Tx                          (Tx (..))
import qualified Tx
import           Utils


-- | Record type representing a block.
data Block = Block
  { blockVersion    :: Word32
  , blockPrevBlock  :: ByteString
  , blockMerkleRoot :: ByteString
  , blockTimestamp  :: Word32
  , blockBits       :: BlockBits
  , blockNonce      :: ByteString
  } deriving (Eq, Show)

instance Serializable Block where
  serialize Block {..} =
    -- {{{
       integralToNBytesLE 4  blockVersion
    <> LBS.take           32 blockPrevBlock  -- guaranteeing the byte count.
    <> LBS.take           32 blockMerkleRoot -- guaranteeing the byte count.
    <> integralToNBytesLE 4  blockTimestamp
    <> serialize             blockBits
    <> LBS.take           4  blockNonce      -- guaranteeing the byte count.
    -- }}}
  parser = do
    -- {{{
    blockVersion    <- word32ParserLE "block's version"
    blockPrevBlock  <- P.takeP  (Just "block's prev block" ) 32
    blockMerkleRoot <- P.takeP  (Just "block's merkle root") 32
    blockTimestamp  <- word32ParserLE "block's version"
    blockBits       <- parser
    blockNonce      <- P.takeP  (Just "block's nonce"      ) 4
    return $ Block {..}
    -- }}}


-- | Get the inverse of the HASH256 of the serialized block.
getId :: Block -> ByteString
getId = hash256 . serialize


-- | Check if block is BIP0009 compliant.
bip9 :: Block -> Bool
bip9   Block {..} = blockVersion `Bits.shiftR` 29        == setBit zeroBits 1


-- | Check if block is BIP0091 compliant.
bip91 :: Block -> Bool
bip91  Block {..} = (blockVersion `Bits.shiftR` 4) .&. 1 == 1


-- | Check if block is BIP0141 (Segwit) compliant.
bip141 :: Block -> Bool
bip141 Block {..} = (blockVersion `Bits.shiftR` 1) .&. 1 == 1


-- | Checks whether the proof-of-work is valid.
confirm :: Block -> Bool
confirm block@Block {..} =
  -- {{{
  bsToIntegerLE (getId block) < BlockBits.toTarget blockBits
  -- }}}


-- | Finds the difficulty value of the given `Block`.
difficulty :: Block -> Double
difficulty Block {..} =
  -- {{{
  let
    target = fromIntegral $ BlockBits.toTarget blockBits
  in
  0xffff * 256 ^ (0x1d - 3) / target
  -- }}}


-- SAMPLE VALUE
-- {{{
sampleBS :: ByteString
sampleBS = integerToBS 0x020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d
-- }}}

