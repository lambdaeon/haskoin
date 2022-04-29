module BlockHead
  ( BlockHead (..)
  , getId
  , bip9
  , bip91
  , bip141
  , confirm
  , difficulty
  , findNewBits
  , sampleBS
  , no471744
  , no473759
  , genesisBlock
  , testnetGenesisBlock
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
import qualified Text.Megaparsec.Byte        as BP
import qualified Text.Megaparsec.Debug       as P
import           TxIn                        (TxIn (..))
import qualified TxIn
import           TxOut                       (TxOut (..))
import qualified TxOut
import           Tx                          (Tx (..))
import qualified Tx
import           Utils


-- | Record type representing a block head.
data BlockHead = BlockHead
  { bhVersion    :: Word32
  , bhPrevBlock  :: ByteString
  , bhMerkleRoot :: ByteString
  , bhTimestamp  :: Word32
  , bhBits       :: BlockBits
  , bhNonce      :: ByteString
  } deriving (Eq, Show)

instance Serializable BlockHead where
  serialize BlockHead {..} =
    -- {{{
       integralToNBytesLE 4  bhVersion
    <> LBS.take           32 bhPrevBlock  -- guaranteeing the byte count.
    <> LBS.take           32 bhMerkleRoot -- guaranteeing the byte count.
    <> integralToNBytesLE 4  bhTimestamp
    <> serialize             bhBits
    <> LBS.take           4  bhNonce      -- guaranteeing the byte count.
    -- }}}
  parser = do
    -- {{{
    bhVersion    <- word32ParserLE "block's version"
    bhPrevBlock  <- P.takeP  (Just "block's prev block" ) 32
    bhMerkleRoot <- P.takeP  (Just "block's merkle root") 32
    bhTimestamp  <- word32ParserLE "block's version"
    bhBits       <- parser
    bhNonce      <- P.takeP  (Just "block's nonce"      ) 4
    return $ BlockHead {..}
    -- }}}


-- | Get the inverse of the HASH256 of the serialized block.
getId :: BlockHead -> ByteString
getId = hash256 . serialize


-- | Check if block is BIP0009 compliant.
bip9 :: BlockHead -> Bool
bip9   BlockHead {..} = bhVersion `Bits.shiftR` 29        == zeroBits `setBit` 0


-- | Check if block is BIP0091 compliant.
bip91 :: BlockHead -> Bool
bip91  BlockHead {..} = (bhVersion `Bits.shiftR` 4) .&. 1 == 1


-- | Check if block is BIP0141 (Segwit) compliant.
bip141 :: BlockHead -> Bool
bip141 BlockHead {..} = (bhVersion `Bits.shiftR` 1) .&. 1 == 1


-- | Checks whether the proof-of-work is valid.
confirm :: BlockHead -> Bool
confirm block@BlockHead {..} =
  -- {{{
  bsToIntegerLE (getId block) < BlockBits.toTarget bhBits
  -- }}}


-- | Finds the difficulty value of the given `BlockHead`.
difficulty :: BlockHead -> Double
difficulty BlockHead {..} =
  -- {{{
  let
    target = fromIntegral $ BlockBits.toTarget bhBits
  in
  0xffff * 256 ^ (0x1d - 3) / target
  -- }}}


-- | Takes two blocks from the start to end of a 2016 range of blocks,
--   and returns a new `BlockBits` value based on the time it took
--   miners to mine this range of blocks.
findNewBits :: BlockHead -> BlockHead -> Either Text BlockBits
findNewBits start end =
  -- {{{
  let
    startTS, endTS :: Integer
    startTS   = fromIntegral $ bhTimestamp start
    endTS     = fromIntegral $ bhTimestamp end
    timeDiff  = clamp (threeAndAHalfDays, eightWeeks) $ endTS - startTS
    endTarget = BlockBits.toTarget (bhBits end)
    newTarget = (endTarget * timeDiff) `div` twoWeeks
  in
  BlockBits.fromTarget newTarget
  -- }}}


eithGenesisBlock :: ParseResult BlockHead
eithGenesisBlock = parse $ integerToBS 0x0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c
Right genesisBlock = eithGenesisBlock

eithTestnetGenesisBlock :: ParseResult BlockHead
eithTestnetGenesisBlock = parse $ integerToBS 0x0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff001d1aa4ae18
Right testnetGenesisBlock = eithTestnetGenesisBlock


-- SAMPLE VALUE
-- {{{
sampleBS :: ByteString
sampleBS = integralToNBytes 80  0x020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d

no471744 :: ByteString
no471744 = integralToNBytes 80 0x000000203471101bbda3fe307664b3283a9ef0e97d9a38a7eacd8800000000000000000010c8aba8479bbaa5e0848152fd3c2289ca50e1c3e58c9a4faaafbdf5803c5448ddb845597e8b0118e43a81d3

no473759 :: ByteString
no473759 = integralToNBytes 80  0x02000020f1472d9db4b563c35f97c428ac903f23b7fc055d1cfc26000000000000000000b3f449fcbe1bc4cfbcb8283a0d2c037f961a3fdf2b8bedc144973735eea707e1264258597e8b0118e5f00474
-- }}}

