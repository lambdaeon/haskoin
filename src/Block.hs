module Block
  ( Bits (..)
  , bitsToTarget
  , bitsFromByteString
  , Hash (..)
  , Header (..)
  , getId
  , bip9
  , bip91
  , bip141
  , confirm
  , verifyChain
  , difficulty
  , findNewBits
  , sampleHeaderBS
  , no471744
  , no473759
  , genesisBlock
  , testnetGenesisBlock
  ) where


import qualified Block.Merkle                as Merkle
import           Data.Bits                   ((.&.), setBit, zeroBits)
import qualified Data.Bits                   as Bits
import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import qualified Data.Text                   as T
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


-- ** Bits
-- {{{
-- | An explicit datatype to represent the @bits@ field of a block header.
data Bits = Bits
  { bitsExp       :: Word8
  , bitsLeftByte  :: Word8
  , bitsMidByte   :: Word8
  , bitsRightByte :: Word8
  } deriving (Eq, Show)


instance Serializable Bits where
  serialize Bits {..} =
    -- {{{
    LBS.pack [bitsLeftByte, bitsMidByte, bitsRightByte, bitsExp]
    -- }}}
  parser = do
    -- {{{
    bitsLeftByte  <- P.anySingle
    bitsMidByte   <- P.anySingle
    bitsRightByte <- P.anySingle
    bitsExp       <- P.anySingle
    return $ Bits {..}
    -- }}}


-- | Finds the mining target based on a `Bits` value.
bitsToTarget :: Bits -> Integer
bitsToTarget Bits {..} =
  -- {{{
  let
    coeff =
      bsToIntegerLE $ LBS.pack [bitsLeftByte, bitsMidByte, bitsRightByte]
    exp   =
      fromIntegral bitsExp
  in
  coeff * 256 ^ (exp - 3)
  -- }}}


-- | Converts an integer back to a `Bits` value.
bitsFromTarget :: Integer -> Either Text Bits
bitsFromTarget n =
  -- {{{
  let
    bs = integerToBS n
  in
  case LBS.unpack bs of
    b0 : b1 : rest | b0 > 0x7f ->
      -- {{{
      Right $ Bits
        { bitsExp       = fromIntegral (LBS.length bs) + 1
        , bitsLeftByte  = b1
        , bitsMidByte   = b0
        , bitsRightByte = 0x00
        }
      -- }}}
    b0 : b1 : b2 : rest ->
      -- {{{
      Right $ Bits
        { bitsExp       = fromIntegral (LBS.length bs)
        , bitsLeftByte  = b2
        , bitsMidByte   = b1
        , bitsRightByte = b0
        }
      -- }}}
    _                        ->
      -- {{{
      Left "invalid target."
      -- }}}
  -- }}}


-- | A more explicit function (compared to its parser) to get a `Bits`
--   from a 4 byte long `ByteString`.
bitsFromByteString :: ByteString -> Either Text Bits
bitsFromByteString bs =
  -- {{{
  case LBS.unpack bs of
    [bitsLeftByte, bitsMidByte, bitsRightByte, bitsExp] ->
      Right $ Bits {..}
    _                                           ->
      Left "invalid bytestring for bits."
  -- }}}


lowestBits :: Bits
lowestBits = Bits
  { bitsExp       = 0x1d
  , bitsLeftByte  = 0xff
  , bitsMidByte   = 0xff
  , bitsRightByte = 0x00
  }
-- }}}


newtype Hash = Hash {getHash :: ByteString}


-- ** Header
-- {{{
-- | Record type representing a block head.
data Header = Header
  { headerVersion    :: Word32
  , headerPrevBlock  :: ByteString
  , headerMerkleRoot :: ByteString
  , headerTimestamp  :: Word32
  , headerBits       :: Bits
  , headerNonce      :: ByteString
  } deriving (Eq)

instance Show Header where
  show bh =
    -- {{{
    let
      firstFourBytes =
        getId bh
          & getHash
          & LBS.take 4
          & encodeHex
          & LBS.unpack
          & map (chr . fromIntegral)
    in
    "Block Header: " ++ firstFourBytes
    -- }}}

instance Serializable Header where
  serialize Header {..} =
    -- {{{
       integralToNBytesLE 4  headerVersion
    <> LBS.take           32 headerPrevBlock  -- guaranteeing the byte count.
    <> LBS.take           32 headerMerkleRoot -- guaranteeing the byte count.
    <> integralToNBytesLE 4  headerTimestamp
    <> serialize             headerBits
    <> LBS.take           4  headerNonce      -- guaranteeing the byte count.
    -- }}}
  parser = do
    -- {{{
    headerVersion    <- parser
    headerPrevBlock  <- P.takeP  (Just "block's prev block" ) 32
    headerMerkleRoot <- P.takeP  (Just "block's merkle root") 32
    headerTimestamp  <- parser
    headerBits       <- parser
    headerNonce      <- P.takeP  (Just "block's nonce"      ) 4
    return $ Header {..}
    -- }}}


-- | Get the inverse of the HASH256 of the serialized block.
getId :: Header -> Hash
getId = Hash . hash256 . serialize


-- | Check if block is BIP0009 compliant.
bip9 :: Header -> Bool
bip9   Header {..} = headerVersion `Bits.shiftR` 29        == zeroBits `setBit` 0


-- | Check if block is BIP0091 compliant.
bip91 :: Header -> Bool
bip91  Header {..} = (headerVersion `Bits.shiftR` 4) .&. 1 == 1


-- | Check if block is BIP0141 (Segwit) compliant.
bip141 :: Header -> Bool
bip141 Header {..} = (headerVersion `Bits.shiftR` 1) .&. 1 == 1


-- | Checks whether the proof-of-work is valid.
confirm :: Header -> Bool
confirm block@Header {..} =
  -- {{{
  bsToIntegerLE (getHash $ getId block) < bitsToTarget headerBits
  -- }}}


-- | Finds the difficulty value of the given `Header`.
difficulty :: Header -> Double
difficulty Header {..} =
  -- {{{
  let
    target = fromIntegral $ bitsToTarget headerBits
  in
  0xffff * 256 ^ (0x1d - 3) / target
  -- }}}


-- | Takes two blocks from the start to end of a 2016 range of blocks,
--   and returns a new `Bits` value based on the time it took
--   miners to mine this range of blocks.
findNewBits :: Header -> Header -> Either Text Bits
findNewBits start end =
  -- {{{
  let
    startTS, endTS :: Integer
    startTS   = fromIntegral $ headerTimestamp start
    endTS     = fromIntegral $ headerTimestamp end
    timeDiff  = clamp (threeAndAHalfDays, eightWeeks) $ endTS - startTS
    endTarget = bitsToTarget (headerBits end)
    newTarget = (endTarget * timeDiff) `div` twoWeeks
  in
  bitsFromTarget newTarget
  -- }}}


-- | Checks the validity of chain of `Header` values.
verifyChain :: [Header] -> Either Text ()
verifyChain []                  =
  -- {{{
  Left "empty chain."
  -- }}}
verifyChain [bh]                =
  -- {{{
  if confirm bh then
    Right ()
  else
    Left $ explainInvalidPOW bh
  -- }}}
verifyChain (bh0 : bh1 : chain) =
  -- {{{
  let
    go []          _     = Right ()
    go (b0 : rest) prevB
      -- {{{
      | not (confirm b0) =
          Left $ explainInvalidPOW b0
      | headerPrevBlock b0 /= getHash (getId prevB) =
          Left $ T.pack ("broken chain at: " ++ show b0)
      | otherwise =
          go rest b0
      -- }}}
  in
  go (bh1 : chain) bh0
  -- }}}


explainInvalidPOW :: Header -> Text
explainInvalidPOW bh =
  T.pack ("invalid proof-of-work at: " ++ show bh)


eithGenesisBlock :: ParseResult Header
eithGenesisBlock = parse $ integerToBS 0x0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c
Right genesisBlock = eithGenesisBlock

eithTestnetGenesisBlock :: ParseResult Header
eithTestnetGenesisBlock = parse $ integerToBS 0x0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff001d1aa4ae18
Right testnetGenesisBlock = eithTestnetGenesisBlock


-- SAMPLE VALUE
-- {{{
sampleHeaderBS :: ByteString
sampleHeaderBS = integralToNBytes 80  0x020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d

no471744 :: ByteString
no471744 = integralToNBytes 80 0x000000203471101bbda3fe307664b3283a9ef0e97d9a38a7eacd8800000000000000000010c8aba8479bbaa5e0848152fd3c2289ca50e1c3e58c9a4faaafbdf5803c5448ddb845597e8b0118e43a81d3

no473759 :: ByteString
no473759 = integralToNBytes 80  0x02000020f1472d9db4b563c35f97c428ac903f23b7fc055d1cfc26000000000000000000b3f449fcbe1bc4cfbcb8283a0d2c037f961a3fdf2b8bedc144973735eea707e1264258597e8b0118e5f00474
-- }}}
-- }}}



