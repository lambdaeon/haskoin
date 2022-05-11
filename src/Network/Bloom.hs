module Network.Bloom
  ( BloomFilter
  , makeFilter
  , addItemToFilter
  , getBitField
  , generateBitField
  ) where


import           Data.Serializable
import           Data.Varint     (Varint (..))
import qualified Data.Varint     as Varint
import qualified Text.Megaparsec as P
import           Utils


bip37Constant :: Word32
bip37Constant = 0xfba4c795


-- | Helper function for generating the bit field of a `BloomFilter` value.
generateBitField :: Integer
                 -> (ByteString -> Integer)
                 -> [ByteString]
                 -> [Bool]
generateBitField size hashFn sources =
  -- {{{
  let
    sourceToIndex src     = hashFn src `mod` size
    mapFn target ind curr = curr || target == fromIntegral ind
  in
  foldr
    (\src bits -> indexedMap (mapFn $ sourceToIndex src) bits)
    (replicate (fromIntegral size) False)
    sources
  -- }}}


-- | Opaque record type representing a Bloom filter.
data BloomFilter = BloomFilter
  { bfBitField  :: [Bool]
  , bfFuncCount :: Word32
  , bfTweak     :: Word32
  } deriving (Eq, Show)

instance Serializable BloomFilter where
  serialize BloomFilter {..} =
    -- {{{
       serialize (Varint $ fromIntegral $ length bfBitField `div` 8)
    <> boolsToBS bfBitField
    <> serialize bfFuncCount
    <> serialize bfTweak
    -- }}}
  parser = do
    -- {{{
    byteCount   <- Varint.countParser
    bfBitField  <- bsToBools <$> P.takeP (Just "bit field bytes.") byteCount
    bfFuncCount <- parser
    bfTweak     <- parser
    return $ BloomFilter {..}
    -- }}}


-- | Getter handle to accommodate `BloomFilter`'s opaqueness.
getBitField :: BloomFilter -> [Bool]
getBitField BloomFilter {..} = bfBitField


-- | Smart constructor for a `BloomFilter` value.
makeFilter :: Integer -> Word32 -> Word32 -> [ByteString] -> BloomFilter
makeFilter fieldSize bfFuncCount bfTweak items =
  -- {{{
  let
    size       = fieldSize * 8
    seed i     = i * bip37Constant + bfTweak
    initBits   = replicate (fromIntegral size) False
    bfBitField =
      -- {{{
      foldr
        (   zipWith (||)
          . ( \i ->
                generateBitField
                  size
                  (fromIntegral . murmur3 (seed i))
                  items
            )
        )
        initBits
        [0 .. bfFuncCount - 1]
      -- }}}
  in
  BloomFilter {..}
  -- }}}


-- | Updates its given `BloomFilter` to include an additional item.
addItemToFilter :: ByteString -> BloomFilter -> BloomFilter
addItemToFilter newItem initBF =
  -- {{{
  let
    bitField  = bfBitField initBF
    funcCount = bfFuncCount initBF
    tweak     = bfTweak initBF
    bf = makeFilter (fromIntegral $ length bitField `div` 8) funcCount tweak [newItem]
  in
  BloomFilter
    { bfBitField  = zipWith (||) (bfBitField bf) bitField
    , bfFuncCount = funcCount
    , bfTweak     = tweak
    }
  -- }}}


