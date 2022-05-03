module Block.Merkle where


import qualified Data.ByteString.Lazy        as LBS
import           Data.List                   (intercalate)
import           Data.Serializable
import           Extension.List              (duplicateLast)
-- import qualified Data.Text                   as T
-- import qualified Extension.ByteString.Lazy   as LBS
-- import           Extension.ByteString.Parser  
import qualified Text.Megaparsec             as P
-- import qualified Text.Megaparsec.Byte        as BP
-- import qualified Text.Megaparsec.Debug       as P
import Utils


-- Datatype to represent the @FlagBits@ of a @merkleblock@ command.
newtype FlagBits = FlagBits {getFlagBits :: [Bool]} deriving (Eq)
instance Show FlagBits where
  show (FlagBits bits) =
    intercalate
      "_"
      (map (\b -> if b then '1' else '0') <$> splitIn 8 bits)

instance Serializable FlagBits where
  serialize (FlagBits bits) =
    -- {{{
    LBS.pack $ map boolsToWord8 $ splitIn 8 bits
    -- }}}
  parser = do
    -- {{{
    bytes <- P.takeRest
    return $ FlagBits $ concat $ word8ToBools <$> LBS.unpack bytes
    -- }}}


-- | Wrapper for `ByteString` Merkle root.
newtype Root = Root {getRoot :: ByteString} deriving (Eq)

instance Show Root where
  show (Root bs) =
    -- {{{
    let
      showFn bs' =
          encodeHex bs'
        & LBS.unpack
        & map (chr . fromIntegral)
      firstFourBytes =
          LBS.take 4 bs -- should be 32 bytes long
        & showFn
      lastFourBytes =
          LBS.drop (LBS.length bs - 4) bs
        & showFn
    in
    firstFourBytes ++ "..." ++ lastFourBytes
    -- }}}


-- | Concatenate and return the HASH256 of the result.
parent :: ByteString -> ByteString -> ByteString
parent b0 b1 = hash256 $ b0 <> b1


-- | One step towards the Merkle root. Doesn't do anything if there is
--   one item or less in the list. Duplicates the last one if there are
--   an odd number of items, before applying the `parent` function to
--   pairs.
hashOneLevel :: [ByteString] -> [ByteString]
hashOneLevel hs =
  -- {{{
  let
    foldFn currBS (mRightBS, acc) =
      -- {{{
      case mRightBS of
        Nothing ->
          (Just currBS, acc)
        Just rightBS ->
          (Nothing, parent currBS rightBS : acc)
      -- }}}
    elemCount = length hs
  in
  if elemCount <= 1 then
    hs
  else if odd elemCount then
    hashOneLevel $ duplicateLast hs
  else
    snd $ foldr foldFn (Nothing, []) hs
  -- }}}


-- | Assumes the input is an ordered list of hashes. It first reverses
--   each hash, before recursively applying the `hashOneLevel` until
--   a single hash remains. Finally returns the reverse of that
--   singular hash.
findRoot :: [ByteString] -> Root
findRoot hashes =
  -- {{{
  let
    go []     = Root LBS.empty
    go [root] = Root $ LBS.reverse root
    go hs     = go $ hashOneLevel hs
  in
  go $ map LBS.reverse hashes
  -- }}}


-- | A more general version of `findRoot`. Converts each `Serializable`
--   value to a `ByteString` before passing the list to `findRoot`.
findRoot' :: Serializable a => [a] -> Root
findRoot' =
  -- {{{
  findRoot . map serialize
  -- }}}



sampleBS01 = integerToBS 0xc117ea8ec828342f4dfb0ad6bd140e03a50720ece40169ee38bdc15d9eb64cf5
sampleBS02 = integerToBS 0xc131474164b412e3406696da1ee20ab0fc9bf41c8f05fa8ceea7a08d672d7cc5
sampleBS03 = integerToBS 0xf391da6ecfeed1814efae39e7fcb3838ae0b02c02ae7d0a5848a66947c0727b0
sampleBS04 = integerToBS 0x3d238a92a94532b946c90e19c49351c763696cff3db400485b813aecb8a13181
sampleBS05 = integerToBS 0x10092f2633be5f3ce349bf9ddbde36caa3dd10dfa0ec8106bce23acbff637dae
sampleBS06 = integerToBS 0x7d37b3d54fa6a64869084bfd2e831309118b9e833610e6228adacdbd1b4ba161
sampleBS07 = integerToBS 0x8118a77e542892fe15ae3fc771a4abfd2f5d5d5997544c3487ac36b5c85170fc
sampleBS08 = integerToBS 0xdff6879848c2c9b62fe652720b8df5272093acfaa45a43cdb3696fe2466a3877
sampleBS09 = integerToBS 0xb825c0745f46ac58f7d3759e6dc535a1fec7820377f24d4c2c6ad2cc55c0cb59
sampleBS10 = integerToBS 0x95513952a04bd8992721e9b7e2937f1c04ba31e0469fbe615a78197f68f52b7c
sampleBS11 = integerToBS 0x2e6d722e5e4dbdf2447ddecc9f7dabb8e299bae921c99ad5b0184cd9eb8e5908
sampleBS12 = integerToBS 0xb13a750047bc0bdceb2473e5fe488c2596d7a7124b4e716fdd29b046ef99bbf0




