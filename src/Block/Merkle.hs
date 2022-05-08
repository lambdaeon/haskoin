module Block.Merkle
  ( findRootFromHashes
  , findRootFromSerializables
  , findRoot
  , hashOneLevel
  , makeEmptyTree
  , Root (..)
  , FlagBits (..)
  , parent
  , treeHeight
  , treeWidthAt
  , sampleBS01
  , sampleBS02
  , sampleBS03
  , sampleBS04
  , sampleBS05
  , sampleBS06
  , sampleBS07
  , sampleBS08
  , sampleBS09
  , sampleBS10
  , sampleBS11
  , sampleBS12
  ) where


import qualified Data.Bits                   as Bits
import qualified Data.ByteString.Lazy        as LBS
import           Data.List                   (intercalate)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Serializable
import           Extension.List              (duplicateLast)
-- import qualified Data.Text                   as T
-- import qualified Extension.ByteString.Lazy   as LBS
-- import           Extension.ByteString.Parser  
import qualified Text.Megaparsec             as P
-- import qualified Text.Megaparsec.Byte        as BP
-- import qualified Text.Megaparsec.Debug       as P
import Utils


data TreeNode
  = EmptyNode
  | FilledNode ByteString
  deriving (Eq)
instance Show TreeNode where
  show EmptyNode = "EmptyNode"
  show (FilledNode bs) =
    -- {{{
    let
      firstFourBytes =
          LBS.take 4 bs
        & encodeHex
        & LBS.unpack
        & map (chr . fromIntegral)
    in
    "Node(" ++ firstFourBytes ++ ")"
    -- }}}


newtype TreeLevel = TreeLevel {getTreeLevel :: Map Word32 TreeNode}
instance Show TreeLevel where
  show (TreeLevel nodes) =
    -- {{{
       "["
    ++ Map.foldrWithKey'
         ( \k hp acc ->
                show k
             ++ "_"
             ++ show hp
             ++ (if null acc then "" else ", ")
             ++ acc
         )
         ""
         nodes
    ++ "]"
    -- }}}


hashesToTreeLevel :: [ByteString] -> TreeLevel
hashesToTreeLevel hashes =
  -- {{{
  nodesToTreeLevel $ FilledNode <$> hashes
  -- }}}


nodesToTreeLevel :: [TreeNode] -> TreeLevel
nodesToTreeLevel =
  -- {{{
  TreeLevel . Map.fromList . zip [0..]
  -- }}}


newtype Tree = Tree {getTree :: Map Word32 TreeLevel}
instance Show Tree where
  show (Tree levels) =
    -- {{{
    Map.foldrWithKey'
      ( \k lvl acc ->
          acc ++ "\n(" ++ show k ++ ") " ++ show lvl
      )
      ""
      levels
    -- }}}


makeEmptyTree :: Word32 -> Tree
makeEmptyTree ntx =
  -- {{{
  Tree $ Map.fromList
    [ ( i
      , TreeLevel $ Map.fromList
          [ (j - 1, EmptyNode)
          | j <- [1 ..  treeWidthAt ntx i]
          ]
      )
    | i <- [0 .. treeHeight ntx]
    ]
  -- }}}


findTreeLevelParent :: TreeLevel -> Either Text TreeLevel
findTreeLevelParent (TreeLevel nodes) =
  -- {{{
  let
    go acc []                                       =
      -- {{{
      Right $ reverse acc
      -- }}}
    go acc node@[FilledNode bs]                     =
      -- {{{
      if Map.size nodes == 1 then
        Right node
      else
        go (FilledNode (parent bs bs) : acc) []
      -- }}}
    go acc [FilledNode bs, EmptyNode]               =
      -- {{{
      if odd (Map.size nodes) then
        go (FilledNode (parent bs bs) : acc) []
      else
        Left "invalid tree."
      -- }}}
    go acc (FilledNode lBS : FilledNode rBS : rest) =
      -- {{{
      go (FilledNode (parent lBS rBS) : acc) rest
      -- }}}
    go acc (EmptyNode : EmptyNode : rest) =
      -- {{{
      go (EmptyNode : acc) rest
      -- }}}
    go _   _                                        =
      -- {{{
      Left "empty nodes."
      -- }}}
  in
  nodesToTreeLevel <$> go [] (Map.elems nodes)
  -- }}}


mergeTreeLevels :: TreeLevel -> TreeLevel -> Either Text TreeLevel
mergeTreeLevels (TreeLevel ns0) (TreeLevel ns1) =
  -- {{{
  if Map.size ns0 == Map.size ns1 then
    let
      unionFn node@(FilledNode _) _                   = node
      unionFn _                   node@(FilledNode _) = node
      unionFn _                   _                   = EmptyNode
    in
    return $ TreeLevel $ Map.unionWith unionFn ns0 ns1
  else
    Left "incompatible TreeLevel values for merge."
  -- }}}


updateNodeOfLevel :: Word32 -> (TreeNode -> TreeNode) -> TreeLevel -> TreeLevel
updateNodeOfLevel j updateFn (TreeLevel nodes) =
  -- {{{
  TreeLevel $ Map.update (return . updateFn) j nodes
  -- }}}


updateNodeAt :: Word32 -> Word32 -> (TreeNode -> TreeNode) -> Tree -> Tree
updateNodeAt i j updateFn (Tree levels) =
  -- {{{
  Tree $ Map.update (return . updateNodeOfLevel j updateFn) i levels
  -- }}}


getNodeAt :: Word32 -> Word32 -> Tree -> Maybe TreeNode
getNodeAt i j (Tree levels) = do
  TreeLevel nodes <- Map.lookup i levels
  Map.lookup j nodes


-- | Attemps to find the Merkle root from the given info within a
--   @merkleblock@ command.
findRoot :: Word32
         -> [ByteString]
         -> FlagBits
         -> Either Text Root
findRoot numTxs allHashes (FlagBits fbBools) =
  -- {{{
  let
    emptyTree  = makeEmptyTree numTxs
    maxH       = treeHeight numTxs
    findMaxJ h = treeWidthAt numTxs h - 1 -- safe (`treeWidthAt` doesn't return values less than 1)
    go fb hs currH currJ tree =
      -- {{{
      let
        downH  = currH - 1
        leftJ  = currJ * 2
        rightJ = currJ * 2 + 1
        fillCurrWithHash bs =
          -- {{{
          updateNodeAt currH currJ (const $ FilledNode bs) tree
          -- }}}
        goHelper newH newJ newFlagBits newHashes newTree =
          -- {{{
          go newFlagBits newHashes newH newJ newTree
          -- }}}
        goUp    =
          -- {{{
          goHelper (currH + 1) (currJ `div` 2)
          -- }}}
        goLeft  =
          -- {{{
          goHelper downH leftJ
          -- }}}
        goRight =
          -- {{{
          goHelper downH rightJ
          -- }}}
        mLeftChild  = getNodeAt downH leftJ  tree
        mRightChild = getNodeAt downH rightJ tree
        badFBHsFail extra =
          -- {{{
          Left $
               "invalid flagbits or hashes provided "
            <> "("
            <> extra
            <> ")."
          -- }}}
        mRootNode   = getNodeAt maxH 0 tree
      in
      case mRootNode of
        Nothing                ->
          -- {{{
          Left "invalid tree."
          -- }}}
        Just (FilledNode root) ->
          -- {{{
          Right $ Root root
          -- case myTrace "\nHERE\n" (fb, hs) of
          --   ([], []) ->
          --     -- {{{
          --     Right $ Root root
          --     -- }}}
          --   _        ->
          --     -- {{{
          --     Left "not all flag bits and/or hashes were consumed."
          --     -- }}}
          -- }}}
        Just EmptyNode         ->
          -- {{{
          if currH == 0 then
            -- {{{
            -- on a leaf, fill with the given hash, and go up.
            case (fb, hs) of
              (_ : flagBits, hash : hashes) ->
                -- {{{
                goUp flagBits hashes (fillCurrWithHash hash)
                -- }}}
              _                             ->
                -- {{{
                badFBHsFail "A"
                -- }}}
            -- }}}
          else
            -- {{{
            case mLeftChild of
              Nothing                     ->
                -- {{{
                Left "invalid merkle block."
                -- }}}
              Just EmptyNode              ->
                -- {{{
                case (fb, hs) of
                  (False : flagBits, hash : hashes) ->
                    -- {{{
                    goUp flagBits hashes (fillCurrWithHash hash)
                    -- }}}
                  (True  : flagBits, _            ) ->
                    -- {{{
                    goLeft flagBits hs tree
                    -- }}}
                  _                                 ->
                    -- {{{
                    badFBHsFail "B"
                    -- }}}
                -- }}}
              Just (FilledNode leftChild) ->
                -- {{{
                case mRightChild of
                  Nothing              ->
                    -- {{{
                    -- the lower level has an odd number of leaves
                    goUp fb hs (fillCurrWithHash $ parent leftChild leftChild)
                    -- }}}
                  Just EmptyNode       ->
                    -- {{{
                    goRight fb hs tree
                    -- }}}
                  Just (FilledNode rightChild) ->
                    -- {{{
                    goUp fb hs (fillCurrWithHash $ parent leftChild rightChild)
                    -- }}}
                -- }}}
            -- }}}
          -- }}}
      -- }}}
  in
  -- seq (myTrace "\nTHE TREE\n" emptyTree) $ Left "NOTHING"
  go (myTrace "\nFLAG BITS\n" fbBools) (myTrace "\nALL HASHES\n" allHashes) maxH 0 emptyTree
  -- }}}


-- | Datatype to represent the @FlagBits@ of a @merkleblock@ command.
newtype FlagBits = FlagBits {getFlagBits :: [Bool]} deriving (Eq)
instance Show FlagBits where
  show (FlagBits bits) =
    -- {{{
    intercalate
      "_"
      (map (\b -> if b then '1' else '0') <$> splitIn 8 bits)
    -- }}}

instance Serializable FlagBits where
  serialize (FlagBits bits) =
    -- {{{
    LBS.pack $ map boolsToWord8 $ splitIn 8 bits
    -- }}}
  parser = do
    -- {{{
    count <- fromIntegral <$> P.anySingle
    bytes <- P.takeP (Just "flagbits") count
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
findRootFromHashes :: [ByteString] -> Root
findRootFromHashes hashes =
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
findRootFromSerializables :: Serializable a => [a] -> Root
findRootFromSerializables =
  -- {{{
  findRootFromHashes . map serialize
  -- }}}


-- UTILS
-- {{{
-- | Computes the height of a Merkle tree.
--   
--   (from the original @haskoin@ package)
treeHeight :: Word32 -- ^ Number of transactions (leaf nodes)
           -> Word32 -- ^ Height of the merkle tree
treeHeight ntx
  -- {{{
  | ntx < 2   = 0
  | even ntx  = 1 + treeHeight (ntx `div` 2)
  | otherwise = treeHeight $ ntx + 1
  -- }}}


-- | Computes the width of a Merkle tree at a specific height.
--   The transactions are at height 0.
--
--   (from the original @haskoin@ package)
treeWidthAt :: Word32 -- ^ Number of transactions (leaf nodes)
            -> Word32 -- ^ Height at which we want to compute the width
            -> Word32 -- ^ Width of the Merkle tree
treeWidthAt ntx h' =
  -- {{{
  let
    h = fromIntegral h'
  in
  (ntx + (1 `Bits.shiftL` h) - 1) `Bits.shiftR` h
  -- }}}


-- | Finds the root using the data within a @merkleblock@ command.
--
--     (1) If the node's value is given in the hashes field, the
--         flag bit is 0.
--
--     2.  If the node is an internal node and the value is to be
--         calculated by the light client, the flag bit is 1.
--
--     3.  If the node is a leaf and is a transaction of interest,
--         the flag is 1 and the node’s value is also given in the
--         hashes field. These are the items proven to be included
--         in the Merkle tree.
findRootFromPartialTree :: Word32
                        -> [ByteString]
                        -> FlagBits
                        -> Either String Root
findRootFromPartialTree numTxs hashes (FlagBits fbBools) =
  let
    maxHeight     = treeHeight numTxs
    findCurrWidth = treeWidthAt numTxs
    treeMap = Map.fromList [(k, []) | k <- [0..maxHeight]]
  in
  -- \flagBit (index, currHeight, treeMap) ->
  --   let
  --     addHashToMapAt k ind =
  --       case hashes !? ind of
  --         Nothing ->
  --           fail ""
  --         Just hash ->
  --           (ind + 1, Map.insertWith' (++) k hash treeMap)
  --   in
  --   if flagBit then
  --     -- {{{
  --     if currHeight == 0 then
  --       addHashToMapAt 0 index
  --     else
  --       (index, treeMap)
  --     -- }}}
  --   else
  --     addHashToMapAt currHeight index
  undefined

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




