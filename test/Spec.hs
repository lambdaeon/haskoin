module Main where


import qualified BlockHead                   as BH
import qualified BlockBits
import           Debug.Trace                 (trace)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Either                 (isRight)
import           Data.Serializable
import           Data.Varint                 (Varint (..))
import qualified Data.Varint                 as Varint
import qualified Extension.ByteString.Lazy   as LBS
import           Extension.ByteString.Parser
import qualified Locktime
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified FieldElement                as FE
import qualified FiniteFieldEllipticCurve    as FFEC
import qualified Script
import qualified Script.PayToPubKeyHash      as P2PKH
import qualified Script.PayToScriptHash      as P2SH
import qualified ECC
import qualified Text.Megaparsec             as P
import qualified Tx
import qualified TxIn
import qualified TxOut
import           Utils

main :: IO ()
main = do
  eitherFaucetAddress <- return $ base58StringToBS "mkHS9ne12qx9pS9VojpwU5xtRd4T7X7ZUt"
  booksTargetAddress  <- return $ base58StringToBS "miKegze5FQNCnGw6PKyqUbYUeBa4x2hFeM"
  booksChangeAddress  <- return $ base58StringToBS "mzx5YhAH9kNHtcN481u6WkjeHjYtVeKVh2"
  let faucetAddress =
        case eitherFaucetAddress of
          Right addr -> addr
          Left  err  -> fromString $ show err
      realTxId01 = integerToBS 0x94a23976ff3a6aeb2786f154073a0d494414bf22b28781674ff5ea48e62ef33a
      realTxId02 = integerToBS 0xfaa5cdf8af84d2a1cd1c1f4c693296c979abdd2d97d7a6d61f5617dc35348b8a
      realTxId03 = integerToBS 0x38e7c4b7877c45327012bf7725f0568b494c26c2258859f92f67c18e8e830a72
  realTx01FetchRes <- runExceptT $ Tx.fetch True realTxId01
  realTx02FetchRes <- runExceptT $ Tx.fetch True realTxId02
  realTx03FetchRes <- runExceptT $ Tx.fetch True realTxId03
  let verifyFetchRes fetchRes =
        case fetchRes of
          Right tx ->
            runExceptT $ Tx.verifyFor Script.P2PKH tx
          Left err ->
            return $ Left err
  realTx01Verified <- verifyFetchRes realTx01FetchRes
  realTx02Verified <- verifyFetchRes realTx02FetchRes
  realTx03Verified <- verifyFetchRes realTx03FetchRes
  --
  synthTx03 <- runExceptT $
                 P2PKH.testnetPayTo
                   realTxId03
                   0
                   60_000
                   faucetAddress
  synthTx03Verified <- verifyFetchRes synthTx03
  -----------------------------------------------------------------------------
  hspec $ do
    let point223 x y = FFEC.fromCoords x y :: Either Text (FFEC.Point 223 0 7)
    describe "\nChapter 3 - Exercise 1" $ do
      -- {{{
      it "Point (192,105) is on the curve." $ do
        (isRight $ point223 192 105) `shouldBe` True
      it "Point (17 , 56) is on the curve." $ do
        (isRight $ point223 17 56)   `shouldBe` True
      it "Point (200,119) is NOT on the curve." $ do
        (isRight $ point223 200 119) `shouldBe` False
      it "Point (1  ,193) is on the curve." $ do
        (isRight $ point223 1 193)   `shouldBe` True
      it "Point (42 , 99) is NOT on the curve." $ do
        (isRight $ point223 42 99)   `shouldBe` False
      -- }}}

    describe "\nChapter 3 - Exercise 2" $ do
      -- {{{
      it "Point (170,142) + (60 ,139) is (220,181)." $ do
        ((<>) <$> point223 170 142 <*> point223 60 139) `shouldBe` (point223 220 181)
      it "Point (47 , 71) + (17 , 56) is (215, 68)." $ do
        ((<>) <$> point223 47 71 <*> point223 17 56)    `shouldBe` (point223 215 68)
      it "Point (143, 98) + (76 , 66) is (47 , 71)." $ do
        ((<>) <$> point223 143 98 <*> point223 76 66)   `shouldBe` (point223 47 71)
      -- }}}

    describe "\nChapter 3 - Exercise 4" $ do
      -- {{{
      it " 2⋅(192,105) is (49 , 71)." $ do
        ((FFEC.scaleBy 2) <$> point223 192 105) `shouldBe` (point223 49 71)
      it " 2⋅(143, 98) is (64 ,168)." $ do
        ((FFEC.scaleBy 2) <$> point223 143 98)  `shouldBe` (point223 64 168)
      it " 2⋅(47 , 71) is (36 ,111)." $ do
        ((FFEC.scaleBy 2) <$> point223 47 71)   `shouldBe` (point223 36 111)
      it " 4⋅(47 , 71) is (194, 51)." $ do
        ((FFEC.scaleBy 4) <$> point223 47 71)   `shouldBe` (point223 194 51)
      it " 8⋅(47 , 71) is (116, 55)." $ do
        ((FFEC.scaleBy 8) <$> point223 47 71)   `shouldBe` (point223 116 55)
      it "21⋅(47 , 71) is  infinity." $ do
        ((FFEC.scaleBy 21) <$> point223 47 71)  `shouldBe` (Right mempty)
      -- }}}

    describe "\nChapter 3 - Exercise 5" $ do
      -- {{{
      it "Order of the group generated by (15,86) is 7." $ do
        (FFEC.findOrderFrom <$> point223 15 86) `shouldBe` (Right 7)
      -- }}}

    describe "\nChapter 3 - Exercise 6" $ do
      -- {{{
      let ex3_6_P =
            FFEC.fromCoords
              0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
              0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34
      it "Signature #1 is valid." $ do
        shouldBe
          ( ( \p ->
                ECC.verify
                  p
                  0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60
                  ( ECC.Signature
                      { ECC.r = 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395
                      , ECC.s = 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4
                      }
                  )
            ) <$> ex3_6_P
          )
          (Right True)
      it "Signature #2 is valid." $ do
        shouldBe
          ( ( \p ->
                ECC.verify
                  p
                  0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d
                  ( ECC.Signature 
                      { ECC.r = 0xeff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c
                      , ECC.s = 0xc7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6
                      }
                  )
            ) <$> ex3_6_P
          )
          (Right True)
      -- }}}

    describe "\nChapter 3 - Exercise 7" $ do
      -- {{{
      let e = 12345
          k = 1234567890
          z = fromInteger $ bsToInteger $ hash256 "Programming Bitcoin!"
      it "Successfully signed \"Programming Bitcoin!\"." $ do
        shouldBe
          ( ECC.signWith e k z
          )
          ( Right $ ECC.Signature
              { ECC.r = 0x2b698a0f0a4041b77e63488ad48c23e8e8838dd1fb7520408b121697b782ef22
              , ECC.s = 0x1dbc63bfef4416705e602a7b564161167076d8b20990a0f26f316cff2cb0bc1a
              }
          )
      -- }}}

    describe "\nChapter 4 - Exercise 1" $ do
      -- {{{
      let fromSecret sec =
            let
              pub = ECC.pubKeyOf sec
            in
            encodeHex $ ECC.toSEC False pub
      it "Successfully found the uncompressed public key associated with 5000." $ do
        (fromSecret 5000) `shouldBe` (fromString "04ffe558e388852f0120e46af2d1b370f85854a8eb0841811ece0e3e03d282d57c315dc72890a4f10a1481c031b03b351b0dc79901ca18a00cf009dbdb157a1d10")
      it "Successfully found the uncompressed public key associated with 2018^5." $ do
        (fromSecret $ 2018 ^ 5) `shouldBe` (fromString "04027f3da1918455e03c46f659266a1bb5204e959db7364d2f473bdf8f0a13cc9dff87647fd023c13b4a4994f17691895806e1b40b57f4fd22581a4f46851f3b06")
      it "Successfully found the uncompressed public key associated with 0xdeadbeef12345." $ do
        (fromSecret 0xdeadbeef12345) `shouldBe` (fromString "04d90cd625ee87dd38656dd95cf79f65f60f7273b67d3096e68bd81e4f5342691f842efa762fd59961d0e99803c61edba8b3e3f7dc3a341836f97733aebf987121")
      -- }}}

    describe "\nChapter 4 - Exercise 2" $ do
      -- {{{
      let fromSecret sec =
            let
              pub = ECC.pubKeyOf sec
            in
            encodeHex $ ECC.toSEC True pub
      it "Successfully found the compressed public key associated with 5001." $ do
        (fromSecret 5001) `shouldBe` (fromString "0357a4f368868a8a6d572991e484e664810ff14c05c0fa023275251151fe0e53d1")
      it "Successfully found the compressed public key associated with 2019^5." $ do
        (fromSecret $ 2019 ^ 5) `shouldBe` (fromString "02933ec2d2b111b92737ec12f1c5d20f3233a0ad21cd8b36d0bca7a0cfa5cb8701")
      it "Successfully found the compressed public key associated with 0xdeadbeef54321." $ do
        (fromSecret 0xdeadbeef54321) `shouldBe` (fromString "0296be5b1292f6c856b3c5654e886fc13511462059089cdf9c479623bfcbe77690")
      -- }}}

    describe "\nChapter 4 - Exercise 3" $ do
      -- {{{
      it "DER format of the given signature was found successfully." $ do
        shouldBe
          ( encodeHex $ serialize $ ECC.Signature
              { ECC.r = 0x37206a0610995c58074999cb9767b87af4c4978db68c06e8e6e81d282047a7c6
              , ECC.s = 0x8ca63759c1157ebeaec0d03cecca119fc9a75bf8e6d0fa65c841c8e2738cdaec
              }
          )
          "3045022037206a0610995c58074999cb9767b87af4c4978db68c06e8e6e81d282047a7c60221008ca63759c1157ebeaec0d03cecca119fc9a75bf8e6d0fa65c841c8e2738cdaec"
      -- }}}

    describe "\nChapter 4 - Exercise 4" $ do
      -- {{{
      it "Base58 encoding of 0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d correctly found." $ do
        shouldBe
          (showBase58EncodedBS $ integerToBase58 0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d)
          (Right "9MA8fRQrT4u8Zj8ZRd6MAiiyaxb2Y1CMpvVkHQu5hVM6")
      it "Base58 encoding of 0xeff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c   correctly found." $ do
        shouldBe
          (showBase58EncodedBS $ integerToBase58 0xeff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c)
          (Right "4fE3H2E6XMp4SsxtwinF7w9a34ooUrwWe4WsW1458Pd")
      it "Base58 encoding of 0xc7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6 correctly found." $ do
        shouldBe
          (showBase58EncodedBS $ integerToBase58 0xc7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6)
          (Right "EQJsjkd6JaGwxrjEhfeqPenqHwrBmPQZjJGNSCHBkcF7")
      -- }}}

    describe "\nChapter 4 - Exercise 5" $ do
      -- {{{
      let fromSecret comp test sec =
            let
              pub = ECC.pubKeyOf sec
            in
            showBase58EncodedBS $ ECC.address comp test pub
      it "Successfully found the address corresponding to 5002." $ do
        shouldBe
          (fromSecret False True 5002)
          (Right "mmTPbXQFxboEtNRkwfh6K51jvdtHLxGeMA")
      it "Successfully found the address corresponding to 2020^5." $ do
        shouldBe
          (fromSecret True True $ 2020 ^ 5)
          (Right "mopVkxp8UhXqRYbCYJsbeE1h1fiF64jcoH")
      it "Successfully found the address corresponding to 0x12345deadbeef." $ do
        shouldBe
          (fromSecret True False 0x12345deadbeef)
          (Right "1F1Pn2y6pDb68E5nYJJeba4TLg2U7B6KF1")
      -- }}}

    describe "\nChapter 4 - Exercise 6" $ do
      -- {{{
      it "Successfully found the WIF of 5003." $ do
        shouldBe
          (showBase58EncodedBS $ ECC.wifOf True True 5003)
          (Right "cMahea7zqjxrtgAbB7LSGbcQUr1uX1ojuat9jZodMN8rFTv2sfUK")
      it "Successfully found the WIF of 2021^5." $ do
        shouldBe
          (showBase58EncodedBS $ ECC.wifOf False True $ 2021 ^ 5)
          (Right "91avARGdfge8E4tZfYLoxeJ5sGBdNJQH4kvjpWAxgzczjbCwxic")
      it "Successfully found the WIF of 0x54321deadbeef." $ do
        shouldBe
          (showBase58EncodedBS $ ECC.wifOf True False 0x54321deadbeef)
          (Right "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgiuQJv1h8Ytr2S53a")
      -- }}}

    describe "\nChapter 4 - Exercise 9" $ do
      -- {{{
      let addrStr   = showBase58EncodedBS ECC.testnetWallet
          fstLetter = take 1 <$> addrStr
      it ("The public address of the testnet wallet is: " ++ (show addrStr)) $ do
        (fstLetter == Right "m" || fstLetter == Right "n") `shouldBe` True
      -- }}}

    describe "\nChapter 5 - Exercise 1" $ do
      -- {{{
      let testTx = integerToBS 0x010000000456919960ac691763688d3d3bcea9ad6ecaf875df5339e148a1fc61c6ed7a069e010000006a47304402204585bcdef85e6b1c6af5c2669d4830ff86e42dd205c0e089bc2a821657e951c002201024a10366077f87d6bce1f7100ad8cfa8a064b39d4e8fe4ea13a7b71aa8180f012102f0da57e85eec2934a82a585ea337ce2f4998b50ae699dd79f5880e253dafafb7feffffffeb8f51f4038dc17e6313cf831d4f02281c2a468bde0fafd37f1bf882729e7fd3000000006a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937feffffff567bf40595119d1bb8a3037c356efd56170b64cbcc160fb028fa10704b45d775000000006a47304402204c7c7818424c7f7911da6cddc59655a70af1cb5eaf17c69dadbfc74ffa0b662f02207599e08bc8023693ad4e9527dc42c34210f7a7d1d1ddfc8492b654a11e7620a0012102158b46fbdff65d0172b7989aec8850aa0dae49abfb84c81ae6e5b251a58ace5cfeffffffd63a5e6c16e620f86f375925b21cabaf736c779f88fd04dcad51d26690f7f345010000006a47304402200633ea0d3314bea0d95b3cd8dadb2ef79ea8331ffe1e61f762c0f6daea0fabde022029f23b3e9c30f080446150b23852028751635dcee2be669c2a1686a4b5edf304012103ffd6f4a67e94aba353a00882e563ff2722eb4cff0ad6006e86ee20dfe7520d55feffffff0251430f00000000001976a914ab0c0b2e98b1ab6dbf67d4750b0a56244948a87988ac005a6202000000001976a9143c82d7df364eb6c75be8c80df2b3eda8db57397088ac46430600
      it "The sample serialized tx is meant for version 1." $ do
        P.runParser parser "" testTx `parseSatisfies` ((== 1) . Tx.txVersion)
      -- }}}

    describe "\nChapter 5 - Exercise Mine01" $ do
      -- {{{
      let varint0 = Varint 0xffffffedcb112200
          ser0    = serialize varint0
          varint1 = Varint 0xfdfead234
          ser1    = serialize varint1
          varint2 = Varint 0x45671
          ser2    = serialize varint2
      it "Serialized and parsed back 0xffffffedcb112200 successfully." $ do
        P.runParser parser "" ser0 `shouldParse` varint0
      it "Serialized and parsed back 0xfdfead234        successfully." $ do
        P.runParser parser "" ser1 `shouldParse` varint1
      it "Serialized and parsed back 0x45671            successfully." $ do
        P.runParser parser "" ser2 `shouldParse` varint2
      -- }}}

    describe "\nChapter 5 - Exercise 5" $ do
      -- {{{
      let testTx     = integerToBS 0x010000000456919960ac691763688d3d3bcea9ad6ecaf875df5339e148a1fc61c6ed7a069e010000006a47304402204585bcdef85e6b1c6af5c2669d4830ff86e42dd205c0e089bc2a821657e951c002201024a10366077f87d6bce1f7100ad8cfa8a064b39d4e8fe4ea13a7b71aa8180f012102f0da57e85eec2934a82a585ea337ce2f4998b50ae699dd79f5880e253dafafb7feffffffeb8f51f4038dc17e6313cf831d4f02281c2a468bde0fafd37f1bf882729e7fd3000000006a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937feffffff567bf40595119d1bb8a3037c356efd56170b64cbcc160fb028fa10704b45d775000000006a47304402204c7c7818424c7f7911da6cddc59655a70af1cb5eaf17c69dadbfc74ffa0b662f02207599e08bc8023693ad4e9527dc42c34210f7a7d1d1ddfc8492b654a11e7620a0012102158b46fbdff65d0172b7989aec8850aa0dae49abfb84c81ae6e5b251a58ace5cfeffffffd63a5e6c16e620f86f375925b21cabaf736c779f88fd04dcad51d26690f7f345010000006a47304402200633ea0d3314bea0d95b3cd8dadb2ef79ea8331ffe1e61f762c0f6daea0fabde022029f23b3e9c30f080446150b23852028751635dcee2be669c2a1686a4b5edf304012103ffd6f4a67e94aba353a00882e563ff2722eb4cff0ad6006e86ee20dfe7520d55feffffff0251430f00000000001976a914ab0c0b2e98b1ab6dbf67d4750b0a56244948a87988ac005a6202000000001976a9143c82d7df364eb6c75be8c80df2b3eda8db57397088ac46430600
          txParseRes = P.runParser parser "" $ trace ("THE BYTESTRING: " ++ show (LBS.chunksOf 2 $ encodeHex testTx)) testTx
          (mScriptSig, mScriptPubKey, mAmount) =
            -- {{{
            case trace ("PARSE RESULT: " ++ show txParseRes) txParseRes of
              Right tx ->
                -- {{{
                case (Tx.txTxIns tx, Tx.txTxOuts tx) of
                  ( _ : sndIn : _, fstOut : sndOut : _) ->
                    ( Right $ TxIn.txInScriptSig      sndIn
                    , Right $ TxOut.txOutScriptPubKey fstOut
                    , Right $ TxOut.txOutAmount       sndOut
                    )
                  _ ->
                    (Left "", Left "", Left "")
                -- }}}
              _ ->
                -- {{{
                (Left "", Left "", Left "")
                -- }}}
            -- }}}
          scriptSigAns =
            -- {{{
            Right $ Script.Script
              [ Script.Element $ integralToBS 0x304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601
              , Script.Element $ integralToBS 0x035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937
              ]
            -- }}}
          scriptPubKeyAns =
            -- {{{
            Right $ Script.Script
              [ Script.OpCommand Script.OP_DUP
              , Script.OpCommand Script.OP_HASH160
              , Script.Element $ integralToBS 0xab0c0b2e98b1ab6dbf67d4750b0a56244948a879
              , Script.OpCommand Script.OP_EQUALVERIFY
              , Script.OpCommand Script.OP_CHECKSIG
              ]
            -- }}}
          amountAns = Right 40_000_000
      it "Successfully parsed the scriptSig." $ do
        mScriptSig `shouldBe` scriptSigAns
      it "Successfully parsed the scriptPubKey." $ do
        mScriptPubKey `shouldBe` scriptPubKeyAns
      it "Successfully parsed the amount." $ do
        mAmount `shouldBe` amountAns
      -- }}}

    describe "\nChapter 6 - Exercise Mine01" $ do
      -- {{{
      let ser0    = serialize Script.sampleScript0
          target0 = Script.sampleScript0BS
          ser1    = serialize Script.sampleScript1
          target1 = Script.sampleScript1BS
          ser2    = serialize Script.sampleScript2
          target2 = Script.sampleScript2BS
      it "Script sample 0 serialized correctly." $ do
        ser0 `shouldBe` target0
      it "Script sample 0 parsed correctly." $ do
        P.runParser parser "" target0 `shouldParse` Script.sampleScript0
      it "Script sample 1 serialized correctly." $ do
        ser1 `shouldBe` target1
      it "Script sample 1 parsed correctly." $ do
        P.runParser parser "" target1 `shouldParse` Script.sampleScript1
      it "Script sample 2 serialized correctly." $ do
        ser2 `shouldBe` target2
      it "Script sample 2 parsed correctly." $ do
        P.runParser parser "" target2 `shouldParse` Script.sampleScript2
      -- }}}

    describe "\nChapter 6 - Exercise Mine02" $ do
      -- {{{
      let parseRes :: ParseResult Tx.Tx
          parseRes = P.runParser parser "" Tx.sampleTxBS
      it "Transaction sample parsed correctly." $ do
        (isRight parseRes) `shouldBe` True
      -- }}}

    describe "\nChapter 6 - Exercise Mine03" $ do
      -- {{{
      let int0, int1, int2 :: Integer
          int0 = (-2002)
          int1 = (-0xdeadbeef)
          int2 = 0
          fn0 = bsToSignedIntegralLE . signedIntegralToBSLE
          fn1 = bsToSignedIntegral . signedIntegralToBS
      it "Successfully encoded and decoded back -2002." $ do
        fn0 int0 `shouldBe` int0
      it "Successfully encoded and decoded back -0xdeadbeef." $ do
        (fn0 int1 + fn1 int1) `shouldBe` (2 * int1)
      it "Successfully encoded and decoded back 0." $ do
        fn0 int2 `shouldBe` int2
      -- }}}

    describe "\nChapter 6 - Exercise 3" $ do
      -- {{{
      let parseRes :: ParseResult Script.ScriptPubKey
          parseRes = P.runParser parser "" $ integerToBS 0x767695935687
          givenScriptPubKey :: Script.ScriptPubKey
          givenScriptPubKey = Script.Script $ fmap Script.OpCommand
            [ Script.OP_DUP
            , Script.OP_MUL
            , Script.OP_ADD
            , Script.OP_6
            , Script.OP_EQUAL
            ]
          validScriptSig :: Script.ScriptSig
          validScriptSig = Script.Script
            [ Script.Element $ LBS.singleton 2
            , Script.Element $ LBS.singleton 2
            ]
      it "Correctly parsed the serialized ScriptPubKey." $ do
        parseRes `shouldParse` givenScriptPubKey
      it "[Element 2, Element 2] is a valid ScriptSig." $ do
        Script.validate validScriptSig givenScriptPubKey 0 `shouldBe` (return ())
      -- }}}

    describe "\nChapter 6 - Exercise 4" $ do
      -- {{{
      let bsLength = serialize $ Varint.Varint 8
          initBS   = integerToBS 0x6e879169a77ca787
          parseRes :: ParseResult Script.Script
          parseRes = P.runParser parser "" $ bsLength <> initBS
          ans' =
            Script.Script $ fmap
              (Script.OpCommand . Script.operationFromOpCode)
              [0x6e, 0x87, 0x91, 0x69, 0xa7, 0x7c, 0xa7, 0x87]
          ans = trace (show ans') ans'
      it "Correctly parsed the serialized script." $ do
        parseRes `shouldParse` ans
      -- from trace log:
      -- [ OP_2DUP
      -- , OP_EQUAL
      -- , OP_NOT
      -- , OP_VERIFY
      -- , OP_SHA1
      -- , OP_SWAP
      -- , OP_SHA1
      -- , OP_EQUAL
      -- ]
      -- }}}

    describe "\nChapter 7 - Exercise Mine01" $ do
      -- {{{
      let tx0         = integerToBS 0x020000000001019bf22550fa6b14809dd687d37dfa23afa15dd3d736b899e3f01bd28af8b6976d0000000000feffffff0200fa000000000000160014cef19aa386112bb20380ccc65051795b252355bea6e40a64080000001600142469fce274f64624cae710a75fb411c71e394ad0024730440220276416e5ce64bcc8cfcf03e947101731f7da38f47d703829ce928d6dbbac6f5a02204d5754ff26d32ff8f397a88638ad748d04c0a4c93bd8241caecb712ae8b7f240012102b72bcf2aa53793c8d0e4a61c66575a929a38035c005c755dd7580a4c0d7b6df26e702100
          tx0ParseRes = P.runParser parser "" tx0 :: ParseResult Tx.Tx
      it "A real transaction on the testnet parsed successfully." $ do
        (isRight tx0ParseRes) `shouldBe` True
      -- }}}

    describe "\nChapter 7 - Exercise Mine02" $ do
      -- {{{
      it "A real transaction on the testnet fetched and parsed successfully." $ do
        (isRight realTx01FetchRes) `shouldBe` True
      it "A real transaction on the testnet fetched and parsed successfully." $ do
        (isRight realTx02FetchRes) `shouldBe` True
      it "A real transaction on the testnet fetched and parsed successfully." $ do
        (isRight realTx03FetchRes) `shouldBe` True
      -- }}}

    describe "\nChapter 7 - Exercise Mine03" $ do
      -- {{{
      let trc = myTrace "\n\nVERIFICATION RESULT: "
      it "A real transaction on the testnet fetched, parsed and verified successfully." $ do
        (trc realTx01Verified) `shouldBe` (return ())
      it "A real transaction on the testnet fetched, parsed and verified successfully." $ do
        (trc realTx02Verified) `shouldBe` (return ())
      it "A real transaction on the testnet fetched, parsed and verified successfully." $ do
        (trc realTx03Verified) `shouldBe` (return ())
      -- }}}

    describe "\nChapter 7 - Exercise 4" $ do
      -- {{{
      it "Successfully created a real transaction." $ do
        (seq (myTrace "\n\nTHE TX: " (encodeHex . serialize <$> synthTx03)) $ isRight synthTx03) `shouldBe` True
      it "The created transaction is valid." $ do
        (myTrace "\nRESULT OF VERIFYING THE TX: " synthTx03Verified) `shouldBe` (Right ())
      -- }}}

    describe "\nChapter 8 - Exercise 3" $ do
      -- {{{
      let h160 = integerToBS 0x74d691da1574e6b3c192ecfb52cc8984ee7b6c56
          p2shAddr = showBase58EncodedBS $ P2SH.hash160ToAddress False h160
      it "P2SH address from the HASH160 ('74d691da1574e6b3c192ecfb52cc8984ee7b6c56') is '3CLoMMyuoDQTPRD3XYZtCvgvkadrAdvdXh'." $ do
        p2shAddr `shouldBe` (Right "3CLoMMyuoDQTPRD3XYZtCvgvkadrAdvdXh")
      -- }}}

    describe "\nChapter 8 - Exercise 4" $ do
      -- {{{
      let parseRes :: ParseResult Tx.Tx
          parseRes = P.runParser parser "" Tx.sampleP2SH
      it "Sample pay-to-script-hash transaction verified successfully." $ do
        (isRight $ Tx.verifyFor Script.P2SH <$> parseRes) `shouldBe` True
      -- }}}

    describe "\nChapter 9 - Exercise 3" $ do
      -- {{{
      let parseRes :: ParseResult BH.BlockHead
          parseRes = P.runParser parser "" BH.sampleBS
          parseRes471744 :: ParseResult BH.BlockHead
          parseRes471744 = P.runParser parser "" BH.no471744
          parseRes473759 :: ParseResult BH.BlockHead
          parseRes473759 = P.runParser parser "" BH.no473759
      it "Successfully parsed a sample block." $ do
        (isRight parseRes) `shouldBe` True
      it "Successfully parsed block#471744." $ do
        (isRight parseRes471744) `shouldBe` True
      it "Successfully parsed block#473759." $ do
        (isRight parseRes473759) `shouldBe` True
      -- }}}

    describe "\nChapter 9 - Exercise 5" $ do
      -- {{{
      let parseRes :: ParseResult BH.BlockHead
          parseRes = P.runParser parser "" BH.sampleBS
          ans = 0x0000000000000000007e9e4c586439b0cdbe13b1370bdd9435d76a644d047523
      it "Successfully found the ID of a sample block." $ do
        ((bsToIntegerLE . BH.getId) <$> parseRes) `shouldBe` (Right ans)
      -- }}}

    describe "\nChapter 9 - Exercise 9" $ do
      -- {{{
      let mBlockBits = BlockBits.fromByteString $ integerToBS 0xe93c0118
      it "Successfully found the target value from a sample BlockBits." $ do
        shouldBe
          (BlockBits.toTarget <$> mBlockBits)
          (Right 0x13ce9000000000000000000000000000000000000000000)
      -- }}}

    describe "\nChapter 9 - Exercise 10" $ do
      -- {{{
      let parseRes :: ParseResult BH.BlockHead
          parseRes = P.runParser parser "" BH.sampleBS
          eithDiff = BH.difficulty <$> parseRes
          compFn x = x == 888171856257.3206
      it "Difficulty of the given sample computed successfully." $ do
        (compFn <$> (myTrace "\nDIFFICULTY VALUE: " eithDiff)) `shouldBe` (Right True)
      -- }}}

    describe "\nChapter 9 - Exercise 11" $ do
      -- {{{
      let parseRes :: ParseResult BH.BlockHead
          parseRes = P.runParser parser "" BH.sampleBS
          parseRes471744 :: ParseResult BH.BlockHead
          parseRes471744 = P.runParser parser "" BH.no471744
          parseRes473759 :: ParseResult BH.BlockHead
          parseRes473759 = P.runParser parser "" BH.no473759
      it "Block proof-of-work confirmation succeeded for a sample." $ do
        (BH.confirm <$> parseRes) `shouldBe` (Right True)
      it "Block proof-of-work confirmation succeeded for block#471744." $ do
        (BH.confirm <$> parseRes471744) `shouldBe` (Right True)
      it "Block proof-of-work confirmation succeeded for block#473759." $ do
        (BH.confirm <$> parseRes473759) `shouldBe` (Right True)
      -- }}}

    describe "\nChapter 9 - Exercise 12" $ do
      -- {{{
      let parseRes471744 =
            mapLeft
              (const "block 471744 parse failed.") 
              (P.runParser parser "" BH.no471744)
          parseRes473759 =
            mapLeft
              (const "block 473759 parse failed.")
              (P.runParser parser "" BH.no473759)
          newBits = do
            bEnd   <- parseRes471744 -- The order of these two seems misplace,
            bStart <- parseRes473759 -- but this is how the books does it.
            BH.findNewBits bStart bEnd
      it "New bits from block#471744 and block#473759 found successfully." $ do
        shouldBe
          newBits
          ( Right $ BlockBits.BlockBits
              { BlockBits.bbExp       = 0x17
              , BlockBits.bbLeftByte  = 0x80
              , BlockBits.bbMidByte   = 0xdf
              , BlockBits.bbRightByte = 0x62
              }
          )
      -- }}}



