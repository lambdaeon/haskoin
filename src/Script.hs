{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Script
  ( Stack (..)
  , ScriptSig
  , ScriptPubKey
  , Command (..)
  , Operation (..)
  , sampleStack0
  , sampleStack1
  , sampleStack2
  , sampleStack0BS
  , sampleStack1BS
  , sampleStack2BS
  ) where


import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as LBS
import           Data.Functor          (void)
import           Data.Serializable
import           Data.Varint           (Varint (..))
import qualified Data.Varint           as Varint
import           Data.Void
import           Data.Word             (Word8)
import qualified FieldElement          as FE
import           Text.Megaparsec       (Parsec)
import qualified Text.Megaparsec       as P
import qualified Text.Megaparsec.Debug as P
import qualified Text.Megaparsec.Byte  as BP
import           Utils


sampleStack0 :: Stack
sampleStack0 = Stack
  [ Element $ integerToBS 0x3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf2132060277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01
  , Element $ integerToBS 0x0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a
  ]
sampleStack0BS :: ByteString
sampleStack0BS =
  integerToBS 0x6b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf2132060277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a
  --                3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf2132060277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01  0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a

sampleStack1 :: Stack
sampleStack1 = Stack
  [ OpCommand OP_DUP
  , OpCommand OP_HASH160
  , Element $ integerToBS 0xbc3b654dca7e56b04dca18f2566cdaf02e8d9ada
  , OpCommand OP_EQUALVERIFY
  , OpCommand OP_CHECKSIG
  ]
sampleStack1BS :: ByteString
sampleStack1BS =
  integerToBS 0x1976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac


sampleStack2 :: Stack
sampleStack2 = Stack
  [ OpCommand OP_DUP
  , OpCommand OP_HASH160
  , Element $ integerToBS 0x1c4bc762dd5423e332166702cb75f40df79fea12
  , OpCommand OP_EQUALVERIFY
  , OpCommand OP_CHECKSIG
  ]
sampleStack2BS :: ByteString
sampleStack2BS =
  integerToBS 0x1976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac


data Command
  = Element   ByteString
  | OpCommand Operation
  deriving (Eq, Show)

instance Serializable Command where
  serialize (Element   bs) =
    -- {{{
    let
      len   = LBS.length bs
      lenBS = integralToBSLE len
    in
    if len < 76 then
      lenBS <> bs
    else if len >= 76 && len < 0x100 then
      (76 `LBS.cons'` lenBS) <> bs
    else if len >= 0x100 && len <= 520 then
      (77 `LBS.cons'` lenBS) <> bs
    else
      (77 `LBS.cons'` integralToBSLE 520) <> LBS.take 520 bs
    -- }}}
  serialize (OpCommand op) =
    -- {{{
    LBS.singleton $ operationToOpCode op
    -- }}}
  parser = do
    -- {{{
    fstByte <- P.dbg "C00" $ P.label "First byte of a script command" P.anySingle
    if fstByte >= 1 || fstByte <= 75 then do
      bytes <- P.dbg "C01" $ P.takeP (Just "Element bytes") (fromIntegral fstByte)
      return $ Element bytes
    else if fstByte == 76 then do
      opLen <- P.dbg "C02" $ P.label "Byte that indicates the length of the element" P.anySingle
      bytes <- P.dbg "C03" $ P.takeP (Just "Element bytes") (fromIntegral opLen)
      return $ Element bytes
    else if fstByte == 77 then do
      opLen <- P.dbg "C04" $ P.takeP (Just "Bytes that indicate the length of the element") 2
      bytes <- P.dbg "C05" $ P.takeP (Just "Element bytes") (fromIntegral $ bsToIntegerLE opLen)
      return $ Element bytes
    else do
      return $ OpCommand $ operationFromOpCode fstByte
    -- }}}


data Operation
  -- {{{
  = OP_0
  | OP_PUSHDATA1
  | OP_PUSHDATA2
  | OP_PUSHDATA4
  | OP_1NEGATE
  | OP_1
  | OP_2
  | OP_3
  | OP_4
  | OP_5
  | OP_6
  | OP_7
  | OP_8
  | OP_9
  | OP_10
  | OP_11
  | OP_12
  | OP_13
  | OP_14
  | OP_15
  | OP_16
  | OP_NOP
  | OP_IF
  | OP_NOTIF
  | OP_ELSE
  | OP_ENDIF
  | OP_VERIFY
  | OP_RETURN
  | OP_TOALTSTACK
  | OP_FROMALTSTACK
  | OP_2DROP
  | OP_2DUP
  | OP_3DUP
  | OP_2OVER
  | OP_2ROT
  | OP_2SWAP
  | OP_IFDUP
  | OP_DEPTH
  | OP_DROP
  | OP_DUP
  | OP_NIP
  | OP_OVER
  | OP_PICK
  | OP_ROLL
  | OP_ROT
  | OP_SWAP
  | OP_TUCK
  | OP_SIZE
  | OP_EQUAL
  | OP_EQUALVERIFY
  | OP_1ADD
  | OP_1SUB
  | OP_NEGATE
  | OP_ABS
  | OP_NOT
  | OP_0NOTEQUAL
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_BOOLAND
  | OP_BOOLOR
  | OP_NUMEQUAL
  | OP_NUMEQUALVERIFY
  | OP_NUMNOTEQUAL
  | OP_LESSTHAN
  | OP_GREATERTHAN
  | OP_LESSTHANOREQUAL
  | OP_GREATERTHANOREQUAL
  | OP_MIN
  | OP_MAX
  | OP_WITHIN
  | OP_RIPEMD160
  | OP_SHA1
  | OP_SHA256
  | OP_HASH160
  | OP_HASH256
  | OP_CODESEPARATOR
  | OP_CHECKSIG
  | OP_CHECKSIGVERIFY
  | OP_CHECKMULTISIG
  | OP_CHECKMULTISIGVERIFY
  | OP_NOP1
  | OP_CHECKLOCKTIMEVERIFY
  | OP_CHECKSEQUENCEVERIFY
  | OP_NOP4
  | OP_NOP5
  | OP_NOP6
  | OP_NOP7
  | OP_NOP8
  | OP_NOP9
  | OP_NOP10
  deriving (Eq, Show)
  -- }}}

operationFromOpCode :: Word8 -> Operation
operationFromOpCode opCode =
  -- {{{
  case opCode of
    0   -> OP_0
    76  -> OP_PUSHDATA1
    77  -> OP_PUSHDATA2
    78  -> OP_PUSHDATA4
    79  -> OP_1NEGATE
    81  -> OP_1
    82  -> OP_2
    83  -> OP_3
    84  -> OP_4
    85  -> OP_5
    86  -> OP_6
    87  -> OP_7
    88  -> OP_8
    89  -> OP_9
    90  -> OP_10
    91  -> OP_11
    92  -> OP_12
    93  -> OP_13
    94  -> OP_14
    95  -> OP_15
    96  -> OP_16
    97  -> OP_NOP
    99  -> OP_IF
    100 -> OP_NOTIF
    103 -> OP_ELSE
    104 -> OP_ENDIF
    105 -> OP_VERIFY
    106 -> OP_RETURN
    107 -> OP_TOALTSTACK
    108 -> OP_FROMALTSTACK
    109 -> OP_2DROP
    110 -> OP_2DUP
    111 -> OP_3DUP
    112 -> OP_2OVER
    113 -> OP_2ROT
    114 -> OP_2SWAP
    115 -> OP_IFDUP
    116 -> OP_DEPTH
    117 -> OP_DROP
    118 -> OP_DUP
    119 -> OP_NIP
    120 -> OP_OVER
    121 -> OP_PICK
    122 -> OP_ROLL
    123 -> OP_ROT
    124 -> OP_SWAP
    125 -> OP_TUCK
    130 -> OP_SIZE
    135 -> OP_EQUAL
    136 -> OP_EQUALVERIFY
    139 -> OP_1ADD
    140 -> OP_1SUB
    143 -> OP_NEGATE
    144 -> OP_ABS
    145 -> OP_NOT
    146 -> OP_0NOTEQUAL
    147 -> OP_ADD
    148 -> OP_SUB
    149 -> OP_MUL
    154 -> OP_BOOLAND
    155 -> OP_BOOLOR
    156 -> OP_NUMEQUAL
    157 -> OP_NUMEQUALVERIFY
    158 -> OP_NUMNOTEQUAL
    159 -> OP_LESSTHAN
    160 -> OP_GREATERTHAN
    161 -> OP_LESSTHANOREQUAL
    162 -> OP_GREATERTHANOREQUAL
    163 -> OP_MIN
    164 -> OP_MAX
    165 -> OP_WITHIN
    166 -> OP_RIPEMD160
    167 -> OP_SHA1
    168 -> OP_SHA256
    169 -> OP_HASH160
    170 -> OP_HASH256
    171 -> OP_CODESEPARATOR
    172 -> OP_CHECKSIG
    173 -> OP_CHECKSIGVERIFY
    174 -> OP_CHECKMULTISIG
    175 -> OP_CHECKMULTISIGVERIFY
    176 -> OP_NOP1
    177 -> OP_CHECKLOCKTIMEVERIFY
    178 -> OP_CHECKSEQUENCEVERIFY
    179 -> OP_NOP4
    180 -> OP_NOP5
    181 -> OP_NOP6
    182 -> OP_NOP7
    183 -> OP_NOP8
    184 -> OP_NOP9
    185 -> OP_NOP10
    _   -> OP_NOP
  -- }}}

operationToOpCode :: Operation -> Word8
operationToOpCode op =
  -- {{{
  case op of
    OP_0                   -> 0
    OP_PUSHDATA1           -> 76
    OP_PUSHDATA2           -> 77
    OP_PUSHDATA4           -> 78
    OP_1NEGATE             -> 79
    OP_1                   -> 81
    OP_2                   -> 82
    OP_3                   -> 83
    OP_4                   -> 84
    OP_5                   -> 85
    OP_6                   -> 86
    OP_7                   -> 87
    OP_8                   -> 88
    OP_9                   -> 89
    OP_10                  -> 90
    OP_11                  -> 91
    OP_12                  -> 92
    OP_13                  -> 93
    OP_14                  -> 94
    OP_15                  -> 95
    OP_16                  -> 96
    OP_NOP                 -> 97
    OP_IF                  -> 99
    OP_NOTIF               -> 100
    OP_ELSE                -> 103
    OP_ENDIF               -> 104
    OP_VERIFY              -> 105
    OP_RETURN              -> 106
    OP_TOALTSTACK          -> 107
    OP_FROMALTSTACK        -> 108
    OP_2DROP               -> 109
    OP_2DUP                -> 110
    OP_3DUP                -> 111
    OP_2OVER               -> 112
    OP_2ROT                -> 113
    OP_2SWAP               -> 114
    OP_IFDUP               -> 115
    OP_DEPTH               -> 116
    OP_DROP                -> 117
    OP_DUP                 -> 118
    OP_NIP                 -> 119
    OP_OVER                -> 120
    OP_PICK                -> 121
    OP_ROLL                -> 122
    OP_ROT                 -> 123
    OP_SWAP                -> 124
    OP_TUCK                -> 125
    OP_SIZE                -> 130
    OP_EQUAL               -> 135
    OP_EQUALVERIFY         -> 136
    OP_1ADD                -> 139
    OP_1SUB                -> 140
    OP_NEGATE              -> 143
    OP_ABS                 -> 144
    OP_NOT                 -> 145
    OP_0NOTEQUAL           -> 146
    OP_ADD                 -> 147
    OP_SUB                 -> 148
    OP_MUL                 -> 149
    OP_BOOLAND             -> 154
    OP_BOOLOR              -> 155
    OP_NUMEQUAL            -> 156
    OP_NUMEQUALVERIFY      -> 157
    OP_NUMNOTEQUAL         -> 158
    OP_LESSTHAN            -> 159
    OP_GREATERTHAN         -> 160
    OP_LESSTHANOREQUAL     -> 161
    OP_GREATERTHANOREQUAL  -> 162
    OP_MIN                 -> 163
    OP_MAX                 -> 164
    OP_WITHIN              -> 165
    OP_RIPEMD160           -> 166
    OP_SHA1                -> 167
    OP_SHA256              -> 168
    OP_HASH160             -> 169
    OP_HASH256             -> 170
    OP_CODESEPARATOR       -> 171
    OP_CHECKSIG            -> 172
    OP_CHECKSIGVERIFY      -> 173
    OP_CHECKMULTISIG       -> 174
    OP_CHECKMULTISIGVERIFY -> 175
    OP_NOP1                -> 176
    OP_CHECKLOCKTIMEVERIFY -> 177
    OP_CHECKSEQUENCEVERIFY -> 178
    OP_NOP4                -> 179
    OP_NOP5                -> 180
    OP_NOP6                -> 181
    OP_NOP7                -> 182
    OP_NOP8                -> 183
    OP_NOP9                -> 184
    OP_NOP10               -> 185
  -- }}}


newtype Stack = Stack
  { getStack :: [Command]
  } deriving (Eq, Show)

instance Serializable Stack where
  serialize (Stack commands) =
    -- {{{
    let
      foldFn currCmd soFar = serialize currCmd <> soFar
      allCmds              = foldr foldFn LBS.empty commands
      totLen               = Varint $ fromIntegral $ LBS.length allCmds
    in
    serialize totLen <> allCmds
    -- }}}
  parser = do
    -- {{{
    -- bytesCount <- Varint.countParser
    -- void Varint.countParser
    commands <- P.dbg "S01" $ Varint.lengthPrefixed parser
    return $ Stack commands
    -- }}}



type ScriptSig    = Stack
type ScriptPubKey = Stack
