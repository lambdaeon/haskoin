module Script
  ( Script (..)
  , ScriptSig
  , ScriptPubKey
  , Command (..)
  , Operation (..)
  , operationFromOpCode
  , operationToOpCode
  , validate
  , sampleScript0
  , sampleScript1
  , sampleScript2
  , sampleScript0BS
  , sampleScript1BS
  , sampleScript2BS
  ) where


import qualified Data.ByteString.Lazy      as LBS
import           Data.Serializable
import           Data.Varint               (Varint (..))
import qualified Data.Varint               as Varint
import           ECC                       (SigHash)
import qualified ECC
import qualified Extension.ByteString.Lazy as LBS
import qualified FieldElement              as FE
import qualified Text.Megaparsec           as P
import qualified Text.Megaparsec.Debug     as P
import           SECP256K1
import           Utils


-- OPERATION
-- {{{
-- | Sum type representing defined operations in Bitcoin's @Script@.
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
  deriving (Eq, Show, Bounded)
  -- }}}


-- | Convertion from an @OpCode@ to an `Operation` value.
--   Returns `OP_RETURN` for invalid inputs.
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
    _   -> OP_RETURN
  -- }}}


-- | Inverse of `operationFromOpCode`.
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
-- }}}


-- COMMAND
-- {{{
-- | Wrapper sum type to represent elements of script stacks.
data Command
  = Element   ByteString
  | OpCommand Operation
  deriving (Eq)

instance Show Command where
  show (Element bs)   = "Element 0x" ++ map (chr . fromIntegral) (LBS.unpack $ encodeHex bs)
  show (OpCommand op) = show op

instance Serializable Command where
  -- | Serialization of `Element` values silently caps at @520@ bytes.
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
  -- | The @OP_PUSHDATA4@ (OpCode 78) *is* handled, but at most @520@
  --   after the length designator byte are consumed.
  parser = do
    -- {{{
    fstByte <- P.label "First byte of a script command" P.anySingle
    if fstByte >= 1 && fstByte <= 75 then do
      bytes <- P.takeP (Just "Element bytes") (fromIntegral fstByte)
      return $ Element bytes
    else if fstByte == 76 then do
      opLen <- P.label "Byte that indicates the length of the element" P.anySingle
      bytes <- P.takeP (Just "Element bytes") (fromIntegral opLen)
      return $ Element bytes
    else if fstByte == 77 then do
      opLen <- P.takeP (Just "Bytes that indicate the length of the element") 2
      bytes <- P.takeP (Just "Element bytes") (fromIntegral $ bsToIntegerLE opLen)
      return $ Element bytes
    else if fstByte == 78 then do
      opLen <- P.takeP (Just "Bytes that indicate the length of the element") 4
      bytes <- P.takeP (Just "Element bytes") (min 520 $ fromIntegral $ bsToIntegerLE opLen)
      return $ Element bytes
    else do
      return $ OpCommand $ operationFromOpCode fstByte
    -- }}}
-- }}}


-- SCRIPT
-- {{{
-- | Newtype wrapper for a list of `Command` values. First element of
--   the list is treated as the top.
newtype Script = Script
  { getScript :: [Command]
  } deriving (Eq, Show)

instance Semigroup Script where
  (Script s1) <> (Script s2) = Script $ s1 <> s2

instance Monoid Script where
  mempty = Script []

instance Serializable Script where
  -- | Results in a varint prefixed serialization, which represents
  --   the total byte count that follows it.
  serialize (Script commands) =
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
    commands <- Varint.lengthPrefixed parser
    return $ Script commands
    -- }}}
-- }}}


-- | Type alias for readability (used in a `TxIn` field).
type ScriptSig    = Script


-- | Type alias for readability (used in a `TxOut` field).
type ScriptPubKey = Script


-- | Record type representing a stack which `Command` values are
--   meant to affect.
data Stack = Stack
  { stackMain :: [ByteString]
  , stackAlt  :: [ByteString]
  } deriving (Eq)

instance Show Stack where
  show (Stack main _) = show $ encodeHex <$> main

emptyStack = Stack [] []


-- | Evaluation of the resulting stack from putting the `ScriptSig` value
--   on top of the `ScriptPubKey` stack. `SigHash` @z@ is provided by
--   the `TxIn` verification function.
validate :: ScriptSig -> ScriptPubKey -> SigHash -> Either Text ()
validate scriptSig scriptPubKey z =
  -- {{{
  void $
    updateStack
      (scriptSig <> scriptPubKey)
      (integralToBS z)
      emptyStack
  -- }}}


-- | A recursive function to apply `Command` values to a stack until
--   no `Command` values remain. In case of failure, `Left` is returned.
--
--   According to Bitcoin's wiki, some failed operations are meant to
--   check whether stack's top value is specifically @0@ or not, while the
--   book is checking for "falsiness" (i.e. whether @0@ or empty). I've
--   decided to follow the book and check for falsiness.
--
--   Some of the affected operations are: `OP_IFDUP`,
--   `OP_NOT`, `OP_0NOTEQUAL`, `OP_BOOLAND`, and `OP_BOOLOR`.
updateStack :: Script -> ByteString -> Stack -> Either Text Stack
updateStack (Script script) z stack@(Stack main alt) =
  -- {{{
  let
    noOp           rest = updateStack (Script rest) z   stack
    appendMain bs  rest = updateStack (Script rest) z $ Stack (bs : main) alt
    appendAlt  bs  rest = updateStack (Script rest) z $ Stack main (bs : alt)
    appendNum  x        = appendMain (encodeNum x)
    genericFail         = Left "invalid script."
    fewElemsFail opLbl  = Left $ "not enough elements in stack for " <> opLbl <> "."
    opOnHead    op rest =
      -- {{{
      case main of
        h : t ->
          updateStack (Script rest) z $ Stack (op h : t) alt
        _ ->
          genericFail
      -- }}}
    opOnHead2   op rest =
      -- {{{
      case main of
        h2 : h1 : t ->
          updateStack (Script rest) z $ Stack (op h2 h1 : t) alt
        _ ->
          genericFail
      -- }}}
    opOnNum     op rest =
      -- {{{
      opOnHead (encodeNum . op . decodeNum) rest
      -- }}}
    opOnTwoNums op rest =
      -- {{{
      opOnHead2 -- v-- b is the topmost element.
        (\b a -> encodeNum $ decodeNum a `op` decodeNum b)
        rest
      -- }}}
    compTwoNums pred rest =
      -- {{{
      opOnHead2
        ( \b a -> -- b is the topmost element.
            if decodeNum a `pred` decodeNum b then
              LBS.singleton 1
            else
              LBS.singleton 0
        )
        rest
      -- }}}
    performAndVerify fstCmd rest =
      -- {{{
      updateStack (Script $ OpCommand fstCmd : OpCommand OP_VERIFY : rest) z stack
      -- }}}
    zMsg :: ECC.SigHash
    zMsg = fromInteger $ bsToInteger z
  in
  case script of
    []                                      ->
      case main of
        [] ->
          Left "no elements remained in stack."
        result : _ ->
          if bsIsFalse result then
            Left "stack reduced to 0x00."
          else
            Right stack
    Element elemBS                   : cmds ->
      appendMain elemBS cmds
    OpCommand OP_0                   : cmds ->
      appendNum 0 cmds
    OpCommand OP_PUSHDATA1           : cmds ->
      noOp cmds
    OpCommand OP_PUSHDATA2           : cmds ->
      noOp cmds
    OpCommand OP_PUSHDATA4           : cmds ->
      noOp cmds
    OpCommand OP_1NEGATE             : cmds ->
      appendNum (-1) cmds
    OpCommand OP_1                   : cmds ->
      appendNum 1 cmds
    OpCommand OP_2                   : cmds ->
      appendNum 2 cmds
    OpCommand OP_3                   : cmds ->
      appendNum 3 cmds
    OpCommand OP_4                   : cmds ->
      appendNum 4 cmds
    OpCommand OP_5                   : cmds ->
      appendNum 5 cmds
    OpCommand OP_6                   : cmds ->
      appendNum 6 cmds
    OpCommand OP_7                   : cmds ->
      appendNum 7 cmds
    OpCommand OP_8                   : cmds ->
      appendNum 8 cmds
    OpCommand OP_9                   : cmds ->
      appendNum 9 cmds
    OpCommand OP_10                  : cmds ->
      appendNum 10 cmds
    OpCommand OP_11                  : cmds ->
      appendNum 11 cmds
    OpCommand OP_12                  : cmds ->
      appendNum 12 cmds
    OpCommand OP_13                  : cmds ->
      appendNum 13 cmds
    OpCommand OP_14                  : cmds ->
      appendNum 14 cmds
    OpCommand OP_15                  : cmds ->
      appendNum 15 cmds
    OpCommand OP_16                  : cmds ->
      appendNum 16 cmds
    OpCommand OP_NOP                 : cmds ->
      noOp cmds
    OpCommand OP_IF                  : cmds ->
      -- {{{
      case main of
        [] ->
          -- {{{
          Left "empty stack."
          -- }}}
        headBS : restOfMain ->
          -- {{{
          let
            endOfGo restOfCmds accedInReverse =
              -- {{{
              fmap
                (Script restOfCmds,)
                ( updateStack
                    (Script $ reverse accedInReverse)
                    z
                    (Stack restOfMain alt)
                )
              -- }}}
          in
          if bsIsFalse headBS then
            -- Skip to the corresponding OP_ELSE or OP_ENDIF:
            -- {{{
            let
              go :: Word
                 -> Bool
                 -> [Command]
                 -> [Command]
                 -> Either Text (Script, Stack)
              go nestLevel inElse acc remainingCmds =
              -- Seek for OP_ELSE or OP_ENDIF.
              -- In case of OP_ELSE, accumulate
              -- the commands up until its OP_ENDIF.
              -- {{{
                case remainingCmds of
                  [] ->
                    -- {{{
                    Left "if block without closure."
                    -- }}}
                  currCmd : restOfCmds ->
                    -- {{{
                    let
                      nestLevelHelper op =
                        -- {{{
                        go
                          (nestLevel `op` 1)
                          inElse
                          ( if inElse then
                              currCmd : acc
                            else
                              acc
                          )
                          restOfCmds
                        -- }}}
                      increaseNestLevel = nestLevelHelper (+)
                      decreaseNestLevel = nestLevelHelper (-)
                      continue          = nestLevelHelper const
                    in
                    case currCmd of
                      OpCommand OP_IF    ->
                        -- {{{
                        increaseNestLevel
                        -- }}}
                      OpCommand OP_NOTIF ->
                        -- {{{
                        increaseNestLevel
                        -- }}}
                      OpCommand OP_ELSE  ->
                        -- {{{
                        if nestLevel == 0 then
                          -- {{{
                          if inElse then
                            -- {{{
                            Left "if block without closure."
                            -- }}}
                          else
                            -- {{{
                            go nestLevel True acc restOfCmds
                            -- }}}
                          -- }}}
                        else
                          -- {{{
                          decreaseNestLevel
                          -- }}}
                        -- }}}
                      OpCommand OP_ENDIF ->
                        -- {{{
                        if nestLevel == 0 then
                          -- {{{
                          if inElse then
                            -- {{{
                            endOfGo restOfCmds acc
                            -- }}}
                          else
                            -- {{{
                            Right (Script restOfCmds, Stack restOfMain alt)
                            -- }}}
                          -- }}}
                        else
                          -- {{{
                          decreaseNestLevel
                          -- }}}
                        -- }}}
                      _                  ->
                        -- {{{
                        continue
                        -- }}}
                    -- }}}
              -- }}}
            in
            case go 0 False [] cmds of
              Left err ->
                Left err
              Right (newScript, newStack) ->
                updateStack newScript z newStack
            -- }}}
          else
            -- Accumulate up until the corresponding OP_ELSE or OP_ENDIF:
            -- {{{
            let
              go :: Word -> [Command] -> [Command] -> Either Text (Script, Stack)
              go nestLevel acc remainingCmds =
              -- Keep accumulating up until an
              -- OP_ELSE or OP_ENDIF is found.
              -- {{{
                case remainingCmds of
                  [] ->
                    -- {{{
                    Left "if block without closure."
                    -- }}}
                  currCmd : restOfCmds ->
                    -- {{{
                    let
                      nestLevelHelper op =
                        -- {{{
                        go
                          (nestLevel `op` 1)
                          (currCmd : acc)
                          restOfCmds
                        -- }}}
                      increaseNestLevel = nestLevelHelper (+)
                      decreaseNestLevel = nestLevelHelper (-)
                      continue          = nestLevelHelper const
                      endRecursion      = endOfGo restOfCmds acc
                    in
                    case currCmd of
                      OpCommand OP_IF    ->
                        -- {{{
                        increaseNestLevel
                        -- }}}
                      OpCommand OP_NOTIF ->
                        -- {{{
                        increaseNestLevel
                        -- }}}
                      OpCommand OP_ELSE  ->
                        -- {{{
                        if nestLevel == 0 then
                          -- {{{
                          endRecursion
                          -- }}}
                        else
                          -- {{{
                          decreaseNestLevel
                          -- }}}
                        -- }}}
                      OpCommand OP_ENDIF ->
                        -- {{{
                        if nestLevel == 0 then
                          -- {{{
                          endRecursion
                          -- }}}
                        else
                          -- {{{
                          decreaseNestLevel
                          -- }}}
                        -- }}}
                      _                  ->
                        -- {{{
                        continue
                        -- }}}
                    -- }}}
              -- }}}
            in
            case go 0 [] cmds of
              Left err ->
                Left err
              Right (newScript, newStack) ->
                updateStack newScript z newStack
            -- }}}
          -- }}}
      -- }}}
    OpCommand OP_NOTIF               : cmds ->
      -- {{{
      case main of
        [] ->
          -- {{{
          Left "empty stack."
          -- }}}
        headBS : restOfMain ->
          -- {{{
          updateStack
            (Script $ OpCommand OP_IF : cmds)
            z
            (Stack (bsBooleanToggle headBS : restOfMain) alt)
          -- }}}
      -- }}}
    OpCommand OP_ELSE                : cmds ->
      genericFail
    OpCommand OP_ENDIF               : cmds ->
      genericFail
    OpCommand OP_VERIFY              : cmds ->
      -- {{{
      case main of
        []            ->
          Left "nothing left to OP_VERIFY."
        headBS : restOfMain ->
          if bsIsFalse headBS then
            Left "OP_VERIFY failed."
          else
            updateStack (Script cmds) z (Stack restOfMain alt)
      -- }}}
    OpCommand OP_RETURN              : cmds ->
      Left "op_return, or invalid command."
    OpCommand OP_TOALTSTACK          : cmds ->
      -- {{{
      case main of
        [] ->
          Left "empty stack."
        mainHead : restOfMain ->
          updateStack (Script cmds) z (Stack restOfMain (mainHead : alt))
      -- }}}
    OpCommand OP_FROMALTSTACK        : cmds ->
      -- {{{
      case alt of
        [] ->
          Left "empty alt stack."
        altHead : restOfAlt ->
          updateStack (Script cmds) z (Stack (altHead : main) restOfAlt)
      -- }}}
    OpCommand OP_2DROP               : cmds ->
      -- {{{
      case main of
        _ : _ : restOfMain ->
          updateStack (Script cmds) z (Stack restOfMain alt)
        _ ->
          fewElemsFail "OP_2DROP"
      -- }}}
    OpCommand OP_2DUP                : cmds ->
      -- {{{
      case main of
        head2 : head1 : _ ->
          updateStack
            (Script cmds)
            z
            (Stack (head2 : head1 : main) alt)
        _ ->
          fewElemsFail "OP_2DUP"
      -- }}}
    OpCommand OP_3DUP                : cmds ->
      -- {{{
      case main of
        head3 : head2 : head1 : _ ->
          updateStack
            (Script cmds)
            z
            (Stack (head3 : head2 : head1 : main) alt)
        _ ->
          fewElemsFail "OP_3DUP"
      -- }}}
    OpCommand OP_2OVER               : cmds ->
      -- {{{
      case main of
        _ : _ : head2 : head1 : _ ->
          updateStack
            (Script cmds)
            z
            (Stack (head2 : head1 : main) alt)
        _ ->
          fewElemsFail "OP_2OVER"
      -- }}}
    OpCommand OP_2ROT                : cmds ->
      -- {{{
      case main of
        head6 : head5 : head4 : head3 : head2 : head1 : restOfMain ->
          updateStack
            (Script cmds)
            z
            (Stack (head2 : head1 : head6 : head5 : head4 : head3 : restOfMain) alt)
        _ ->
          fewElemsFail "OP_2ROT"
      -- }}}
    OpCommand OP_2SWAP               : cmds ->
      -- {{{
      case main of
        head4 : head3 : head2 : head1 : restOfMain ->
          updateStack
            (Script cmds)
            z
            (Stack (head2 : head1 : head4 : head3 : restOfMain) alt)
        _ ->
          fewElemsFail "OP_2SWAP"
      -- }}}
    OpCommand OP_IFDUP               : cmds ->
      -- {{{
      case main of
        head1 : _ ->
          if bsIsFalse head1 then
            -- From the bitcoin wiki:
            -- ``` If the top stack value is not 0, duplicate it.
            -- ```
            -- Although I've decided to follow the book and used
            -- "falsiness" as the predicate.
            noOp cmds
          else
            updateStack
              (Script cmds)
              z
              (Stack (head1 : main) alt)
        _ ->
          fewElemsFail "OP_IFDUP"
      -- }}}
    OpCommand OP_DEPTH               : cmds ->
      -- {{{
      appendNum (length main) cmds
      -- }}}
    OpCommand OP_DROP                : cmds ->
      -- {{{
      case main of
        _ : restOfMain ->
          updateStack
            (Script cmds)
            z
            (Stack restOfMain alt)
        _ ->
          fewElemsFail "OP_DROP"
      -- }}}
    OpCommand OP_DUP                 : cmds ->
      -- {{{
      case main of
        head1 : _ ->
          appendMain head1 cmds
        _ ->
          fewElemsFail "OP_DUP"
      -- }}}
    OpCommand OP_NIP                 : cmds ->
      -- {{{
      case main of
        head2 : _ : restOfMain ->
          updateStack
            (Script cmds)
            z
            (Stack (head2 : restOfMain) alt)
        _ ->
          fewElemsFail "OP_NIP"
      -- }}}
    OpCommand OP_OVER                : cmds ->
      -- {{{
      case main of
        _ : head1 : _ ->
          appendMain head1 cmds
        _ ->
          fewElemsFail "OP_OVER"
      -- }}}
    OpCommand OP_PICK                : cmds ->
      -- {{{
      case main of
        head1 : restOfMain ->
          let
            n = bsToSignedIntegralLE head1
          in
          if length restOfMain < n + 1 then
            Left "OP_PICK failed - bad index."
          else
            appendMain (restOfMain !! n) cmds
        _ ->
          fewElemsFail "OP_PICK"
      -- }}}
    OpCommand OP_ROLL                : cmds ->
      -- {{{
      case main of
        head1 : restOfMain ->
          let
            n = bsToSignedIntegralLE head1
          in
          if n == 0 then
            noOp cmds
          else if length restOfMain < n + 1 || n < 0 then
            Left "OP_ROLL failed - bad index."
          else
            let
              -- length firstN == n
              (firstN, restOfMain') = splitAt n restOfMain
            in
            updateStack
              (Script cmds)
              z
              (Stack (take (n - 1) firstN ++ restOfMain') alt)
        _ ->
          fewElemsFail "OP_ROLL"
      -- }}}
    OpCommand OP_ROT                 : cmds ->
      -- {{{
      case main of
        head3 : head2 : head1 : restOfMain ->
          updateStack
            (Script cmds)
            z
            (Stack (head1 : head3 : head2 : restOfMain) alt)
        _ ->
          fewElemsFail "OP_ROT"
      -- }}}
    OpCommand OP_SWAP                : cmds ->
      -- {{{
      case main of
        head2 : head1 : restOfMain ->
          updateStack
            (Script cmds)
            z
            (Stack (head1 : head2 : restOfMain) alt)
        _ ->
          fewElemsFail "OP_SWAP"
      -- }}}
    OpCommand OP_TUCK                : cmds ->
      -- {{{
      case main of
        head2 : head1 : restOfMain ->
          updateStack
            (Script cmds)
            z
            (Stack (head2 : head1 : head2 : restOfMain) alt)
        _ ->
          fewElemsFail "OP_TUCK"
      -- }}}
    OpCommand OP_SIZE                : cmds ->
      -- {{{
      case main of
        head1 : _ ->
          appendNum (LBS.length head1) cmds
        _ ->
          fewElemsFail "OP_SIZE"
      -- }}}
    OpCommand OP_EQUAL               : cmds ->
      -- {{{
      opOnHead2
        ( \h2 h1 ->
            if h2 == h1 then
              LBS.singleton 1
            else
              LBS.singleton 0
        )
        cmds
      -- }}}
    OpCommand OP_EQUALVERIFY         : cmds ->
      -- {{{
      performAndVerify OP_EQUAL cmds
      -- }}}
    OpCommand OP_1ADD                : cmds ->
      -- {{{
      opOnNum (+1) cmds
      -- }}}
    OpCommand OP_1SUB                : cmds ->
      -- {{{
      opOnNum (\x -> x - 1) cmds
      -- }}}
    OpCommand OP_NEGATE              : cmds ->
      -- {{{
      opOnNum negate cmds
      -- }}}
    OpCommand OP_ABS                 : cmds ->
      -- {{{
      opOnNum abs cmds
      -- }}}
    OpCommand OP_NOT                 : cmds ->
      -- {{{
      opOnHead
        ( \x ->
            if bsIsFalse x then
              -- Similar to OP_IFDUP, the predicate check's
              -- if x is 0, but the book's code checks for
              -- "falsiness" instead.
              LBS.singleton 1
            else
              LBS.singleton 0
        )
        cmds
      -- }}}
    OpCommand OP_0NOTEQUAL           : cmds ->
      -- {{{
      opOnHead
        ( \x ->
            if bsIsFalse x then
              -- Similar to OP_IFDUP and OP_NOT.
              LBS.singleton 0
            else
              LBS.singleton 1
        )
        cmds
      -- }}}
    OpCommand OP_ADD                 : cmds ->
      -- {{{
      opOnTwoNums (+) cmds
      -- }}}
    OpCommand OP_SUB                 : cmds ->
      -- {{{
      -- topmost reduced from the one below it.
      opOnTwoNums (-) cmds
      -- }}}
    OpCommand OP_MUL                 : cmds ->
      -- {{{
      opOnTwoNums (*) cmds
      -- }}}
    OpCommand OP_BOOLAND             : cmds ->
      -- {{{
      opOnHead2
        ( \h2 h1 ->
            if bsIsFalse h2 || bsIsFalse h2 then
              LBS.singleton 0
            else
              LBS.singleton 1
        )
        cmds
      -- }}}
    OpCommand OP_BOOLOR              : cmds ->
      -- {{{
      opOnHead2
        ( \h2 h1 ->
            if bsIsFalse h2 && bsIsFalse h2 then
              LBS.singleton 0
            else
              LBS.singleton 1
        )
        cmds
      -- }}}
    OpCommand OP_NUMEQUAL            : cmds ->
      -- {{{
      compTwoNums (==) cmds
      -- }}}
    OpCommand OP_NUMEQUALVERIFY      : cmds ->
      -- {{{
      performAndVerify OP_NUMEQUAL cmds
      -- }}}
    OpCommand OP_NUMNOTEQUAL         : cmds ->
      -- {{{
      compTwoNums (/=) cmds
      -- }}}
    OpCommand OP_LESSTHAN            : cmds ->
      -- {{{
      compTwoNums (<) cmds
      -- }}}
    OpCommand OP_GREATERTHAN         : cmds ->
      -- {{{
      compTwoNums (>) cmds
      -- }}}
    OpCommand OP_LESSTHANOREQUAL     : cmds ->
      -- {{{
      compTwoNums (<=) cmds
      -- }}}
    OpCommand OP_GREATERTHANOREQUAL  : cmds ->
      -- {{{
      compTwoNums (>=) cmds
      -- }}}
    OpCommand OP_MIN                 : cmds ->
      -- {{{
      opOnTwoNums min cmds
      -- }}}
    OpCommand OP_MAX                 : cmds ->
      -- {{{
      opOnTwoNums max cmds
      -- }}}
    OpCommand OP_WITHIN              : cmds ->
      -- {{{
      case main of
        -- left inclusive
        maxBS : minBS : nBS : restOfMain ->
          let
            maxN = decodeNum maxBS
            minN = decodeNum minBS
            n    = decodeNum nBS
            appendResult oneOrZero =
              updateStack
                (Script cmds)
                z
                (Stack (LBS.singleton oneOrZero : restOfMain) alt)
          in
          if n >= minN && n < maxN then
            appendResult 1
          else
            appendResult 0
        _ ->
          fewElemsFail "OP_WITHIN"
      -- }}}
    OpCommand OP_RIPEMD160           : cmds ->
      -- {{{
      opOnHead ripemd160 cmds
      -- }}}
    OpCommand OP_SHA1                : cmds ->
      -- {{{
      opOnHead sha1 cmds
      -- }}}
    OpCommand OP_SHA256              : cmds ->
      -- {{{
      opOnHead sha256 cmds
      -- }}}
    OpCommand OP_HASH160             : cmds ->
      -- {{{
      opOnHead hash160 cmds
      -- }}}
    OpCommand OP_HASH256             : cmds ->
      -- {{{
      opOnHead hash256 cmds
      -- }}}
    OpCommand OP_CODESEPARATOR       : cmds ->
      Left "op_codeseparator" -- TODO
    OpCommand OP_CHECKSIG            : cmds ->
      -- {{{
      case main of
        secBS : initDER : restOfMain -> do
          -- {{{
          pubKey    <- parseToPubKey secBS
          signature <- signatureFromSIGHASHALLDER initDER
          let appendResult oneOrZero =
                updateStack
                  (Script cmds)
                  z
                  (Stack (oneOrZero : restOfMain) alt)
          if ECC.verify pubKey zMsg signature then
            appendResult $ LBS.singleton 1
          else
            appendResult $ LBS.singleton 0
          -- }}}
        _                                     ->
          -- {{{
          fewElemsFail "OP_CHECKSIG"
          -- }}}
      -- }}}
    OpCommand OP_CHECKSIGVERIFY      : cmds ->
      -- {{{
      performAndVerify OP_CHECKSIG cmds
      -- }}}
    OpCommand OP_CHECKMULTISIG       : cmds ->
      -- {{{
      case main of
        nBS : rest01 ->
          -- {{{
          let
            n = decodeNum nBS
          in
          if length rest01 < n then
            -- {{{
            Left "not enough public keys in stack for OP_CHECKMULTISIG."
            -- }}}
          else
            -- {{{
            let
              (secBSs, rest02) = splitAt n rest01
            in
            case rest02 of
              mBS : rest03 ->
                -- {{{
                let
                  m = decodeNum mBS
                in
                if length rest03 < m then
                  -- {{{
                  Left "not enough signatures in stack for OP_CHECKMULTISIG."
                  -- }}}
                else if m > n then
                  -- {{{
                  Left "too many signatures."
                  -- }}}
                else
                  -- {{{
                  let
                    (initDERs, rest04) = splitAt m rest03
                  in
                  case rest04 of
                    bs : rest05 | bs == LBS.singleton 0x00 -> do
                      -- {{{
                      pubKeys    <- mapM parseToPubKey secBSs
                      signatures <- mapM signatureFromSIGHASHALLDER initDERs
                      let go []           _       = Right ()
                          go _            []      = Left "leftover signature(s)."
                          go (sig : sigs) remPubs =
                            -- {{{
                            let
                              innerGo _            []       = Nothing
                              innerGo pubsToReturn (p : ps) =
                                -- {{{
                                if ECC.verify p zMsg sig then
                                  Just $ pubsToReturn ++ ps
                                else
                                  innerGo (p : pubsToReturn) ps
                                -- }}}
                              mNewPubs = innerGo [] remPubs
                            in
                            case mNewPubs of
                              Just newPubs ->
                                go sigs newPubs
                              Nothing      ->
                                Left "invalid signature."
                            -- }}}
                          appendResult oneOrZero =
                            -- {{{
                            updateStack
                              (Script cmds)
                              z
                              (Stack (oneOrZero : rest05) alt)
                            -- }}}
                      case go signatures pubKeys of
                        Right _ ->
                          appendResult $ LBS.singleton 1
                        Left  _ ->
                          appendResult $ LBS.singleton 0
                      -- }}}
                    _ ->
                      -- {{{
                      Left "OP_CHECKMULTISIG couldn't find a OP_0 after the signatures."
                      -- }}}
                  -- }}}
                -- }}}
              _ ->
                -- {{{
                fewElemsFail "OP_CHECKMULTISIG (no \"m\" found)"
                -- }}}
            -- }}}
          -- }}}
        _ ->
          -- {{{
          fewElemsFail "OP_CHECKMULTISIG"
          -- }}}
      -- }}}
    OpCommand OP_CHECKMULTISIGVERIFY : cmds ->
      -- {{{
      performAndVerify OP_CHECKMULTISIG cmds
      -- }}}
    OpCommand OP_NOP1                : cmds ->
      noOp cmds
    OpCommand OP_CHECKLOCKTIMEVERIFY : cmds ->
      undefined -- TODO
    OpCommand OP_CHECKSEQUENCEVERIFY : cmds ->
      undefined -- TODO
    OpCommand OP_NOP4                : cmds ->
      noOp cmds
    OpCommand OP_NOP5                : cmds ->
      noOp cmds
    OpCommand OP_NOP6                : cmds ->
      noOp cmds
    OpCommand OP_NOP7                : cmds ->
      noOp cmds
    OpCommand OP_NOP8                : cmds ->
      noOp cmds
    OpCommand OP_NOP9                : cmds ->
      noOp cmds
    OpCommand OP_NOP10               : cmds ->
      noOp cmds
  -- }}}


-- UTILS
-- {{{
-- | Function rename of `signedIntegralToBSLE` for easier
--   following of the book.
encodeNum :: Integral a => a -> ByteString
encodeNum = signedIntegralToBSLE


-- | Function rename of `bsToSignedIntegralLE` for easier
--   following of the book.
decodeNum :: Integral a => ByteString -> a
decodeNum = bsToSignedIntegralLE


-- | Falsiness checker of a `ByteString` value.
bsIsFalse :: ByteString -> Bool
bsIsFalse bs =
  decodeNum bs == 0


-- | Toggles the falsiness/truthiness of a `ByteString` value.
--   Returns either @1@ or @0@.
bsBooleanToggle :: ByteString -> ByteString
bsBooleanToggle bs =
  -- {{{
  if bsIsFalse bs then
    LBS.singleton 1
  else
    LBS.singleton 0
  -- }}}


-- | Utility function for parsing an SEC formatted `ByteString` into a `PubKey`.
parseToPubKey :: ByteString -> Either Text ECC.PubKey
parseToPubKey secBS =
  -- {{{
    P.runParser ECC.secParser "" secBS
  & mapLeft (const "failed to parse sec formatted public key.")
  -- }}}


-- | Parses a DER formatted signature with @SIGHASH_ALL@ as its last byte.
signatureFromSIGHASHALLDER :: ByteString -> Either Text ECC.Signature
signatureFromSIGHASHALLDER initDER = do
  -- {{{
  derBS     <- explainMaybe "invalid der." $ LBS.safeInit initDER
  P.runParser parser "" derBS 
    & mapLeft (const "failed to parse der formatted signature.")
  -- }}}
-- }}}


-- SAMPLE VALUES
-- {{{
sampleScript0 :: Script
sampleScript0 = Script
  [ Element $ integerToBS 0x3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf2132060277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01
  , Element $ integerToBS 0x0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a
  ]
sampleScript0BS :: ByteString
sampleScript0BS =
  integerToBS 0x6b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf2132060277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a


sampleScript1 :: Script
sampleScript1 = Script
  [ OpCommand OP_DUP
  , OpCommand OP_HASH160
  , Element $ integerToBS 0xbc3b654dca7e56b04dca18f2566cdaf02e8d9ada
  , OpCommand OP_EQUALVERIFY
  , OpCommand OP_CHECKSIG
  ]
sampleScript1BS :: ByteString
sampleScript1BS =
  integerToBS 0x1976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac


sampleScript2 :: Script
sampleScript2 = Script
  [ OpCommand OP_DUP
  , OpCommand OP_HASH160
  , Element $ integerToBS 0x1c4bc762dd5423e332166702cb75f40df79fea12
  , OpCommand OP_EQUALVERIFY
  , OpCommand OP_CHECKSIG
  ]
sampleScript2BS :: ByteString
sampleScript2BS =
  integerToBS 0x1976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac
-- }}}
