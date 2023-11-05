{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- VmBytecodeTest
-}

module VirtualMachine.VmBytecodeTest (vmBytecodeTestList) where

import Test.HUnit
import Execution()
import VirtualMachine.Vm (Val(..), EnumOperator(..), Instructions(..),
    execCall, execOperation, execHelper, Compiled(..), Insts)
import Data.Binary (encode, decode, decodeOrFail)
import Data.Binary.Get (ByteOffset)
import qualified Data.ByteString.Lazy as BS

-- Helper function to test encoding and decoding of instructions
testBinaryEncodingDecoding :: Insts -> Test
testBinaryEncodingDecoding instructions = TestCase $ do
    let encodedInstructions = encode instructions
    let decodedInstructions = decode encodedInstructions
    assertEqual "Binary Encoding/Decoding" instructions decodedInstructions

testValBinary :: Test
testValBinary = TestList [
      testBinaryEncodingDecoding [Push (VNum 42)],
      testBinaryEncodingDecoding [Push (VBool True)],
      testBinaryEncodingDecoding [Push (VChar 'O'), Push (VChar 'K'), BuildList 2],
      testBinaryEncodingDecoding [Push (VList [VNum 1, VNum 2, VNum 3])],
      testBinaryEncodingDecoding [Push (VOp SignPlus)],
      testBinaryEncodingDecoding [Push (VFunction [Push (VNum 5), Push (VNum 3),
         Push (VOp SignPlus), Call 2, Ret])],
      testBinaryEncodingDecoding [Push VNil]
   ]

testOperationsBinary :: Test
testOperationsBinary = TestList [
      testBinaryEncodingDecoding [Push (VOp SignPlus)],
      testBinaryEncodingDecoding [Push (VOp SignMinus)],
      testBinaryEncodingDecoding [Push (VOp SignDivide)],
      testBinaryEncodingDecoding [Push (VOp SignMultiply)],
      testBinaryEncodingDecoding [Push (VOp SignModulo)],
      testBinaryEncodingDecoding [Push (VOp SignEqual)],
      testBinaryEncodingDecoding [Push (VOp SignNotEqual)],
      testBinaryEncodingDecoding [Push (VOp SignNot)],
      testBinaryEncodingDecoding [Push (VOp SignAnd)],
      testBinaryEncodingDecoding [Push (VOp SignInfEqual)],
      testBinaryEncodingDecoding [Push (VOp SignSupEqual)],
      testBinaryEncodingDecoding [Push (VOp SignInf)],
      testBinaryEncodingDecoding [Push (VOp SignSup)]
   ]

testInstructionsBinary :: Test
testInstructionsBinary = TestList [
      testBinaryEncodingDecoding [Push (VNum 5), Push (VNum 3), Push (VOp SignPlus),
         Call 0, Ret],
      testBinaryEncodingDecoding [PushArg 0],
      testBinaryEncodingDecoding [PushArg 1],
      testBinaryEncodingDecoding [PushEnv "myKey"],
      testBinaryEncodingDecoding [JumpIfFalse 2],
      testBinaryEncodingDecoding [Call 2],
      testBinaryEncodingDecoding [Ret],
      testBinaryEncodingDecoding [Jump 2]
   ]

-- Helper function to test decoding failures
testBinaryDecodingFailure :: BS.ByteString -> String -> Test
testBinaryDecodingFailure encodedData expectedError = TestCase $ do
    let result = decodeOrFail encodedData :: Either (BS.ByteString, Data.Binary.Get.ByteOffset, String) (BS.ByteString, ByteOffset, Insts)
    case result of
        Left (_, _, actualError) -> assertEqual "Binary Decoding Failure" expectedError actualError
        Right _ -> assertFailure "Decoding did not fail as expected"

testValBinaryFailure :: Test
testValBinaryFailure = TestList [
   testBinaryDecodingFailure (BS.pack [0, 0, 0, 1])
      "not enough bytes"
   ]

testOperationsBinaryFailure :: Test
testOperationsBinaryFailure = TestList [
   testBinaryDecodingFailure (BS.pack [0, 1, 2, 4])
      "not enough bytes"
   ]

testInstructionsBinaryFailure :: Test
testInstructionsBinaryFailure = TestList [
   testBinaryDecodingFailure (BS.pack [0, 1, 2, 4])
      "not enough bytes"
   ]

testExecCallWithFunction :: Test
testExecCallWithFunction = TestCase $ do
   let env = []
   let args = [VNum 5]
   let instructions = [PushArg 0, Push (VNum 2),
                        Push (VOp SignMultiply), Call 2, Ret]
   let _ = []
   let result = execCall env args (VFunction instructions)
   let expected = Right (VNum 10)
   assertEqual "ExecCall with function" expected result

testExecCallWithOp :: Test
testExecCallWithOp = TestCase $ do
   let env = []
   let args = [VNum 5, VNum 3]
   let result = execCall env args (VOp SignPlus)
   let expected = Right (VNum 8)
   assertEqual "ExecCall with operation" expected result

testExecCallWithInvalidArgs :: Test
testExecCallWithInvalidArgs = TestCase $ do
   let env = []
   let args = []
   let result = execCall env args (VNum 5)
   let expected = Left "Call: invalid arguments"
   assertEqual "ExecCall with invalid arguments" expected result

testExecOperation :: Test
testExecOperation = TestList [
   TestCase $ assertEqual "Addition with valid arguments" (VNum 8) (execOperation SignPlus [VNum 5, VNum 3]),
   TestCase $ assertEqual "Addition with invalid arguments" (VNil) (execOperation SignPlus [VNum 5]),
   TestCase $ assertEqual "Addition with valid arguments" (VFloatNum 8.0) (execOperation SignPlus [VFloatNum 5.0, VFloatNum 3.0]),
   TestCase $ assertEqual "Addition with invalid arguments" (VNil) (execOperation SignPlus [VFloatNum 5.0]),
   TestCase $ assertEqual "Addition with list" (VList [VNum 1, VNum 2, VNum 3, VNum 4]) (execOperation SignPlus [VList [VNum 1, VNum 2], VList [VNum 3, VNum 4]]),

   TestCase $ assertEqual "Subtraction with valid arguments" (VNum 2) (execOperation SignMinus [VNum 5, VNum 3]),
   TestCase $ assertEqual "Subtraction with invalid arguments" (VNil) (execOperation SignMinus [VNum 5]),
   TestCase $ assertEqual "Subtraction with valid arguments" (VFloatNum 2.0) (execOperation SignMinus [VFloatNum 5.0, VFloatNum 3.0]),
   TestCase $ assertEqual "Subtraction with invalid arguments" (VNil) (execOperation SignMinus [VFloatNum 5.0]),

   TestCase $ assertEqual "Multiplication with valid arguments" (VNum 15) (execOperation SignMultiply [VNum 5, VNum 3]),
   TestCase $ assertEqual "Multiplication with invalid arguments" (VNil) (execOperation SignMultiply [VNum 5]),
   TestCase $ assertEqual "Multiplication with valid arguments" (VBool True) (execOperation SignMultiply [VBool True, VBool True]),
   TestCase $ assertEqual "Multiplication with valid arguments" (VFloatNum 15.0) (execOperation SignMultiply [VFloatNum 5.0, VFloatNum 3.0]),
   TestCase $ assertEqual "Multiplication with invalid arguments" (VNil) (execOperation SignMultiply [VFloatNum 5.0]),
   TestCase $ assertEqual "Multiplication with invalid arguments" (VNil) (execOperation SignMultiply [VChar 'a', VChar 'b']),

   TestCase $ assertEqual "Division with valid arguments" (VNum 2) (execOperation SignDivide [VNum 6, VNum 3]),
   TestCase $ assertEqual "Division with invalid arguments" (VNil) (execOperation SignDivide [VNum 5]),
   TestCase $ assertEqual "Division with valid arguments" (VFloatNum 2.0) (execOperation SignDivide [VFloatNum 6.0, VFloatNum 3.0]),
   TestCase $ assertEqual "Division with invalid arguments" (VNil) (execOperation SignDivide [VFloatNum 5.0]),

   TestCase $ assertEqual "Equality with valid arguments" (VBool True) (execOperation SignEqual [VNum 5, VNum 5]),
   TestCase $ assertEqual "Equality with invalid number of arguments" (VNil) (execOperation SignEqual [VNum 5]),
   TestCase $ assertEqual "Equality with valid arguments" (VBool True) (execOperation SignEqual [VFloatNum 5.0, VFloatNum 5.0]),
   TestCase $ assertEqual "Equality with invalid number of arguments" (VNil) (execOperation SignEqual [VFloatNum 5.0]),

   TestCase $ assertEqual "Less than with valid arguments" (VBool True) (execOperation SignInf [VNum 3, VNum 5]),
   TestCase $ assertEqual "Less than with invalid arguments" (VNil) (execOperation SignInf [VNum 5]),
   TestCase $ assertEqual "Less than with valid arguments" (VBool True) (execOperation SignInf [VFloatNum 3.0, VFloatNum 5.0]),
   TestCase $ assertEqual "Less than with invalid arguments" (VNil) (execOperation SignInf [VFloatNum 5.0]),

   TestCase $ assertEqual "Greater than with valid arguments" (VBool True) (execOperation SignSup [VNum 5, VNum 3]),
   TestCase $ assertEqual "Greater than with invalid arguments" (VNil) (execOperation SignSup [VNum 5]),
   TestCase $ assertEqual "Greater than with valid arguments" (VBool True) (execOperation SignSup [VFloatNum 5.0, VFloatNum 3.0]),
   TestCase $ assertEqual "Greater than with invalid arguments" (VNil) (execOperation SignSup [VFloatNum 5.0]),

   TestCase $ assertEqual "Less than or equal with valid arguments" (VBool True) (execOperation SignInfEqual [VNum 3, VNum 5]),
   TestCase $ assertEqual "Less than or equal with invalid arguments" (VNil) (execOperation SignInfEqual [VNum 5]),
   TestCase $ assertEqual "Less than or equal with valid arguments" (VBool True) (execOperation SignInfEqual [VFloatNum 3.0, VFloatNum 5.0]),
   TestCase $ assertEqual "Less than or equal with invalid arguments" (VNil) (execOperation SignInfEqual [VFloatNum 5.0]),

   TestCase $ assertEqual "Greater than or equal with valid arguments" (VBool True) (execOperation SignSupEqual [VNum 5, VNum 5]),
   TestCase $ assertEqual "Greater than or equal with invalid arguments" (VNil) (execOperation SignSupEqual [VNum 5]),
   TestCase $ assertEqual "Greater than or equal with valid arguments" (VBool True) (execOperation SignSupEqual [VFloatNum 5.0, VFloatNum 5.0]),
   TestCase $ assertEqual "Greater than or equal with invalid arguments" (VNil) (execOperation SignSupEqual [VFloatNum 5.0]),

   TestCase $ assertEqual "And with valid arguments" (VBool False) (execOperation SignAnd [VBool True, VBool False]),
   TestCase $ assertEqual "Or with valid arguments" (VBool True) (execOperation SignOr [VBool True, VBool True]),
   TestCase $ assertEqual "Or with valid arguments" (VBool False) (execOperation SignOr [VBool False, VBool False]),
   TestCase $ assertEqual "And with invalid arguments" (VNil) (execOperation SignAnd [VBool True]),

   TestCase $ assertEqual "Or with valid arguments" (VBool True) (execOperation SignOr [VBool True, VBool False]),
   TestCase $ assertEqual "Or with valid arguments" (VBool True) (execOperation SignOr [VBool True, VBool True]),
   TestCase $ assertEqual "Or with valid arguments" (VBool False) (execOperation SignOr [VBool False, VBool False]),
   TestCase $ assertEqual "Or with invalid arguments" (VNil) (execOperation SignOr [VBool True]),

   TestCase $ assertEqual "Not with valid arguments" (VBool False) (execOperation SignNot [VBool True]),
   TestCase $ assertEqual "Not with invalid arguments" (VNil) (execOperation SignNot [VNum 5]),

   TestCase $ assertEqual "Modulo with valid arguments" (VNum 1) (execOperation SignModulo [VNum 5, VNum 2]),
   TestCase $ assertEqual "Modulo with division by zero" (VNil) (execOperation SignModulo [VNum 5, VNum 0]),
   TestCase $ assertEqual "Modulo with invalid arguments" (VNil) (execOperation SignModulo [VNum 5]),
   TestCase $ assertEqual "Modulo with valid arguments" (VFloatNum 1.0) (execOperation SignModulo [VFloatNum 5.0, VFloatNum 2.0]),
   TestCase $ assertEqual "Modulo with division by zero" (VNil) (execOperation SignModulo [VFloatNum 5.0, VFloatNum 0.0]),
   TestCase $ assertEqual "Modulo with invalid arguments" (VNil) (execOperation SignModulo [VFloatNum 5.0]),

   TestCase $ assertEqual "Inequality with valid arguments" (VBool True) (execOperation SignNotEqual [VNum 5, VNum 3]),
   TestCase $ assertEqual "Inequality with invalid number of arguments" (VNil) (execOperation SignNotEqual [VNum 5]),
   TestCase $ assertEqual "Inequality with valid arguments" (VBool True) (execOperation SignNotEqual [VFloatNum 5.0, VFloatNum 3.0]),
   TestCase $ assertEqual "Inequality with invalid number of arguments" (VNil) (execOperation SignNotEqual [VFloatNum 5.0])
   ]

testExecHelper :: Test
testExecHelper = TestList [
   TestCase $ assertEqual "ExecHelper with PushEnv" (Right (VNum 5)) (execHelper [("x", VNum 5)] [] [PushEnv "x", Ret] [PushEnv "x", Ret] []),
   TestCase $ assertEqual "ExecHelper with Push" (Right (VNum 5)) (execHelper [] [] [Push (VNum 5), Ret] [Push (VNum 5), Ret] []),
   TestCase $ assertEqual "ExecHelper with Call" (Left "PushArg: invalid index") (execHelper [("double", VFunction [PushArg 0, Push (VNum 2), Push (VOp SignMultiply), Ret])] [VNum 5] [PushEnv "double", Call 0, Ret] [PushEnv "double", Call 0, Ret] []),
   TestCase $ assertEqual "ExecHelper with JumpIfFalse" (Left "Missing return instruction") (execHelper [] [] [Push (VBool False), JumpIfFalse 2, Push (VNum 5), Ret] [Push (VBool False), JumpIfFalse 2, Push (VNum 5), Ret] []),
   TestCase $ assertEqual "ExecHelper with Jump" (Left "Missing return instruction") (execHelper [] [] [Push (VNum 5), Jump 2, Push (VNum 10), Ret] [Push (VNum 5), Jump 2, Push (VNum 10), Ret] []),
   TestCase $ assertEqual "ExecHelper with PushArg" (Right (VNum 5)) (execHelper [] [VNum 5] [PushArg 0, Ret] [PushArg 0, Ret] [])
   ]

testAbsolute :: Test
testAbsolute = TestList [
    TestCase $ assertEqual "Absolute of 5" (VNum 5) (abs (VNum 5)),
    TestCase $ assertEqual "Absolute of -5" (VNum 5) (abs (VNum (-5))),
    TestCase $ assertEqual "Absolute of 5.0" (VFloatNum 5.0) (abs (VFloatNum 5.0)),
    TestCase $ assertEqual "Absolute of -5.0" (VFloatNum 5.0) (abs (VFloatNum (-5.0))),
    TestCase $ assertEqual "Absolute of invalid argument" (VNil) (abs (VChar 'a'))
    ]

testSignum :: Test
testSignum = TestList [
    TestCase $ assertEqual "Signum of 5" (VNum 1) (signum (VNum 5)),
    TestCase $ assertEqual "Signum of -5" (VNum (-1)) (signum (VNum (-5))),
    TestCase $ assertEqual "Signum of 0" (VNum 0) (signum (VNum 0)),
    TestCase $ assertEqual "Signum of 5" (VFloatNum 1) (signum (VFloatNum 5.0)),
    TestCase $ assertEqual "Signum of -5" (VFloatNum (-1)) (signum (VFloatNum (-5.0))),
    TestCase $ assertEqual "Signum of 0" (VFloatNum 0) (signum (VFloatNum 0.0)),
    TestCase $ assertEqual "Signum of invalid argument" (VNil) (signum (VChar 'z'))
    ]

testCompare :: Test
testCompare = test
  [ "Test VNum equality" ~: compare (VNum 42) (VNum 42) @?= EQ
  , "Test VNum less than VFloatNum" ~: compare (VNum 42) (VFloatNum 3.14) @?= EQ
  , "Test VFloatNum greater than VNum" ~: compare (VFloatNum 3.14) (VNum 42) @?= EQ
  , "Test VFloatNum equality" ~: compare (VFloatNum 3.14) (VFloatNum 3.14) @?= EQ
  , "Test VSomethingElse" ~: compare (VChar 'a') (VNum 42) @?= EQ
  ]

testRational :: Test
testRational = test
  [ "Test VNum" ~: toRational (VNum 42) @?= 42
  , "Test VFloatNum" ~: toRational (VFloatNum 1.25) @?= 1.25
  , "Test default case" ~: toRational (VChar 'a') @?= 0
  ]

testShowInstrucion :: Test
testShowInstrucion = TestList [
    TestCase $ assertEqual "show instance JumpIfFalse" "If false 5" (show (JumpIfFalse 5)),
    TestCase $ assertEqual "show instance Jump" "Jump 3 instructions" (show (Jump 3)),
    TestCase $ assertEqual "show invalide Jump" "Jump back 3 instructions" (show (Jump (-3))),
    TestCase $ assertEqual "show instance PushArg" "Push to stack, arg 5" (show (PushArg 5)),
    TestCase $ assertEqual "show instance PushEnv" "Push to stack, env key \"my_key\"" (show (PushEnv "my_key")),
    TestCase $ assertEqual "show instance AddEnv" "Add to env \"my_key\"" (show (AddEnv ("my_key"))),
    TestCase $ assertEqual "show instance Call" "Call with 5 args\n" (show (Call 5)),
    TestCase $ assertEqual "show instance BuildList" "Build list with 5 args" (show (BuildList 5)),
    TestCase $ assertEqual "show instance AccessList" "Access list" (show AccessList),
    TestCase $ assertEqual "show instance Ret" "Return" (show Ret)
  ]

testPushEnvMissingValue :: Test
testPushEnvMissingValue = TestCase $ do
    let env = []
        args = [VNum 42, VBool True]
        insts = [PushEnv "variable", Call 1, Ret]
        result = execHelper env args insts insts []
    assertEqual "PushEnv: missing value in env" (Left "PushEnv: missing value in env for key 'variable'.") result

testJumpIfFalse :: Test
testJumpIfFalse = TestList
    [ TestCase $ assertEqual "JumpIfFalse - true condition" (Right (VNum 42)) $
        execHelper [] [] instructions [JumpIfFalse 2, Ret] [VBool True, VNum 42]
    , TestCase $ assertEqual "JumpIfFalse - negative shift" (Left "Missing return instruction") $
        execHelper [] [] instructions [JumpIfFalse (-1), Ret] [VBool False, VNum 42]
    ]
  where
    instructions = [Push (VNum 1), Push (VNum 2)]

testJumpIfFalseNumeric :: Test
testJumpIfFalseNumeric = TestList
    [ TestCase $ assertEqual "JumpIfFalse - negative shift" (Left "Missing return instruction") $
        execHelper [] [] instructions [JumpIfFalse (-1), Ret] [VNum 1, VNum 42]
    , TestCase $ assertEqual "JumpIfFalse - positive shift" (Left "Missing return instruction") $
        execHelper [] [] instructions [JumpIfFalse 2, Ret] [VNum 1, VNum 42]
    ]
  where
    instructions = [Push (VNum 1), Push (VNum 2)]

testJump :: Test
testJump = TestList
    [ TestCase $ assertEqual "Jump - negative shift" (Left "Missing return instruction") $
        execHelper [] [] instructions [Jump (-1), Ret] [VNum 0, VNum 42]
    , TestCase $ assertEqual "Jump - positive shift" (Left "Missing return instruction") $
        execHelper [] [] instructions [Jump 2, Ret] [VNum 0, VNum 42]
    -- Ajoutez d'autres tests pour couvrir davantage de cas
    ]
  where
    instructions = [Push (VNum 1), Push (VNum 2)]

testValShow :: Test
testValShow = TestList [
   TestCase $ assertEqual "Show VNum" "5" (show (VNum 5)),
   TestCase $ assertEqual "Show VBool" "true" (show (VBool True)),
   TestCase $ assertEqual "Show VChar" "'e'" (show (VChar 'e')),
   TestCase $ assertEqual "Show VFunction" "" (show (VFunction []))
   ]

testShowVNum :: Test
testShowVNum = TestCase $ do
    let v = VNum 42
    assertEqual "Show VNum" "42" (show v)

testShowVBool :: Test
testShowVBool = TestCase $ do
    let v = VBool True
    assertEqual "Show VBool True" "true" (show v)

testShowVStr :: Test
testShowVStr = TestCase $ do
    let v = VChar 'o'
    assertEqual "Show VChar o" "'o'" (show v)

testShowVList :: Test
testShowVList = TestCase $ do
    let v = VList [VNum 1, VChar 'o', VBool True]
    assertEqual "Show VList" "[1,'o',true]" (show v)

testShowVOp :: Test
testShowVOp = TestCase $ do
    let v = VOp SignPlus
    assertEqual "Show VOp SignPlus" "<Operator '+'>" (show v)

testShowVFunction :: Test
testShowVFunction = TestCase $ do
    let v = VFunction [Push (VNum 1), Push (VNum 2)]
    assertEqual "Show VFunction" "Push 1\nPush 2\n" (show v)

testShowVNil :: Test
testShowVNil = TestCase $ do
    let v = VNil
    assertEqual "Show VNil" "null" (show v)

testShowEnumOperator :: Test
testShowEnumOperator = TestList [
   TestCase $ assertEqual "Show SignPlus" "+" (show SignPlus),
   TestCase $ assertEqual "Show SignMinus" "-" (show SignMinus),
   TestCase $ assertEqual "Show SignMultiply" "*" (show SignMultiply),
   TestCase $ assertEqual "Show SignDivide" "/" (show SignDivide),
   TestCase $ assertEqual "Show SignModulo" "%" (show SignModulo),
   TestCase $ assertEqual "Show SignEqual" "==" (show SignEqual),
   TestCase $ assertEqual "Show SignNotEqual" "!=" (show SignNotEqual),
   TestCase $ assertEqual "Show SignNot" "!" (show SignNot),
   TestCase $ assertEqual "Show SignAnd" "&&" (show SignAnd),
   TestCase $ assertEqual "Show SignOr" "||" (show SignOr),
   TestCase $ assertEqual "Show SignInfEqual" "<=" (show SignInfEqual),
   TestCase $ assertEqual "Show SignSupEqual" ">=" (show SignSupEqual),
   TestCase $ assertEqual "Show SignInf" "<" (show SignInf),
   TestCase $ assertEqual "Show SignSup" ">" (show SignSup)
   ]

testShowCompiled :: Test
testShowCompiled = TestCase $ do
    let env = [("x", VNum 5), ("y", VBool True)]
    let instructions = [Push (VNum 1), Push (VNum 2), Push (VOp SignPlus)]
    let compiled = Compiled env instructions
    let expectedShow =  "x:\n\t5\n\ny:\n\ttrue\n\n\nPush 1\nPush 2\nPush <Operator '+'>\n\n"
    assertEqual "Show Compiled" expectedShow (show compiled)

testEnumOperatorEq :: Test
testEnumOperatorEq = TestList [
   TestCase $ assertEqual "SignPlus == SignPlus" SignPlus SignPlus,
   TestCase $ assertEqual "SignMinus == SignMinus" SignMinus SignMinus,
   TestCase $ assertEqual "SignMultiply == SignMultiply" SignMultiply SignMultiply,
   TestCase $ assertEqual "SignDivide == SignDivide" SignDivide SignDivide,
   TestCase $ assertEqual "SignModulo == SignModulo" SignModulo SignModulo,
   TestCase $ assertEqual "SignEqual == SignEqual" SignEqual SignEqual,
   TestCase $ assertEqual "SignNotEqual == SignNotEqual" SignNotEqual SignNotEqual,
   TestCase $ assertEqual "SignNot == SignNot" SignNot SignNot,
   TestCase $ assertEqual "SignAnd == SignAnd" SignAnd SignAnd,
   TestCase $ assertEqual "SignInfEqual == SignInfEqual" SignInfEqual SignInfEqual,
   TestCase $ assertEqual "SignSupEqual == SignSupEqual" SignSupEqual SignSupEqual,
   TestCase $ assertEqual "SignInf == SignInf" SignInf SignInf,
   TestCase $ assertEqual "SignSup == SignSup" SignSup SignSup
   ]

vmBytecodeTestList :: Test
vmBytecodeTestList = TestList [
    testEnumOperatorEq,
    testShowCompiled,
    testShowEnumOperator,
    testShowVNum,
    testShowVBool,
    testShowVStr,
    testShowVList,
    testShowVOp,
    testShowVFunction,
    testShowVNil,
    testValShow,
    testJump,
    testJumpIfFalseNumeric,
    testJumpIfFalse,
    testPushEnvMissingValue,
    testExecHelper,
    testExecOperation,
    testExecCallWithInvalidArgs,
    testExecCallWithOp,
    testExecCallWithFunction,
    testValBinary,
    testOperationsBinary,
    testInstructionsBinary,
    testValBinaryFailure,
    testOperationsBinaryFailure,
    testInstructionsBinaryFailure,
    testAbsolute,
    testSignum,
    testCompare,
    testRational,
    testShowInstrucion
    ]
