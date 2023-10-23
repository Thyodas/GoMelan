{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- VmBytecodeTest
-}

module VirtualMachine.VmBytecodeTest (vmBytecodeTestList) where

import Test.HUnit
import InternalFunctions (internalEnv)
import Ast (Ast(..), envInsert)
import Execution (runCode)
import VirtualMachine.Vm (Val(..), Operations(..), Instructions(..), Stack,
   Insts, exec)
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
      testBinaryEncodingDecoding [Push (VStr "Hello, World!")],
      testBinaryEncodingDecoding [Push (VList [VNum 1, VNum 2, VNum 3])],
      testBinaryEncodingDecoding [Push (VOp Add)],
      testBinaryEncodingDecoding [Push (VFunction [Push (VNum 5), Push (VNum 3),
         Push (VOp Add), Call, Ret])],
      testBinaryEncodingDecoding [Push VNil]
   ]

testOperationsBinary :: Test
testOperationsBinary = TestList [
      testBinaryEncodingDecoding [Push (VOp Add)],
      testBinaryEncodingDecoding [Push (VOp Sub)],
      testBinaryEncodingDecoding [Push (VOp Mul)],
      testBinaryEncodingDecoding [Push (VOp Div)],
      testBinaryEncodingDecoding [Push (VOp Eq)],
      testBinaryEncodingDecoding [Push (VOp Less)]
   ]

testInstructionsBinary :: Test
testInstructionsBinary = TestList [
      testBinaryEncodingDecoding [Push (VNum 5), Push (VNum 3), Push (VOp Add),
         Call, Ret],
      testBinaryEncodingDecoding [PushArg 0],
      testBinaryEncodingDecoding [PushArg 1],
      testBinaryEncodingDecoding [PushEnv "myKey"],
      testBinaryEncodingDecoding [JumpIfFalse 2],
      testBinaryEncodingDecoding [Call],
      testBinaryEncodingDecoding [Ret]
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
      "Invalid tag while deserializing Val"
   ]

testOperationsBinaryFailure :: Test
testOperationsBinaryFailure = TestList [
   testBinaryDecodingFailure (BS.pack [0, 1, 2, 4])
      "Invalid tag while deserializing Operations"
   ]

testInstructionsBinaryFailure :: Test
testInstructionsBinaryFailure = TestList [
   testBinaryDecodingFailure (BS.pack [0, 1, 2, 4])
      "Invalid tag while deserializing Instructions"
   ]

vmBytecodeTestList :: Test
vmBytecodeTestList = TestList [
    testValBinary,
    testOperationsBinary,
    testInstructionsBinary,
    testValBinaryFailure,
    testOperationsBinaryFailure,
    testInstructionsBinaryFailure
    ]
