{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- VmExecTest
-}

module VirtualMachine.VmExecTest (vmExecTestList) where

import Test.HUnit
import InternalFunctions (internalEnv)
import Execution (runCode)
import VirtualMachine.Vm (Val(..), EnumOperator(..), Instructions(..), Stack,
   Insts, exec)

testSimpleVmExec :: Test
testSimpleVmExec = TestCase $ do
   let env = []
   let args = []
   let instructions = [Push (VNum 5), Push (VNum 3), Push (VOp SignPlus), Call 2, Ret]
   let stack = []
   let result = exec env args instructions stack
   let expected = Right (VNum 8)
   assertEqual "Simple vm exec" expected result

testAbsCodeInEnv :: Test
testAbsCodeInEnv = TestCase $ do
   let env = [("abs", VFunction [
         PushArg 0,
         Push (VNum 0),
         Push (VOp SignMinus),
         Call 2,
         JumpIfFalse 2,
         PushArg 0,
         Ret,
         PushArg 0,
         Push (VNum (-1)),
         Push (VOp SignMultiply),
         Call 2,
         Ret])]
   let args = []
   let instructions = [Push (VNum (-42)), PushEnv "abs", Call 1, Ret]
   let stack = []
   let result = exec env args instructions stack
   let expected = Right (VNum 42)
   assertEqual "Abs function in env" expected result

testFactorialCodeInEnv :: Test
testFactorialCodeInEnv = TestCase $ do
   let env = [("fact", VFunction [
         Push (VNum 1),
         PushArg 0,
         Push (VOp SignMinus),
         Call 2,
         JumpIfFalse 2,
         Push (VNum 1),
         Ret,
         Push (VNum 1),
         PushArg 0,
         Push (VOp SignMinus),
         Call 2,
         PushEnv "fact",
         Call 1,
         PushArg 0,
         Push (VOp SignMultiply),
         Call 2,
         Ret])]
   let args = []
   let instructions = [Push (VNum 5), PushEnv "fact", Call 1, Ret]
   let stack = []
   let result = exec env args instructions stack
   let expected = Right (VNum 120)
   assertEqual "Factorial function in env" expected result

testInvalidPushArgIndex :: Test
testInvalidPushArgIndex = TestCase $ do
   let env = []
   let args = [VNum 10, VNum 20]
   let instructions = [PushArg 2]  -- Attempt to access an invalid index
   let stack = []
   let result = exec env args instructions stack
   let expected = Left "PushArg: invalid index"
   assertEqual "PushArg with invalid index" expected result

testMissingValueOnStackReturnInstruction :: Test
testMissingValueOnStackReturnInstruction = TestCase $ do
   let env = []
   let args = []
   let instructions = [Ret]  -- Missing the Ret instruction
   let stack = []
   let result = exec env args instructions stack
   let expected = Left "Ret: missing value on stack"
   assertEqual "Missing value for return instruction" expected result

testMissingReturnInstruction :: Test
testMissingReturnInstruction = TestCase $ do
   let env = []
   let args = []
   let instructions = [Push (VNum 5)]  -- Missing the Ret instruction
   let stack = []
   let result = exec env args instructions stack
   let expected = Left "Missing return instruction"
   assertEqual "Missing return instruction" expected result

testDivisionByZero :: Test
testDivisionByZero = TestCase $ do
   let env = []
   let args = []
   let instructions = [Push (VNum 10), Push (VNum 0), Push (VOp SignDivide), Call 2, Ret]
   let stack = []
   let result = exec env args instructions stack
   let expected = Left "Div: division by zero"
   assertEqual "Division by zero error" expected result

testInvalidOperationArguments :: Test
testInvalidOperationArguments = TestCase $ do
   let env = []
   let args = []
   let instructions = [Push (VBool True), Push (VNum 3), Push (VOp SignPlus), Call 2, Ret]  -- Trying to add a boolean and a number
   let stack = []
   let result = exec env args instructions stack
   let expected = Left "Add: invalid arguments"
   assertEqual "Invalid operation arguments" expected result

testMissingValueOnStack :: Test
testMissingValueOnStack = TestCase $ do
   let env = []
   let args = []
   let instructions = [Call 1]  -- Missing a value on the stack
   let stack = []
   let result = exec env args instructions stack
   let expected = Left "Call: missing value on stack"
   assertEqual "Missing value on stack" expected result

testMissingOrInvalidValueOnStack :: Test
testMissingOrInvalidValueOnStack = TestCase $ do
   let env = []
   let args = []
   let instructions = [JumpIfFalse 2]  -- Missing or invalid value on the stack
   let stack = []
   let result = exec env args instructions stack
   let expected = Left "JumpIfFalse: missing or invalid value on stack"
   assertEqual "Missing or invalid value on stack" expected result

vmExecTestList :: Test
vmExecTestList = TestList [
   testSimpleVmExec,
   testAbsCodeInEnv,
   testFactorialCodeInEnv,
   testInvalidPushArgIndex,
   testMissingValueOnStackReturnInstruction,
   testMissingReturnInstruction,
   testDivisionByZero,
   testInvalidOperationArguments,
   testMissingValueOnStack,
   testMissingOrInvalidValueOnStack
   ]
