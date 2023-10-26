{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- ExecutionTest
-}

module ExecutionTest (executionTestList) where

import Test.HUnit
import InternalFunctions (internalEnv)
import Ast (Ast(..), envInsert)
import Execution (runCode)

testRecursiveFunction :: Test
testRecursiveFunction = TestCase $ assertEqual "Recursive function" expected result
    where
         result = runCode env code
         env = internalEnv
         code = unlines [
               "(defun factorial (n)",
               "   (if (> 1 n)      ",
               "            1       ",
               "            (* n (factorial (- n 1)))))",
               "(factorial 20)    ; comment"
            ]
         expected = Right (expectedEnv, expectedAst)
         expectedEnv = envInsert env "factorial" factorialFunc
         expectedAst = [
               factorialFunc,
               ANumber 2432902008176640000
            ]
         factorialFunc = AFunction {
                  argumentNames = ["n"],
                  body = ACondition {
                     condition = ACall {
                        function = ">",
                        arguments = [ANumber 1, ASymbol "n"]
                     },
                     ifTrue = ANumber 1,
                     ifFalse = ACall {
                        function = "*",
                        arguments = [
                           ASymbol "n",
                           ACall {
                              function = "factorial",
                              arguments = [
                                 ACall {
                                    function = "-",
                                    arguments = [ASymbol "n", ANumber 1]
                                 }
                              ]
                           }
                        ]
                     }
                  }
               }

testInternalFunctions :: Test
testInternalFunctions = TestCase $ assertEqual "Internal functions valid" expected result
    where
         result = runCode env code
         env = internalEnv
         code = unlines [
               "(+ 1 2)",
               "(div 10 2)",
               "(mod 10 3)",
               "(= 1 1)",
               "(< 1 2)",
               "(> 1 2)",
               "(<= 1 2)",
               "(>= 1 2)",
               "(- 1 2)",
               "(* 1 2)",
               "(not (and (= 1 1) (= 1 2)))",
               "(or (= 1 1) (= 1 2))"
            ]
         expected = Right (expectedEnv, expectedAst)
         expectedEnv = env
         expectedAst = [
               ANumber 3,
               ANumber 5,
               ANumber 1,
               ABoolean True,
               ABoolean True,
               ABoolean False,
               ABoolean True,
               ABoolean False,
               ANumber (-1),
               ANumber 2,
               ABoolean True,
               ABoolean True
            ]


executionTestList :: Test
executionTestList = TestList [
      -- testRecursiveFunction,
      -- testInternalFunctions
    ]
