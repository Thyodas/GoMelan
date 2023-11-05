{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- ExecutionTest
-}

module ExecutionTest (executionTestList) where

import Test.HUnit
import Ast (GomAST(..), envInsert)
import Execution (runCode)

-- testRecursiveFunction :: Test
-- testRecursiveFunction = TestCase $ assertEqual "Recursive function" expected result
--     where
--          result = runCode env code
--          env = internalEnv
--          code = unlines [
--                "(defun factorial (n)",
--                "   (if (> 1 n)      ",
--                "            1       ",
--                "            (* n (factorial (- n 1)))))",
--                "(factorial 20)    ; comment"
--             ]
--          expected = Right (expectedEnv, expectedAst)
--          expectedEnv = envInsert env "factorial" factorialFunc
--          expectedAst = [
--                factorialFunc,
--                AGomNumber 2432902008176640000
--             ]
--          factorialFunc = AGomFunctionDefinition {
--                   aGomFnArguments = ["n"],
--                   aGomFnBody = AGomCondition {
--                      condition = AGomFunctionCall {
--                         aGomFunctionName = ">",
--                         aGomFunctionArguments = [AGomNumber 1, AGomIdentifier "n"]
--                      },
--                      ifTrue = AGomNumber 1,
--                      ifFalse = AGomFunctionCall {
--                         aGomFunctionName = "*",
--                         aGomFunctionArguments = [
--                            AGomIdentifier "n",
--                            AGomFunctionCall {
--                               aGomFunctionName = "factorial",
--                               aGomFunctionArguments = [
--                                  AGomFunctionCall {
--                                     aGomFunctionName = "-",
--                                     aGomFunctionArguments = [AGomIdentifier "n", AGomNumber 1]
--                                  }
--                               ]
--                            }
--                         ]
--                      }
--                   }
--                }

-- testInternalFunctions :: Test
-- testInternalFunctions = TestCase $ assertEqual "Internal functions valid" expected result
--     where
--          result = runCode env code
--          env = internalEnv
--          code = unlines [
--                "(+ 1 2)",
--                "(div 10 2)",
--                "(mod 10 3)",
--                "(= 1 1)",
--                "(< 1 2)",
--                "(> 1 2)",
--                "(<= 1 2)",
--                "(>= 1 2)",
--                "(- 1 2)",
--                "(* 1 2)",
--                "(not (and (= 1 1) (= 1 2)))",
--                "(or (= 1 1) (= 1 2))"
--             ]
--          expected = Right (expectedEnv, expectedAst)
--          expectedEnv = env
--          expectedAst = [
--                AGomNumber 3,
--                AGomNumber 5,
--                AGomNumber 1,
--                AGomBooleanLiteral True,
--                AGomBooleanLiteral True,
--                AGomBooleanLiteral False,
--                AGomBooleanLiteral True,
--                AGomBooleanLiteral False,
--                AGomNumber (-1),
--                AGomNumber 2,
--                AGomBooleanLiteral True,
--                AGomBooleanLiteral True
--             ]


executionTestList :: Test
executionTestList = TestList [
      -- testRecursiveFunction,
      -- testInternalFunctions
    ]
