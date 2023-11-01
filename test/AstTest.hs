{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- Spec
-}

module AstTest (astTestList) where

import Test.HUnit
import Data.Maybe
import InternalFunctions (internalEnv)
import Ast (Env, envInsert, envLookup, GomAST(..), EvalError(..), EnvKey, EnvValue,
   EvalResult(..), GomExpr(..), gomExprToGomAST, InternalFunction(..), checkCallArg,
   extractSymbol, EvalError(..), applyToSnd, gomExprToGomAST, envLookupEval, checkType,
   getAGomFunctionDefinition)

testEnv :: Env
testEnv = internalEnv ++  [
    ("key1", AGomIdentifier "value1"),
    ("key2", AGomIdentifier "value2"),
    ("key3", AGomIdentifier "value3"),
    ("key4", AGomIdentifier "value4")
    ]

testInsert :: Test
testInsert = TestList [
    TestCase $ assertEqual "Element inserted" (newElement : testEnv) (envInsert testEnv "key5" (AGomIdentifier "value5")),
    TestCase $ assertEqual "Element inserted" (newElement2 : testEnv) (envInsert testEnv "key5" (AGomIdentifier "value5"))
    ]
    where
        newElement = ("key5", AGomIdentifier "value5")
        newElement2 = ("key5", AGomIdentifier "value5")

testLookupExists :: Test
testLookupExists = TestCase $ assertEqual "Element exists" (AGomIdentifier "value3") (fromJust (envLookup testEnv "key3"))

testLookupNotExists :: Test
testLookupNotExists = TestCase $ assertEqual "Element not exists" Nothing (envLookup testEnv "key5")

testCheckCallArg :: Test
testCheckCallArg = TestList [
        TestCase $ assertEqual "Testing AGomIdentifier" expected1 result1,
        TestCase $ assertEqual "Testing AGomIdentifier" expected2 result2,
        TestCase $ assertEqual "Testing AGomIdentifier" expected3 result3
        -- TestCase $ assertEqual "Testing AGomIdentifier" expected4 result4
        -- TestCase $ assertEqual "Testing AGomIdentifier" expected5 result5,
        -- TestCase $ assertEqual "Testing AGomIdentifier" expected6 result6,
        -- TestCase $ assertEqual "Testing AGomIdentifier" expected7 result7,
        -- TestCase $ assertEqual "Testing AGomIdentifier" expected8 result8
        -- TestCase $ assertEqual "Testing AGomIdentifier" expected9 result9
    ]
    where
        result1 = checkCallArg (AGomIdentifier "someIdentifier")
        expected1 = pure ([], AGomIdentifier "someIdentifier")

        result2 = checkCallArg (AGomFunctionCall {aGomFunctionName = (AGomIdentifier "add"), aGomFunctionArguments = (AGomList [AGomNumber 2, AGomNumber 3])})
        expected2 = pure ([], AGomFunctionCall {aGomFunctionName = (AGomIdentifier "add"), aGomFunctionArguments = (AGomList [AGomNumber 2, AGomNumber 3])})

        result3 = checkCallArg (AGomExpression [AGomStringLiteral "BasicExpression"])
        expected3 = pure ([], AGomExpression [AGomStringLiteral "BasicExpression"])

        -- result4 = checkCallArg (AGomTerm "someIdentifier")
        -- expected4 = pure ([], AGomTerm "someIdentifier")

        -- result5 = checkCallArg (AGomList [1,2,3])
        -- expected5 = pure ([], AGomList [1,2,3])

        -- result6 = checkCallArg (AGomBooleanLiteral True)
        -- expected6 = pure ([], AGomBooleanLiteral True)

        -- result7 = checkCallArg (AGomNumber 42)
        -- expected7 = pure ([], AGomNumber 42)

        -- result8 = checkCallArg (AGomStringLiteral "someString")
        -- expected8 = pure ([], AGomStringLiteral "someString")

        -- result9 = checkCallArg autrechose
        -- expected9 = pure ([], "Invalid argument type in function call" [])

-- testFunctionCall :: Test
-- testFunctionCall = TestCase $ assertEqual "testFunctionCall" ~: do
--     let input = []
--     let expected =
-- assertEqual "testFunctionDeclaration" expected (fromJust (gomexprToGomAST input))

-- testFunctionDeclaration :: Test
-- testFunctionDeclaration = TestCase $ assertEqual "testFunctionDeclaration" ~: do
--     let input = []
--     let expected =
--     assertEqual "testFunctionDeclaration" expected (fromJust (gomexprToGomAST input))


-- testGomexprToGomAST :: Test
-- testGomexprToGomAST = TestList
--     [ TestCase $ assertEqual "Gomexpr to GomAST" (Just (AGomAssignment "x" (AGomNumber 42))) result
--     , TestCase $ assertEqual "Gomexpr to GomAST" (Just (AGomBooleanLiteral True)) result2
--     , TestCase $ assertEqual "Gomexpr to GomAST" Nothing result3
--     ]
--     where
--         result = gomexprToGomAST (List [AGomIdentifier "define", AGomIdentifier "x", Number 42])
--         result2 = gomexprToGomAST (Boolean True)
--         result3 = gomexprToGomAST (List [])


testExtractSymbol :: Test
testExtractSymbol = TestList [
    TestCase $ assertEqual "Extract symbol from identifier" (Just "foo") (extractSymbol (Identifier "foo")),
    TestCase $ assertEqual "Extract symbol from non-identifier" Nothing (extractSymbol (Number 42))
    ]

testApplyToSnd :: Test
testApplyToSnd = TestList [
    TestCase $ assertEqual "Apply function to second element of tuple" (2, 5) (applyToSnd (+2) (2, 3)),
    TestCase $ assertEqual "Apply function to second element of tuple" (2, 8) (applyToSnd (*2) (2, 4))
    ]

testEnvLookupEval :: Test
testEnvLookupEval = TestList [
    TestCase $ assertEqual "Lookup existing key" (EvalResult (Right (AGomIdentifier "value1"))) (envLookupEval testEnv "key1"),
    TestCase $ assertEqual "Lookup non-existing key" (EvalResult (Left (EvalError "Identifier 'key5' not found in env" []))) (envLookupEval testEnv "key5")
    ]

testCheckType :: Test
testCheckType = TestList [
    TestCase $ assertEqual "Check type with matching types" expected1 result1,
    TestCase $ assertEqual "Check type with non-matching types" expected2 result2,
    TestCase $ assertEqual "Check type with matching typed identifiers" expected3 result3,
    TestCase $ assertEqual "Check type with non-matching typed identifiers" expected4 result4,
    TestCase $ assertEqual "Check type in an environment" expected5 result5
    ]
    where
        result1 = (checkType [] (AGomType "Int") (AGomType "Int"))
        expected1 = (EvalResult (Right (AGomType "Int")))

        result2 = (checkType [] (AGomType "Int") (AGomType "String"))
        expected2 = (EvalResult (Left (EvalError "Type mismatch, found 'AGomType \"Int\"' but expected 'AGomType \"String\"'." [])))

        result3 = (checkType [] (AGomTypedIdentifier "x" (AGomType "Int")) (AGomTypedIdentifier "y" (AGomType "Int")))
        expected3 = (EvalResult (Right (AGomType "Int")))

        result4 = (checkType [] (AGomTypedIdentifier "x" (AGomType "Int")) (AGomTypedIdentifier "y" (AGomType "String")))
        expected4 = (EvalResult (Left (EvalError "Type mismatch, found 'AGomType \"Int\"' but expected 'AGomType \"String\"'." [])))

        environment = [("x", AGomType "Int"), ("y", AGomType "String")]
        result5 = checkType environment (AGomTypedIdentifier "x" (AGomType "Int")) (AGomTypedIdentifier "y" (AGomType "String"))
        expected5 = EvalResult $ Left $ EvalError "Type mismatch, found 'AGomType \"Int\"' but expected 'AGomType \"String\"'." []


testGetAGomFunctionDefinition :: Test
testGetAGomFunctionDefinition = TestList
  [ "Get function definition" ~: do
      let env = [("foo", AGomFunctionDefinition "foo" (AGomParameterList []) (AGomStatements []) (AGomType "Int"))]
      let result = getAGomFunctionDefinition env "foo"
      let expected = EvalResult (Right (AGomFunctionDefinition "foo" (AGomParameterList []) (AGomStatements []) (AGomType "Int")))
      assertEqual "Retrieving an existing function" expected result

  , "Get non-existent function definition" ~: do
      let env = [("foo", AGomFunctionDefinition "foo" (AGomParameterList []) (AGomStatements []) (AGomType "Int"))]
      let result = getAGomFunctionDefinition env "bar"
      let expected = EvalResult (Left (EvalError "Identifier 'bar' not found in env" []))
      assertEqual "Retrieving a non-existent function" expected result

  , "Invalid argument type" ~: do
      let env = [("foo", AGomFunctionDefinition "foo" (AGomParameterList [AGomNumber 1]) (AGomStatements [AGomEmpty]) (AGomType "Int"))]
      let result = getAGomFunctionDefinition env "bar"
      let expected = EvalResult {unEvalResult = Left (EvalError "Identifier 'bar' not found in env" [])}
      assertEqual "Retrieving a function with an invalid argument" expected result
  ]



astTestList :: Test
astTestList = TestList [
    testExtractSymbol,
    testApplyToSnd,
    testInsert,
    testLookupExists,
    testLookupNotExists,
    testEnvLookupEval,
    testCheckType,
    testCheckCallArg,
    testGetAGomFunctionDefinition
    ]
