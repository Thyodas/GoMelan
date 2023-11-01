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
import Ast (Env, envInsert, envLookup, GomAST(..), EvalError(..), EnvKey, EnvValue, EnumOperator(..), GomExprType(..),
   EvalResult(..), GomExpr(..), gomExprToGomAST, InternalFunction(..), checkCallArg,
   typeResolver, extractSymbol, applyToSnd, envLookupEval, checkType,
   getAGomFunctionDefinition, throwEvalError, gomExprToAGomFunctionCall)

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
        TestCase $ assertEqual "Testing AGomFunctionCall" expected2 result2,
        TestCase $ assertEqual "Testing AGomExpression" expected3 result3,
        TestCase $ assertEqual "Testing AGomTerm" expected4 result4,
        TestCase $ assertEqual "Testing AGomList" expected5 result5,
        TestCase $ assertEqual "Testing AGomBooleanLiteral" expected6 result6,
        TestCase $ assertEqual "Testing AGomNumber" expected7 result7,
        TestCase $ assertEqual "Testing AGomStringLiteral" expected8 result8,
        TestCase $ assertEqual "Testing AGomIdentifier" expected9 result9
    ]
    where
        result1 = checkCallArg (AGomIdentifier "someIdentifier")
        expected1 = pure ([], AGomIdentifier "someIdentifier")

        result2 = checkCallArg (AGomFunctionCall {aGomFunctionName = (AGomIdentifier "add"), aGomFunctionArguments = (AGomList [AGomNumber 2, AGomNumber 3])})
        expected2 = pure ([], AGomFunctionCall {aGomFunctionName = (AGomIdentifier "add"), aGomFunctionArguments = (AGomList [AGomNumber 2, AGomNumber 3])})

        result3 = checkCallArg (AGomExpression [AGomStringLiteral "BasicExpression"])
        expected3 = pure ([], AGomExpression [AGomStringLiteral "BasicExpression"])

        result4 = checkCallArg (AGomTerm [AGomIdentifier "BasicTerm"])
        expected4 = pure ([], AGomTerm [AGomIdentifier "BasicTerm"])

        result5 = checkCallArg (AGomList [AGomNumber 1, AGomNumber 2, AGomNumber 3])
        expected5 = pure ([], AGomList [AGomNumber 1, AGomNumber 2, AGomNumber 3])

        result6 = checkCallArg (AGomBooleanLiteral True)
        expected6 = pure ([], AGomBooleanLiteral True)

        result7 = checkCallArg (AGomNumber 42)
        expected7 = pure ([], AGomNumber 42)

        result8 = checkCallArg (AGomStringLiteral "someString")
        expected8 = pure ([], AGomStringLiteral "someString")

        result9 = checkCallArg (AGomType "Int")
        expected9 = throwEvalError "Invalid argument type in function call" []

testTypeResolver :: Test
testTypeResolver = TestList [
        TestCase $ assertEqual "Testing AGomType" expected1 result1,
        TestCase $ assertEqual "Testing AGomTypedIdentifier" expected2 result2,
        TestCase $ assertEqual "Testing AGomTypeList" expected3 result3,
        TestCase $ assertEqual "Testing AGomBooleanLiteral" expected4 result4,
        TestCase $ assertEqual "Testing AGomNumber" expected5 result5,
        TestCase $ assertEqual "Testing AGomStringLiteral" expected6 result6,
        TestCase $ assertEqual "Testing invald matching pattern" expected7 result7,
        TestCase $ assertEqual "Testing invald matching pattern" expected8 result8,
        TestCase $ assertEqual "Testing invald matching pattern" expected9 result9,
        TestCase $ assertEqual "Testing invald matching pattern" expected10 result10
    ]
    where
        result1 = typeResolver [] (AGomType "Int")
        expected1 = pure (AGomType "Int")

        result2 = typeResolver [] (AGomTypedIdentifier { aGomIdentifier = "x", aGomIdentifierType = (AGomType "Int") })
        expected2 = pure (AGomType "Int")

        result3 = typeResolver [] (AGomTypeList [AGomNumber 1, AGomNumber 2, AGomNumber 3])
        expected3 = pure (AGomTypeList [AGomNumber 1, AGomNumber 2, AGomNumber 3])

        result4 = typeResolver [] (AGomBooleanLiteral True)
        expected4 = pure (AGomType "Bool")

        result5 = typeResolver [] (AGomNumber 42)
        expected5 = pure (AGomType "Int")

        result6 = typeResolver [] (AGomStringLiteral "my_42")
        expected6 = pure (AGomType "String")

        result7 = typeResolver [] (AGomList [AGomNumber 1, AGomNumber 2, AGomNumber 3])
        expected7 = throwEvalError ("Couldn't resolve type for 'AGomList [AGomNumber 1,AGomNumber 2,AGomNumber 3]'.") []

        result8 = typeResolver [("key5", AGomNumber 42)] (AGomIdentifier "key5")
        expected8 = pure (AGomType "Int")

        result9 = typeResolver [("add", AGomNumber 42)] (AGomFunctionCall { aGomFunctionName = AGomIdentifier "add", aGomFunctionArguments = AGomEmpty })
        expected9 = throwEvalError ("Identifier 'add' is not a function") []

        result10 = typeResolver [("add", AGomFunctionDefinition { aGomFnName = "add", aGomFnArguments = AGomEmpty, aGomFnBody = AGomEmpty, aGomFnReturnType = AGomType "Int" })] (AGomFunctionCall { aGomFunctionName = AGomIdentifier "add", aGomFunctionArguments = AGomEmpty })
        expected10 = pure (AGomType "Int")

testGomExprToGomAST :: Test
testGomExprToGomAST = TestList [
        TestCase $ assertEqual "Testing Number" expected1 result1,
        TestCase $ assertEqual "Testing Identifier" expected2 result2,
        TestCase $ assertEqual "Testing GomString" expected3 result3,
        TestCase $ assertEqual "Testing Boolean" expected4 result4,
        TestCase $ assertEqual "Testing Type SingleType" expected5 result5,
        TestCase $ assertEqual "Testing Type TypeList all same" expected6 result6,
        TestCase $ assertEqual "Testing Statement" expected7 result7,
        TestCase $ assertEqual "Testing Operator" expected8 result8,
        TestCase $ assertEqual "Testing Term" expected9 result9,
        TestCase $ assertEqual "Testing Expression" expected10 result10,
        TestCase $ assertEqual "Testing List" expected11 result11
    ]
    where
        result12 = gomExprToGomAST [] (Expression [Number 1,Operator "+",Number 1,Operator "*",Number 2])
        expected12 = EvalResult $ Right $ ([], AGomExpression [AGomNumber 1,AGomNumber 1,AGomOperator SignPlus,AGomNumber 2,AGomOperator SignMultiply])

        result13 = gomExprToGomAST [] (Function {fnName = "main", fnArguments = ParameterList [], fnBody = Block [Expression [FunctionCall {functionName = Identifier "main", functionArguments = ParameterList []},Operator "+",Number 1,Operator "*",Number 2]], fnReturnType = Type (SingleType "Int")})
        expected13 = EvalResult $ Right $ ([],AGomFunctionDefinition {aGomFnName = "main", aGomFnArguments = AGomParameterList [], aGomFnBody = AGomBlock [AGomExpression [AGomFunctionCall {aGomFunctionName = AGomIdentifier "main", aGomFunctionArguments = AGomList []},AGomNumber 1,AGomOperator SignPlus,AGomNumber 2,AGomOperator SignMultiply]], aGomFnReturnType = AGomType "Int"})

        result14 = gomExprToGomAST [] (Expression [Number 10,Operator "-",Number 1,Operator "/",Number 3,Operator "==",Number 3,Operator "&&",Number 5,Operator "<=",Number 34,Operator ">=",Number 56,Operator "<",Number 1,Operator ">",Number 100,Operator "&&",Number 4,Operator "!",Number 90,Operator "!=",Number 70])
        expected14 = EvalResult $ Right $ ([],AGomExpression [AGomNumber 10,AGomNumber 1,AGomOperator SignMinus,AGomNumber 3,AGomOperator SignDivide,AGomNumber 3,AGomOperator SignEqual,AGomNumber 5,AGomNumber 34,AGomNumber 56,AGomNumber 1,AGomNumber 100,AGomOperator SignSup,AGomOperator SignInf,AGomOperator SignSupEqual,AGomOperator SignInfEqual,AGomOperator SignAnd,AGomNumber 4,AGomOperator SignAnd,AGomNumber 90,AGomNumber 70,AGomOperator SignNot,AGomOperator SignNotEqual])

        result1 = gomExprToGomAST [] (Number 42)
        expected1 = pure ([], AGomNumber 42)

        result2 = gomExprToGomAST [] (Identifier "quarante_deux")
        expected2 = pure ([], AGomIdentifier "quarante_deux")

        result3 = gomExprToGomAST [] (GomString "Quarante deux ?!")
        expected3 = pure ([], AGomStringLiteral "Quarante deux ?!")

        result4 = gomExprToGomAST [] (Boolean True)
        expected4 = pure ([], AGomBooleanLiteral True)

        result5 = gomExprToGomAST [] (Type (SingleType "Int"))
        expected5 = pure ([], AGomType "Int")

        result6 = gomExprToGomAST [] (Type (TypeList [SingleType "String", SingleType "String", SingleType "String"]))
        expected6 = pure ([],AGomTypeList [AGomType "String",AGomType "String",AGomType "String"])

        result7 = gomExprToGomAST [] (Statements [Identifier "x", Number 42])
        expected7 = pure ([], AGomStatements [AGomIdentifier "x", AGomNumber 42])

        result8 = gomExprToGomAST [] (Operator "+")
        expected8 = pure ([], AGomOperator "+")

        result9 = gomExprToGomAST [] (Term [Identifier "x", Operator "*", Number 42])
        expected9 = pure ([], AGomTerm [AGomIdentifier "x", AGomOperator "*", AGomNumber 42])

        result10 = gomExprToGomAST [] (Expression [Number 3, Operator "+", Number 4, Operator "*", Number 6])
        expected10 = pure ([], AGomExpression [AGomNumber 3, AGomOperator "+", AGomNumber 4, AGomOperator "*", AGomNumber 6])

        result11 = gomExprToGomAST [] (List [Number 21, Number 42, Number 84])
        expected11 = pure ([], AGomList [AGomNumber 21, AGomNumber 42, AGomNumber 84])

        result12 = gomExprToGomAST [] (Block [Identifier "x", Operator "/", Number 4])
        expected12 = pure ([], AGomBlock [AGomIdentifier "x", AGomOperator SignDivide, AGomNumber 4])

        result13 = gomExprToGomAST [] (ParameterList [TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")},TypedIdentifier {identifier = "y", identifierType = Type (SingleType "Int")}])
        expected13 = pure ([], AGomParameterList  [AGomTypedIdentifier {aGomIdentifier = "x", aGomIdentifierType = AGomType "Int"},AGomTypedIdentifier {aGomIdentifier = "y", aGomIdentifierType = AGomType "Int"}])

        -- result14 = gomExprToGomAST [] (FunctionCall { functionName = Identifier "add", functionArguments = [Number 42] })
        -- expected14 = pure ([], AGomFunctionCall { aGomFunctionName = AGomIdentifier "add", aGomFunctionArguments = [AGomNumber 42] })

        result15 = gomExprToGomAST [] (TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")})
        expected15 = pure ([], AGomTypedIdentifier {aGomIdentifier = "x", aGomIdentifierType = AGomType "Int"})

        result16 = gomExprToGomAST [] (IncludeStatement { includeList = Identifier "*", fromModule = Identifier "myModule" })
        expected16 = pure ([], AGomIncludeStatement { aGomIncludeList = AGomIdentifier "*", aGomFromModule = AGomIdentifier "myModule" })

        result17 = gomExprToGomAST [("x", AGomNumber 41)] (Assignment { assignedIdentifier = Identifier "x", assignedExpression = Number 42 })
        expected17 = pure ([("x", AGomNumber 42)], AGomEmpty)

        result18 = gomExprToGomAST [] (ForLoopIter { forLoopInitialization = Empty, forLoopCondition = Expression [Number 42, Operator "<", Number 84], forLoopUpdate = Empty, forLoopIterBlock = Empty })
        expected18 = pure ([],AGomForLoop {aGomForLoopInitialization = AGomEmpty, aGomForLoopCondition = AGomExpression [AGomNumber 42,AGomNumber 84,AGomOperator SignInf], aGomForLoopUpdate = AGomEmpty, aGomForLoopIterBlock = AGomEmpty})

        result19 = gomExprToGomAST [] (Condition { gomIfCondition = Expression [Number 42, Operator "<", Number 84], gomIfTrue = Expression [Boolean True], gomIfFalse = Expression [Boolean False] })
        expected19 = pure ([],AGomCondition {aGomIfCondition = AGomExpression [AGomNumber 42,AGomNumber 84,AGomOperator SignInf], aGomIfTrue = AGomExpression [AGomBooleanLiteral True], aGomIfFalse = AGomExpression [AGomBooleanLiteral False]})

        result20 = gomExprToGomAST [] (Function { fnName = "add", fnArguments = ParameterList [TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")}], fnBody = Empty, fnReturnType = Type (SingleType "Int") })
        expected20 = pure ([], AGomFunctionDefinition { aGomFnName = "add", aGomFnArguments = AGomParameterList  [AGomTypedIdentifier {aGomIdentifier = "x", aGomIdentifierType = AGomType "Int"}], aGomFnBody = AGomEmpty, aGomFnReturnType = AGomType "Int" })



-- testGomExprToAGomFunctionCall :: Test
-- testGomExprToAGomFunctionCall = TestList [
--         TestCase $ assertEqual "Testing gomExprToAGomFunctionCall" expected1 result1
--     ]
--     where
--         result1 = gomExprToAGomFunctionCall [("add", AGomFunctionDefinition { aGomFnName = "add", aGomFnArguments = AGomParameterList [AGomFunctionArgument "x" (AGomType "Int")], aGomFnBody = AGomEmpty, aGomFnReturnType = AGomType "Int" })] (FunctionCall { functionName = Identifier "add", functionArguments = ParameterList [Number 1] })
--         expected1 = EvalResult $ Right $ ([], (AGomFunctionCall { aGomFunctionName = AGomIdentifier "add", aGomFunctionArguments = AGomParameterList [AGomNumber 1] }))




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
