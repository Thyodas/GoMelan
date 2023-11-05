{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- Spec
-}

module AstTest (astTestList) where

import Test.HUnit
import Data.Maybe
import InternalFunctions (astInternalEnv)
import Ast (Env, envInsert, envLookup, GomAST(..), EvalError(..), EnvKey, EnvValue, EnumOperator(..), GomExprType(..),
   EvalResult(..), GomExpr(..), gomExprToGomAST,
   typeResolver, extractSymbol, applyToSnd, envLookupEval, checkType,
   getAGomFunctionDefinition, throwEvalError, gomExprToAGomFunctionCall,
   operatorToGomAST, getIdDetails, precedence, gomExprToAGomAssignment
   )

import VirtualMachine.Vm (EnumOperator(..))

testEnv :: Env
testEnv = astInternalEnv ++  [
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
        TestCase $ assertEqual "Testing invald matching pattern" expected10 result10,
        TestCase $ assertEqual "Testing empty list" expected11 result11,
        TestCase $ assertEqual "Testing invalid list type" result12 expected12,
        TestCase $ assertEqual "Testing invalid, Couldn't resolve type" result13 expected13
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

        result6 = typeResolver [] (AGomCharLiteral 'o')
        expected6 = pure (AGomType "Char")

        result7 = typeResolver [] (AGomList [AGomNumber 1, AGomNumber 2, AGomNumber 3])
        expected7 = pure (AGomTypeList [AGomType "Int"])

        result8 = typeResolver [("key5", AGomNumber 42)] (AGomIdentifier "key5")
        expected8 = pure (AGomType "Int")

        result9 = typeResolver [("add", AGomNumber 42)] (AGomFunctionCall { aGomFunctionName = "add", aGomFunctionArguments = AGomEmpty })
        expected9 = throwEvalError ("Identifier 'add' is not a function") []

        result10 = typeResolver [("add", AGomFunctionDefinition { aGomFnName = "add", aGomFnArguments = AGomEmpty, aGomFnBody = AGomEmpty, aGomFnReturnType = AGomType "Int" })] (AGomFunctionCall { aGomFunctionName = "add", aGomFunctionArguments = AGomEmpty })
        expected10 = pure (AGomType "Int")

        result11 = typeResolver [] (AGomList [])
        expected11 = throwEvalError "Empty List" []

        result12 = typeResolver [] (AGomList [AGomNumber 1, AGomCharLiteral 'e'])
        expected12 = throwEvalError "Types mismatch in list, found '[AGomType \"Int\",AGomType \"Char\"]'" []

        result13 = typeResolver [] (AGomEmpty)
        expected13 = throwEvalError "Couldn't resolve type for 'AGomEmpty'." []


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
        TestCase $ assertEqual "Testing List" expected11 result11,
        TestCase $ assertEqual "Testing Block" expected12 result12,
        TestCase $ assertEqual "Testing ParameterList" expected13 result13,
        TestCase $ assertEqual "Testing FunctionCall" expected14 result14,
        TestCase $ assertEqual "Testing TypedIdentifier" expected15 result15,
        TestCase $ assertEqual "Testing IncludeStatement" expected16 result16,
        TestCase $ assertEqual "Testing Assignment" expected17 result17,
        TestCase $ assertEqual "Testing ForLoopIter" expected18 result18,
        TestCase $ assertEqual "Testing Condition" expected19 result19,
        TestCase $ assertEqual "Testing Function" expected20 result20,
        TestCase $ assertEqual "Testing Shunting Yard" expected21 result21,
        TestCase $ assertEqual "Testing Shunting Yard with function call" expected22 result22,
        TestCase $ assertEqual "Testing Shunting Yard with massive operators" expected23 result23,
        TestCase $ assertEqual "Testing Shunting Yard and return ([], AGomReturnStatement expr')" expected24 result24
    ]
    where

        result1 = gomExprToGomAST [] (Number 42)
        expected1 = pure ([], AGomNumber 42)

        result2 = gomExprToGomAST [] (Identifier "quarante_deux")
        expected2 = pure ([], AGomIdentifier "quarante_deux")

        result3 = gomExprToGomAST [] (Character 'o')
        expected3 = pure ([], AGomCharLiteral 'o')

        result4 = gomExprToGomAST [] (Boolean True)
        expected4 = pure ([], AGomBooleanLiteral True)

        result5 = gomExprToGomAST [] (Type (SingleType "Int"))
        expected5 = pure ([], AGomType "Int")

        result6 = gomExprToGomAST [] (Type (TypeList [SingleType "String", SingleType "String", SingleType "String"]))
        expected6 = pure ([],AGomTypeList [AGomType "String",AGomType "String",AGomType "String"])

        result7 = gomExprToGomAST [] (Statements [Identifier "x", Number 42])
        expected7 = pure ([], AGomStatements [AGomIdentifier "x", AGomNumber 42])

        result8 = gomExprToGomAST [] (Operator "+")
        expected8 = pure ([], AGomOperator SignPlus)

        result9 = gomExprToGomAST [] (Term [Identifier "x", Operator "*", Number 42])
        expected9 = pure ([], AGomTerm [AGomIdentifier "x", AGomOperator SignMultiply, AGomNumber 42])

        result10 = gomExprToGomAST [] (Expression [Number 3, Operator "+", Number 4, Operator "*", Number 6])
        expected10 = pure ([], AGomExpression [AGomNumber 3,AGomNumber 4,AGomNumber 6,AGomOperator SignMultiply,AGomOperator SignPlus])

        result11 = gomExprToGomAST [] (List [Number 21, Number 42, Number 84])
        expected11 = pure ([], AGomList [AGomNumber 21, AGomNumber 42, AGomNumber 84])

        result12 = gomExprToGomAST [] (Block [Identifier "x", Operator "/", Number 4])
        expected12 = pure ([], AGomBlock [AGomIdentifier "x", AGomOperator SignDivide, AGomNumber 4])

        result13 = gomExprToGomAST [] (ParameterList [TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")},TypedIdentifier {identifier = "y", identifierType = Type (SingleType "Int")}])
        expected13 = pure ([], AGomParameterList  [AGomTypedIdentifier {aGomIdentifier = "x", aGomIdentifierType = AGomType "Int"},AGomTypedIdentifier {aGomIdentifier = "y", aGomIdentifierType = AGomType "Int"}])

        result14 = gomExprToGomAST [("add", AGomFunctionDefinition { aGomFnName = "add", aGomFnArguments = AGomParameterList [AGomTypedIdentifier "x" (AGomType "Int")], aGomFnBody = AGomEmpty, aGomFnReturnType = AGomType "Int" })] (FunctionCall { functionName = Identifier "add", functionArguments = ParameterList [Number 42] })
        expected14 = pure ([("add",AGomFunctionDefinition {aGomFnName = "add", aGomFnArguments = AGomParameterList [AGomTypedIdentifier "x" (AGomType "Int")], aGomFnBody = AGomEmpty, aGomFnReturnType = AGomType "Int"})],AGomFunctionCall {aGomFunctionName = "add", aGomFunctionArguments = AGomParameterList [AGomNumber 42]})

        result15 = gomExprToGomAST [] (TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")})
        expected15 = pure ([], AGomTypedIdentifier {aGomIdentifier = "x", aGomIdentifierType = AGomType "Int"})

        result16 = gomExprToGomAST [] (IncludeStatement { includeList = Identifier "*", fromModule = Identifier "myModule" })
        expected16 = pure ([], AGomIncludeStatement { aGomIncludeList = AGomIdentifier "*", aGomFromModule = AGomIdentifier "myModule" })

        result17 = gomExprToGomAST [("x", AGomNumber 41)] (Assignment { assignedIdentifier = Identifier "x", assignedExpression = Number 42 })
        expected17 = pure ([("x", AGomNumber 42)], AGomAssignment {aGomAssignedIdentifier = AGomIdentifier "x", aGomAssignedExpression = AGomNumber 42})

        result18 = gomExprToGomAST [] (ForLoopIter { forLoopInitialization = Empty, forLoopCondition = Expression [Number 42, Operator "<", Number 84], forLoopUpdate = Empty, forLoopIterBlock = Empty })
        expected18 = pure ([],AGomForLoop {aGomForLoopInitialization = AGomEmpty, aGomForLoopCondition = AGomExpression [AGomNumber 42,AGomNumber 84,AGomOperator SignInf], aGomForLoopUpdate = AGomEmpty, aGomForLoopIterBlock = AGomEmpty})

        result19 = gomExprToGomAST [] (Condition { gomIfCondition = Expression [Number 42, Operator "<", Number 84], gomIfTrue = Expression [Boolean True], gomIfFalse = Expression [Boolean False] })
        expected19 = pure ([],AGomCondition {aGomIfCondition = AGomExpression [AGomNumber 42,AGomNumber 84,AGomOperator SignInf], aGomIfTrue = AGomExpression [AGomBooleanLiteral True], aGomIfFalse = AGomExpression [AGomBooleanLiteral False]})

        main20 = AGomFunctionDefinition {aGomFnName = "add", aGomFnArguments = AGomParameterList [AGomTypedIdentifier {aGomIdentifier = "x", aGomIdentifierType = AGomType "Int"}], aGomFnBody = AGomEmpty, aGomFnReturnType = AGomType "Int"}
        result20 = gomExprToGomAST [] (Function { fnName = "add", fnArguments = ParameterList [TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")}], fnBody = Empty, fnReturnType = Type (SingleType "Int") })
        expected20 = pure ([("add", main20)], main20)

        result21 = gomExprToGomAST [] (Expression [Number 1,Operator "+",Number 1,Operator "*",Number 2])
        expected21 = EvalResult $ Right $ ([], AGomExpression [AGomNumber 1,AGomNumber 1,AGomNumber 2,AGomOperator SignMultiply,AGomOperator SignPlus])

        main22 = AGomFunctionDefinition {aGomFnName = "main", aGomFnArguments = AGomParameterList [], aGomFnBody = AGomBlock [AGomExpression [AGomFunctionCall {aGomFunctionName = "main", aGomFunctionArguments = AGomParameterList []},AGomNumber 1,AGomNumber 2,AGomOperator SignMultiply,AGomOperator SignPlus]], aGomFnReturnType = AGomType "Int"}
        result22 = gomExprToGomAST [] (Function {fnName = "main", fnArguments = ParameterList [], fnBody = Block [Expression [FunctionCall {functionName = Identifier "main", functionArguments = ParameterList []},Operator "+",Number 1,Operator "*",Number 2]], fnReturnType = Type (SingleType "Int")})
        expected22 = EvalResult $ Right $ ([("main", main22)], main22)

        result23 = gomExprToGomAST [] (Expression [Number 10,Operator "-",Number 1,Operator "/",Number 3,Operator "==",Number 3,Operator "&&",Number 5,Operator "<=",Number 34,Operator ">=",Number 56,Operator "<",Number 1,Operator ">",Number 100,Operator "&&",Number 4,Operator "!",Number 90,Operator "!=",Number 70])
        expected23 = EvalResult $ Right $ ([],AGomExpression [AGomNumber 10,AGomNumber 1,AGomNumber 3,AGomOperator SignDivide,AGomOperator SignMinus,AGomNumber 3,AGomOperator SignEqual,AGomNumber 5,AGomNumber 34,AGomOperator SignInfEqual,AGomOperator SignAnd,AGomNumber 56,AGomOperator SignSupEqual,AGomNumber 1,AGomOperator SignInf,AGomNumber 100,AGomOperator SignSup,AGomNumber 4,AGomNumber 90,AGomOperator SignNot,AGomOperator SignAnd,AGomNumber 70,AGomOperator SignNotEqual])

        main24 = AGomFunctionDefinition {aGomFnName = "main", aGomFnArguments = AGomParameterList [], aGomFnBody = AGomBlock [AGomExpression [AGomFunctionCall {aGomFunctionName = "main", aGomFunctionArguments = AGomParameterList []},AGomNumber 1,AGomNumber 2,AGomOperator SignMultiply,AGomOperator SignPlus]], aGomFnReturnType = AGomType "Int"}
        result24 = gomExprToGomAST [("main", main24)] (ReturnStatement (Number 42))
        expected24 = EvalResult $ Right $ ([], AGomReturnStatement (AGomNumber 42))



testEqualType :: Test
testEqualType = TestList [
            TestCase $ assertEqual "Testing equal type in an expression" expected1 result1,
            TestCase $ assertEqual "Testing not equal type in an expression" expected2 result2,
            TestCase $ assertEqual "Testing empty expression" expected3 result3
        ]

        where
            result1 = typeResolver [] (AGomExpression [AGomNumber 1,AGomNumber 1,AGomOperator SignPlus,AGomNumber 2,AGomOperator SignMultiply])
            expected1 = pure (AGomType "Int")

            result2 = typeResolver [] (AGomExpression [AGomNumber 1,AGomNumber 1,AGomOperator SignPlus,AGomCharLiteral 'e',AGomOperator SignMultiply])
            expected2 = throwEvalError "Types mismatch in expression, found '[AGomType \"Int\",AGomType \"Char\"]'" []

            result3 = typeResolver [] (AGomExpression [])
            expected3 = throwEvalError "Empty expression" []

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
    TestCase $ assertEqual "Apply function to second element of tuple" (2 :: Integer, 5 :: Integer) (applyToSnd (+2) (2 :: Integer, 3 :: Integer)),
    TestCase $ assertEqual "Apply function to second element of tuple" (2 :: Integer, 8 :: Integer) (applyToSnd (*2) (2 :: Integer, 4 :: Integer))
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
    [ TestCase $ assertEqual "Get function definition" expected1 result1
    , TestCase $ assertEqual "Get non-existent function definition" expected2 result2
    , TestCase $ assertEqual "Invalid argument type" expected3 result3
    , TestCase $ assertEqual "Invalid argument" expected4 result4
    , TestCase $ assertEqual "getFunctionDef not a function in env" expected5 result5
    ]
    where
        result1 = getAGomFunctionDefinition [("foo", AGomFunctionDefinition "foo" (AGomParameterList []) AGomEmpty AGomEmpty)] "foo"
        expected1 = EvalResult (Right (AGomFunctionDefinition "foo" (AGomParameterList []) AGomEmpty AGomEmpty))

        result2 = getAGomFunctionDefinition [("foo", AGomFunctionDefinition "foo" (AGomParameterList []) AGomEmpty AGomEmpty)] "bar"
        expected2 = EvalResult (Left (EvalError "Identifier 'bar' not found in env" []))

        result3 = getAGomFunctionDefinition [("foo", AGomFunctionDefinition "foo" (AGomParameterList [AGomNumber 1]) AGomEmpty AGomEmpty)] "bar"
        expected3 = EvalResult (Left (EvalError "Identifier 'bar' not found in env" []))

        result4 = getAGomFunctionDefinition [("nameFunc", AGomFunctionDefinition "nameFunc" (AGomEmpty) AGomEmpty AGomEmpty)] "nameFunc"
        expected4 = throwEvalError ("Function '" ++ "nameFunc" ++ "' has invalid arguments") []

        result5 = getAGomFunctionDefinition [("nameFunc", AGomEmpty)] "nameFunc"
        expected5 = throwEvalError ("Identifier '" ++ "nameFunc" ++ "' is not a function") []

testGomExprToAGomFunctionCall :: Test
testGomExprToAGomFunctionCall = TestList [
        TestCase $ assertEqual "gomExprToAGomFunctionCall" expected1 result1,
        TestCase $ assertEqual "gomExprToAGomFunctionCall" expected2 result2,
        TestCase $ assertEqual "gomExprToAGomFunctionCall" expected3 result3,
        TestCase $ assertEqual "gomExprToAGomFunctionCall" expected4 result4,
        TestCase $ assertEqual "gomExprToAGomFunctionCall" expected5 result5,
        TestCase $ assertEqual "gomExprToAGomFunctionCall" expected6 result6,
        TestCase $ assertEqual "gomExprToAGomFunctionCall" expected7 result7
    ]
    where
        env = [("foo", AGomFunctionDefinition "foo" (AGomParameterList []) AGomEmpty AGomEmpty)]

        result1 = gomExprToAGomFunctionCall env (FunctionCall (Identifier "bar") (ParameterList []))
        expected1 = EvalResult {unEvalResult = Left (EvalError "Identifier 'bar' not found in env" [])}

        result2 = gomExprToAGomFunctionCall env (FunctionCall (Identifier "foo") (Identifier "bar"))
        expected2 = EvalResult {unEvalResult = Left (EvalError "Expected a ParameterList" [Identifier "bar"])}

        result3 = gomExprToAGomFunctionCall env (Identifier "foo")
        expected3 = (EvalResult $ Left $ EvalError "Expected a FunctionCall" [])

        result4 = gomExprToAGomFunctionCall env (FunctionCall (Identifier "foo") (ParameterList []))
        expected4 = EvalResult $ Right (env, AGomFunctionCall ("foo") (AGomParameterList []))

        result5 = gomExprToAGomFunctionCall env (FunctionCall (Identifier "foo") (ParameterList [Identifier "bar"]))
        expected5 = EvalResult $ Right (env, AGomFunctionCall ("foo") (AGomParameterList [AGomIdentifier "bar"]))

        result6 = gomExprToAGomFunctionCall env (FunctionCall (Identifier "foo") (ParameterList [Identifier "bar", Identifier "baz"]))
        expected6 = EvalResult $ Right (env, AGomFunctionCall ("foo") (AGomParameterList [AGomIdentifier "bar", AGomIdentifier "baz"]))

        result7 = gomExprToAGomFunctionCall env (FunctionCall (List [Character 'n', Character 'a', Character 'm', Character 'e']) Empty)
        expected7 = EvalResult {unEvalResult = Left (EvalError "Expected an Identifier" [List [Character 'n', Character 'a', Character 'm', Character 'e']])}

testOperatorToGomAST :: Test
testOperatorToGomAST = TestList
    [ TestCase $ assertEqual "Operator to GomAST" expected1 result1
    , TestCase $ assertEqual "Operator to GomAST" expected2 result2
    , TestCase $ assertEqual "Operator to GomAST" expected3 result3
    , TestCase $ assertEqual "Operator to GomAST" expected4 result4
    , TestCase $ assertEqual "Operator to GomAST" expected5 result5
    , TestCase $ assertEqual "Operator to GomAST" expected6 result6
    , TestCase $ assertEqual "Operator to GomAST" expected7 result7
    , TestCase $ assertEqual "Operator to GomAST" expected8 result8
    , TestCase $ assertEqual "Operator to GomAST" expected9 result9
    , TestCase $ assertEqual "Operator to GomAST" expected10 result10
    , TestCase $ assertEqual "Operator to GomAST" expected11 result11
    , TestCase $ assertEqual "Operator to GomAST" expected12 result12
    , TestCase $ assertEqual "Operator to GomAST" expected13 result13
    , TestCase $ assertEqual "Operator to GomAST" expected14 result14
    , TestCase $ assertEqual "Operator to GomAST" expected15 result15
    , TestCase $ assertEqual "Operator to GomAST error" expected16 result16
    ]
    where
        result1 = operatorToGomAST (Operator "+")
        expected1 = pure (AGomOperator SignPlus)

        result2 = operatorToGomAST (Operator "-")
        expected2 = pure (AGomOperator SignMinus)

        result3 = operatorToGomAST (Operator "*")
        expected3 = pure (AGomOperator SignMultiply)

        result4 = operatorToGomAST (Operator "/")
        expected4 = pure (AGomOperator SignDivide)

        result5 = operatorToGomAST (Operator "==")
        expected5 = pure (AGomOperator SignEqual)

        result6 = operatorToGomAST (Operator "!=")
        expected6 = pure (AGomOperator SignNotEqual)

        result7 = operatorToGomAST (Operator "<=")
        expected7 = pure (AGomOperator SignInfEqual)

        result8 = operatorToGomAST (Operator ">=")
        expected8 = pure (AGomOperator SignSupEqual)

        result9 = operatorToGomAST (Operator "<")
        expected9 = pure (AGomOperator SignInf)

        result10 = operatorToGomAST (Operator ">")
        expected10 = pure (AGomOperator SignSup)

        result11 = operatorToGomAST (Operator "&&")
        expected11 = pure (AGomOperator SignAnd)

        result12 = operatorToGomAST (Operator "!")
        expected12 = pure (AGomOperator SignNot)

        result13 = operatorToGomAST (Operator "%")
        expected13 = pure (AGomOperator SignModulo)

        result14 = operatorToGomAST (Operator "&")
        expected14 = throwEvalError ("Unknown operator '" ++ "&" ++ "'") []

        result15 = operatorToGomAST (Operator "||")
        expected15 = pure (AGomOperator SignOr)

        result16 = operatorToGomAST (Number 42)
        expected16 = throwEvalError ("Expected an Operator") []

testGetIdDetails :: Test
testGetIdDetails = TestList
    [ TestCase $ assertEqual "Get details for an identifier" expected result
    , TestCase $ assertEqual "Get details for a typed identifier" expected2 result2
    , TestCase $ assertEqual "Error when identifier redeclaration" expected3 result3
    , TestCase $ assertEqual "Throw error when input is not an identifier" expected4 result4
    , TestCase $ assertEqual "Handle envLookup returning Nothing" expected5 result5
    ]
    where
        env = [("x", AGomNumber 5)]
        env2 = [("y", AGomNumber 10)]
        env3 = [("z", AGomNumber 15)]
        env4 = [("w", AGomNumber 20)]
        env5 = []

        result = getIdDetails env (AGomIdentifier "x")
        expected = EvalResult $ Right ("x", AGomNumber 5)

        result2 = getIdDetails env2 (AGomTypedIdentifier "y" (AGomType "Int"))
        expected2 = EvalResult {unEvalResult = Left (EvalError "Cannot redeclare 'y' already exists" [])}

        result3 = getIdDetails env3 (AGomTypedIdentifier "z" (AGomType "Int"))
        expected3 = EvalResult $ Left (EvalError "Cannot redeclare 'z' already exists" [])

        result4 = getIdDetails env4 (AGomNumber 20)
        expected4 = EvalResult (Left (EvalError "Expected an Identifier" []))

        result5 = getIdDetails env5 (AGomTypedIdentifier "non_existing" (AGomType "String"))
        expected5 = EvalResult {unEvalResult = Right ("non_existing",AGomType "String")}

testprecedence :: Test
testprecedence = TestList
    [ TestCase $ assertEqual "precedence" 3 (precedence(Operator "+"))
    , TestCase $ assertEqual "precedence" 3 (precedence(Operator "-"))
    , TestCase $ assertEqual "precedence" 4 (precedence(Operator "*"))
    , TestCase $ assertEqual "precedence" 4 (precedence(Operator "/"))
    , TestCase $ assertEqual "precedence" 2 (precedence(Operator "=="))
    , TestCase $ assertEqual "precedence" 2 (precedence(Operator "!="))
    , TestCase $ assertEqual "precedence" 2 (precedence(Operator "<="))
    , TestCase $ assertEqual "precedence" 2 (precedence(Operator ">="))
    , TestCase $ assertEqual "precedence" 2 (precedence(Operator "<"))
    , TestCase $ assertEqual "precedence" 2 (precedence(Operator ">"))
    , TestCase $ assertEqual "precedence" 1 (precedence(Operator "&&"))
    , TestCase $ assertEqual "precedence" 5 (precedence(Operator "!"))
    , TestCase $ assertEqual "precedence" 0 (precedence(Operator "&"))
    , TestCase $ assertEqual "precedence" 0 (precedence(Number 42))
    , TestCase $ assertEqual "precedence" 1 (precedence(Operator "||"))
    , TestCase $ assertEqual "precedence" 4 (precedence(Operator "%"))
    ]

testGomExprToAGomAssignment :: Test
testGomExprToAGomAssignment = TestList [
        TestCase $ assertEqual "gomExprToAGomAssignment" expected1 result1,
        TestCase $ assertEqual "gomExprToAGomAssignment" expected2 result2,
        TestCase $ assertEqual "gomExprToAGomAssignment" expected3 result3,
        TestCase $ assertEqual "gomExprToAGomAssignment" expected4 result4,
        TestCase $ assertEqual "gomExprToAGomAssignment" expected5 result5,
        TestCase $ assertEqual "gomExprToAGomAssignment" expected6 result6
    ]
    where
        env = [("x", AGomNumber 5)]
        env2 = [("y", AGomNumber 10)]
        env3 = [("z", AGomNumber 15)]
        env4 = []
        env5 = []

        result1 = gomExprToAGomAssignment env (Assignment (Identifier "x") (Number 10))
        expected1 = EvalResult {unEvalResult = Right ([("x",AGomNumber 10)],AGomAssignment {aGomAssignedIdentifier = AGomIdentifier "x", aGomAssignedExpression = AGomNumber 10})}

        result2 = gomExprToAGomAssignment env2 (Assignment (Identifier "y") (List [Character 'h', Character 'e', Character 'l', Character 'l', Character 'o']))
        expected2 = EvalResult {unEvalResult = Left (EvalError "Type mismatch, found 'AGomTypeList [AGomType \"Char\"]' but expected 'AGomType \"Int\"'." [])}

        result3 = gomExprToAGomAssignment env3 (Assignment (Identifier "z") (Boolean True))
        expected3 = EvalResult {unEvalResult = Left (EvalError "Type mismatch, found 'AGomType \"Bool\"' but expected 'AGomType \"Int\"'." [])}

        result4 = gomExprToAGomAssignment env4 (Number 10)
        expected4 = EvalResult {unEvalResult = Left (EvalError "Expected an Assignment" [Number 10])}

        result5 = gomExprToAGomAssignment env5 (Assignment (Identifier "non_existing") (Number 10))
        expected5 = EvalResult $ Left (EvalError "Identifier 'non_existing' not found in env" [])

        result6 = gomExprToAGomAssignment env5 (Assignment (Number 10) (Number 20))
        expected6 = EvalResult $ Left (EvalError "Expected an Identifier" [])

testEqGomExpr :: Test
testEqGomExpr = TestList
    [
    TestCase $ assertEqual "Equality for Number" expr1 expr2,
    TestCase $ assertBool "Inequality for Number" (expr1 /= expr3),
    TestCase $ assertEqual "Equality for Identifier" expr4 expr5,
    TestCase $ assertBool "Inequality for Identifier" (expr4 /= expr6)
    ]
    where
        expr1 = Number 10
        expr2 = Number 10
        expr3 = Number 20
        expr4 = Identifier "x"
        expr5 = Identifier "x"
        expr6 = Identifier "y"

testFunctionName :: Test
testFunctionName = TestList
    [
    TestCase $ assertEqual "Extract Function Name" func (functionName functionCall)
    ]
    where
        func = Identifier "add"
        args = Number 1
        functionCall = FunctionCall func args

testFunctionArguments :: Test
testFunctionArguments = TestList
    [ TestCase $ assertEqual "Extract Function Arguments" args (functionArguments functionCall)
    ]
    where
        func = Identifier "add"
        args = Number 1
        functionCall = FunctionCall func args

-- testIdentifier :: Test
-- testIdentifier = TestList
--     [ TestCase $ assertEqual "Extract Identifier" expected (identifier result)
--     ]
--     where
--         result = Identifier "x"
--         expected = "x"

testIdentifierType :: Test
testIdentifierType = TestList
    [ TestCase $ assertEqual "Extract Identifier Type" identType (identifierType identTypeExpr)
    ]
    where
        identType = Identifier "int"
        identTypeExpr = TypedIdentifier "x" identType

testIncludeList :: Test
testIncludeList = TestList
    [ TestCase $ assertEqual "Extract Include List" include (includeList includeExpr)
    ]
    where
        include = Identifier "moduleA"
        includeExpr = IncludeStatement include (Identifier "moduleB")

testFromModule :: Test
testFromModule = TestCase $ assertEqual "Extract fromModule" expected (fromModule includeExpr)
  where
    include = Identifier "moduleA"
    from = Identifier "moduleB"
    includeExpr = IncludeStatement include from
    expected = from


astTestList :: Test
astTestList = TestList [
    testFromModule,
    testIdentifierType,
    testIncludeList,
    testFunctionArguments,
    testFunctionName,
    testEqGomExpr,
    testGomExprToAGomAssignment,
    testGetIdDetails,
    testOperatorToGomAST,
    testExtractSymbol,
    testApplyToSnd,
    testInsert,
    testLookupExists,
    testLookupNotExists,
    testEnvLookupEval,
    testCheckType,
    testGetAGomFunctionDefinition,
    testTypeResolver,
    testGomExprToGomAST,
    testGomExprToAGomFunctionCall,
    testEqualType,
    testprecedence
    ]
