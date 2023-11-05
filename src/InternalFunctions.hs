{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module InternalFunctions (astInternalEnv) where

import Ast (GomAST(..), Env, EnvEntry)

astInternalEnv :: Env
astInternalEnv = [
        lenFuncType,
        intToFloatFuncType,
        floatToIntFuncType,
        stringToIntFuncType,
        intToStringFuncType,
        intToBoolFuncType,
        floatToStringFuncType
    ]

lenFuncType :: EnvEntry
lenFuncType = (name,
    AGomInternalFunction name (AGomParameterList args) returnType)
    where
        name = "len"
        args = [AGomTypedIdentifier "list" (AGomTypeList [AGomTypeAny])]
        returnType = AGomType "Int"

intToFloatFuncType :: EnvEntry
intToFloatFuncType = (name,
    AGomInternalFunction name (AGomParameterList args) returnType)
    where
        name = "intToFloat"
        args = [AGomTypedIdentifier "int" (AGomType "Int")]
        returnType = AGomType "Float"

floatToIntFuncType :: EnvEntry
floatToIntFuncType = (name,
    AGomInternalFunction name (AGomParameterList args) returnType)
    where
        name = "floatToInt"
        args = [AGomTypedIdentifier "float" (AGomType "Float")]
        returnType = AGomType "Int"

stringToIntFuncType :: EnvEntry
stringToIntFuncType = (name,
    AGomInternalFunction name (AGomParameterList args) returnType)
    where
        name = "stringToInt"
        args = [AGomTypedIdentifier "float" (AGomTypeList [AGomType "Char"])]
        returnType = AGomType "Int"

intToStringFuncType :: EnvEntry
intToStringFuncType = (name,
    AGomInternalFunction name (AGomParameterList args) returnType)
    where
        name = "intToString"
        args = [AGomTypedIdentifier "int" (AGomType "Int")]
        returnType = (AGomTypeList [AGomType "Char"])

intToBoolFuncType :: EnvEntry
intToBoolFuncType = (name,
    AGomInternalFunction name (AGomParameterList args) returnType)
    where
        name = "intToBool"
        args = [AGomTypedIdentifier "int" (AGomType "Int")]
        returnType = AGomType "Bool"

floatToStringFuncType :: EnvEntry
floatToStringFuncType = (name,
    AGomInternalFunction name (AGomParameterList args) returnType)
    where
        name = "floatToString"
        args = [AGomTypedIdentifier "float" (AGomType "Float")]
        returnType = (AGomTypeList [AGomType "Char"])