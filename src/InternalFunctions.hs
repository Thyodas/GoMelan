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
        floatToIntFuncType
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

