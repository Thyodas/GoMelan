{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module InternalFunctions (internalEnv) where

import VirtualMachine.Vm (Args(..), Val(..))
import Ast (GomAST(..), EvalResult(..), Env,
    throwEvalError)

newtype InternalFunction = InternalFunction (Args -> Either String Val)


internalEnv :: Env
internalEnv = [
        ("len", AGomInternalFunction
            "len"
            (AGomParameterList [AGomTypedIdentifier "list" (AGomTypeList
                [AGomTypeAny])])
            (AGomType "Int"))
    ]
