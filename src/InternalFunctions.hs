{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module InternalFunctions (astInternalEnv) where

import Ast (GomAST(..), Env)

astInternalEnv :: Env
astInternalEnv = [
        ("len", AGomInternalFunction
            "len"
            (AGomParameterList [AGomTypedIdentifier "list" (AGomTypeList
                [AGomTypeAny])])
            (AGomType "Int"))
    ]
