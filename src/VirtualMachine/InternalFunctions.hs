{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module VirtualMachine.InternalFunctions where

import VirtualMachine.Vm (Args(..), Val(..), VmEnv(..), InternalFunction(..))
import Data.Either (Either(..))

internalEnv :: VmEnv
internalEnv = [
        ("len", VInternalFunction (InternalFunction lenOfList))
    ]

lenOfList :: Args -> Either String Val
lenOfList [] = Left "lenOfList: invalid number of arguments"
lenOfList [VList list] = Right $ VNum $ length list
lenOfList _ = Left "lenOfList: invalid argument"
