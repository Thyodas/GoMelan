{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module VirtualMachine.InternalFunctions (vmInternalEnv) where

import VirtualMachine.Vm (Args(..), Val(..), VmEnv(..), InternalFunction(..))
import Data.Either (Either(..))

import Data.Binary
import qualified Data.ByteString.Lazy as BS

vmInternalEnv :: VmEnv
vmInternalEnv = [
        ("len", VInternalFunction (InternalFunction "len" lenOfList))
    ]

lenOfList :: Args -> Either String Val
lenOfList [] = Left "lenOfList: invalid number of arguments"
lenOfList [VList list] = Right $ VNum $ length list
lenOfList _ = Left "lenOfList: invalid argument"
