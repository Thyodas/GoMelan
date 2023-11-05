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
        ("len", VInternalFunction (InternalFunction "len" lenOfList)),
        ("intToFloat", VInternalFunction (InternalFunction "intToFloat" intToFloat)),
        ("floatToInt", VInternalFunction (InternalFunction "floatToInt" floatToInt))
    ]

lenOfList :: Args -> Either String Val
lenOfList [] = Left "lenOfList: invalid number of arguments"
lenOfList [VList list] = Right $ VNum $ length list
lenOfList _ = Left "lenOfList: invalid argument"

intToFloat :: Args -> Either String Val
intToFloat [] = Left "intToFloat: invalid number of argumentss"
intToFloat [VNum x] = Right $ VFloatNum $ fromIntegral x
intToFloat _ = Left "intToFloat: invalid argument type expected Int"

floatToInt :: Args -> Either String Val
floatToInt [] = Left "floatToInt: invalid number of argumentss"
floatToInt [VFloatNum x] = Right $ VNum $ round x
floatToInt _ = Left "floatToInt: invalid argument type expected Int"
