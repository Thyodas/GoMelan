{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module VirtualMachine.InternalFunctions (vmInternalEnv) where

import VirtualMachine.Vm (Args, Val(..), VmEnv, InternalFunction(..))
import Data.Either()
import Data.Char()
import Data.Binary()
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as BS

internalEntry :: String -> (Args -> Either String Val) -> (String, Val)
internalEntry name func = (name, VInternalFunction (InternalFunction name func))

vmInternalEnv :: VmEnv
vmInternalEnv = [
        internalEntry "len" lenOfList,
        internalEntry "intToFloat" intToFloat,
        internalEntry "floatToInt" floatToInt,
        internalEntry "stringToInt" stringToInt,
        internalEntry "intToString" intToString,
        internalEntry "intToBool" intToBool,
        internalEntry "floatToString" floatToString
    ]

lenOfList :: Args -> Either String Val
lenOfList [] = Left "lenOfList: invalid number of arguments"
lenOfList [VList list] = Right $ VNum $ length list
lenOfList _ = Left "lenOfList: invalid argument"

intToFloat :: Args -> Either String Val
intToFloat [] = Left "intToFloat: invalid number of arguments"
intToFloat [VNum x] = Right $ VFloatNum $ fromIntegral x
intToFloat _ = Left "intToFloat: invalid argument type expected Int"

floatToInt :: Args -> Either String Val
floatToInt [] = Left "floatToInt: invalid number of arguments"
floatToInt [VFloatNum x] = Right $ VNum $ round x
floatToInt _ = Left "floatToInt: invalid argument type expected Int"

stringToInt :: Args -> Either String Val
stringToInt [] = Left "stringToInt: invalid number of arguments"
stringToInt [str@(VList (VChar _:_))] = case findInt (concatCharList str) of
    Just num -> Right $ VNum num
    Nothing -> Left $ "stringToInt: no integer found in '" ++ show str ++ "'"
    where
        concatCharList :: Val -> String
        concatCharList (VList charList) = concatMap extractChar charList
        concatCharList _ = ""

        extractChar :: Val -> String
        extractChar (VChar c) = [c]
        extractChar _ = ""

        findInt :: String -> Maybe Int
        findInt str' = readMaybe str'
stringToInt _ = Left "stringToInt: invalid argument type expected String"

intToString :: Args -> Either String Val
intToString [] = Left "intToString: invalid number of arguments"
intToString [VNum x] = Right $ VList $ map VChar $ show x
intToString _ = Left "intToString: invalid argument type expected Int"

intToBool :: Args -> Either String Val
intToBool [] = Left "intToBool: invalid number of arguments"
intToBool [VNum x] = Right $ VBool $ x /= 0
intToBool _ = Left "intToBool: invalid argument type expected Int"

floatToString :: Args -> Either String Val
floatToString [] = Left "floatToString: invalid number of arguments"
floatToString [VFloatNum x] = Right $ VList $ map VChar $ show x
floatToString _ = Left "floatToString: invalid argument type expected Float"
