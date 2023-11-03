{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Vm
-}

module VirtualMachine.Vm (exec, Val(..), EnumOperator(..), Instructions(..),
    Stack, Insts, Compiled(..), VmEnv(..), main, execCall, execOperation,
    execHelper) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Safe (toEnumMay)


import Data.List (find)

data Val = VNum Int
    | VBool Bool
    | VStr String
    | VList [Val]
    | VOp EnumOperator
    | VFunction Insts
    | VNil
    deriving (Show, Eq)

data EnumOperator = SignPlus
    | SignMinus
    | SignMultiply
    | SignDivide
    | SignModulo
    | SignEqual
    | SignNotEqual
    | SignNot
    | SignAnd
    | SignInfEqual
    | SignSupEqual
    | SignInf
    | SignSup
    deriving (Show, Eq, Enum, Bounded)

data Instructions = Push Val
    | JumpIfFalse Int
    | Jump Int
    | PushArg Int
    | PushEnv VmEnvKey
    | Call
    | Ret
    deriving (Show, Eq)

type Stack = [Val]
type Insts = [Instructions]
type Args = [Val]

data Compiled = Compiled VmEnv Insts
                deriving (Show)


instance Binary Val where
    put (VNum num) = putWord8 0 >> put num
    put (VBool bool) = putWord8 1 >> put bool
    put (VStr str) = putWord8 2 >> put str
    put (VList list) = putWord8 3 >> put list
    put (VOp op) = putWord8 4 >> put op
    put (VFunction insts) = putWord8 5 >> put insts
    put VNil = putWord8 6

    get = do
        tag <- getWord8
        get' tag
        where
            get' 0 = VNum <$> get
            get' 1 = VBool <$> get
            get' 2 = VStr <$> get
            get' 3 = VList <$> get
            get' 4 = VOp <$> get
            get' 5 = VFunction <$> get
            get' 6 = return VNil
            get' _ = fail "Invalid tag while deserializing Val"

instance Binary EnumOperator where
    put op = putWord8 (fromIntegral $ fromEnum op)

    get = do
        tag <- getWord8
        get' tag
            where
                get' :: Word8 -> Get EnumOperator
                get' tag = case toEnumMay (fromIntegral tag) of
                    Just op -> return op
                    Nothing -> fail "Invalid tag while deserializing Operations"

instance Binary Instructions where
    put (Push val) = putWord8 0 >> put val
    put (JumpIfFalse i) = putWord8 1 >> put i
    put (PushArg i) = putWord8 2 >> put i
    put (PushEnv key) = putWord8 3 >> put key
    put Call = putWord8 4
    put Ret = putWord8 5
    put (Jump i) = putWord8 6 >> put i

    get = do
        tag <- getWord8
        get' tag
        where
            get' 0 = Push <$> get
            get' 1 = JumpIfFalse <$> get
            get' 2 = PushArg <$> get
            get' 3 = PushEnv <$> get
            get' 4 = return Call
            get' 5 = return Ret
            get' 6 = Jump <$> get
            get' _ = fail "Invalid tag while deserializing Instructions"

instance Eq Compiled where
    (Compiled env1 insts1) == (Compiled env2 insts2) = env1 == env2 && insts1 == insts2

instance Binary Compiled where
    put (Compiled insts args) = put insts >> put args
    get = Compiled <$> get <*> get

-- Serialize Compiled and write it to a file
serializeAndWriteCompiled :: FilePath -> Compiled -> IO ()
serializeAndWriteCompiled filePath compiled = do
    let encoded = encode compiled
    BS.writeFile filePath encoded

-- Deserialize Compiled from a file
readAndDeserializeCompiled :: FilePath -> IO (Either String Compiled)
readAndDeserializeCompiled filePath = do
    encoded <- BS.readFile filePath
    return $ case decodeOrFail encoded of
        Left (_, _, errMsg) -> Left errMsg
        Right (_, _, compiled) -> Right compiled

main :: IO ()
main = do
    let instructions = [Push (VBool True),JumpIfFalse 2,Push (VStr "then"),Jump 1,Push (VStr "else"), Ret]
    let env = []
    let compiled = Compiled env instructions

    serializeAndWriteCompiled "compiled.bin" compiled

    result <- readAndDeserializeCompiled "compiled.bin"
    case result of
        Left errMsg -> putStrLn $ "Deserialization Error: " ++ errMsg
        Right decodedCompiled@(Compiled newEnv inst) -> print decodedCompiled
            >> case exec newEnv [] inst [] of
                Left errMsg -> putStrLn $ "Execution Error: " ++ errMsg
                Right val -> print val

execOperation :: EnumOperator -> Args -> Either String Val
execOperation SignPlus (VNum a:VNum b:_) = Right (VNum (a + b))
execOperation SignPlus _ = Left ("Add: invalid arguments")
execOperation SignMinus (VNum a:VNum b:_) = Right (VNum (a - b))
execOperation SignMinus _ = Left ("Sub: invalid arguments")
execOperation SignMultiply (VNum a:VNum b:_) = Right (VNum (a * b))
execOperation SignMultiply _ = Left ("Mul: invalid arguments")
execOperation SignDivide (VNum 0:VNum _:_) = Left ("Div: division by zero")
execOperation SignDivide (VNum a:VNum b:_) = Right (VNum (a `div` b))
execOperation SignDivide _ = Left ("Div: invalid arguments")
execOperation SignEqual (a:b:_) = Right (VBool (a == b))
execOperation SignEqual _ = Left ("Eq: invalid number of arguments")
execOperation SignInf (VNum a:VNum b:_) = Right (VBool (a < b))
execOperation SignInf _ = Left ("Less: invalid arguments")
execOperation SignSup (VNum a:VNum b:_) = Right (VBool (a > b))
execOperation SignSup _ = Left ("Greater: invalid arguments")
execOperation SignInfEqual (VNum a:VNum b:_) = Right (VBool (a <= b))
execOperation SignInfEqual _ = Left ("LessEq: invalid arguments")
execOperation SignSupEqual (VNum a:VNum b:_) = Right (VBool (a >= b))
execOperation SignSupEqual _ = Left ("GreaterEq: invalid arguments")
execOperation SignAnd (VBool a:VBool b:_) = Right (VBool (a && b))
execOperation SignAnd _ = Left ("And: invalid arguments")
execOperation SignNot (VBool a:_) = Right (VBool (not a))
execOperation SignNot _ = Left ("Not: invalid arguments")
execOperation SignModulo (VNum 0:VNum _:_) = Left ("Mod: modulo by zero")
execOperation SignModulo (VNum a:VNum b:_) = Right (VNum (a `mod` b))
execOperation SignModulo _ = Left ("Mod: invalid arguments")
execOperation SignNotEqual (a:b:_) = Right (VBool (a /= b))
execOperation SignNotEqual _ = Left ("Neq: invalid number of arguments")

execCall :: VmEnv -> Args -> Val -> Either String Val
execCall env args (VFunction insts) = execHelper env args insts insts []
execCall _ args (VOp op) = execOperation op args
execCall _ _ _ = Left ("Call: invalid arguments")

getIndexEither :: Int -> [a] -> e -> Either e a
getIndexEither i list e | i >= length list || i < 0 = Left $ e
getIndexEither i list _ = Right $ list !! i

type VmEnv = [VmEnvEntry]
type VmEnvEntry = (VmEnvKey, VmEnvValue)
type VmEnvKey = String
type VmEnvValue = Val

-- | Check if element is in env
vmEnvLookup :: VmEnv -> VmEnvKey -> Maybe VmEnvValue
vmEnvLookup env key = find checkKey env >>= Just . snd
  where
    checkKey :: VmEnvEntry -> Bool
    checkKey (sym, _) = sym == key

-- | Execute instructions
exec :: VmEnv -> Args -> Insts -> Stack -> Either String Val
exec env args insts stack = execHelper env args insts insts stack

-- | Helper function, please use exec instead
execHelper :: VmEnv -> Args -> Insts -> Insts -> Stack -> Either String Val
execHelper env args allInsts ((PushEnv envKey):xs) stack = case vmEnvLookup env envKey of
    Just value -> execHelper env args allInsts xs (value : stack)
    Nothing -> Left $ "PushEnv: missing value in env"
execHelper env args allInsts ((Push value):xs) stack =
    execHelper env args allInsts xs (value : stack)
execHelper _ _ _ (Call:_) [] = Left $ "Call: missing value on stack"
execHelper env args allInsts (Call:xs) (call:stack) = execCall env stack call
    >>= execHelper env args allInsts xs . (: stack)
execHelper _ _ _ ((Ret):_) (value:_) = Right $ value
execHelper _ _ _ ((Ret):_) _ = Left $ "Ret: missing value on stack"

execHelper env args allInsts ((JumpIfFalse _):xs) (VBool True:stack) = execHelper env args allInsts xs stack
execHelper env args allInsts ((JumpIfFalse shift):xs) (VBool False:stack)
    | shift < 0 = execHelper env args allInsts (drop nbToDrop allInsts) stack
    | otherwise = execHelper env args allInsts (drop shift xs) stack
        where
            nbToDrop = (length allInsts - length xs + 1) + shift
execHelper env args allInsts ((JumpIfFalse _):xs) (VNum 0:stack) = execHelper env args allInsts xs stack
execHelper env args allInsts ((JumpIfFalse shift):xs) (VNum _:stack)
    | shift < 0 = execHelper env args allInsts (drop nbToDrop allInsts) stack
    | otherwise = execHelper env args allInsts (drop shift xs) stack
        where
            nbToDrop = (length allInsts - length xs + 1) + shift
execHelper _ _ _ ((JumpIfFalse _):_) _ = Left $ "JumpIfFalse: missing or invalid value on stack"

execHelper env args allInsts ((Jump shift):xs) stack
    | shift < 0 = execHelper env args allInsts (drop nbToDrop allInsts) stack
    | otherwise = execHelper env args allInsts (drop shift xs) stack
        where
            nbToDrop = (length allInsts - length xs + 1) + shift

execHelper env args allInsts ((PushArg i):xs) stack = getIndexEither i args
    "PushArg: invalid index" >>= execHelper env args allInsts xs . (: stack)
execHelper _ _ _ [] _ = Left $ "Missing return instruction"
