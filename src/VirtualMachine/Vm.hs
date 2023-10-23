{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Vm
-}

module VirtualMachine.Vm (exec, Val(..), Operations(..), Instructions(..),
    Stack, Insts) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS


import Data.List (find)

data Val = VNum Int
    | VBool Bool
    | VStr String
    | VList [Val]
    | VOp Operations
    | VFunction Insts
    | VNil
    deriving (Show, Eq)

data Operations = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Less
    deriving (Show, Eq)

data Instructions = Push Val
    | JumpIfFalse Int
    | PushArg Int
    | PushEnv VmEnvKey
    | Call
    | Ret
    deriving (Show, Eq)

type Stack = [Val]
type Insts = [Instructions]
type Args = [Val]

data Compiled = Compiled Insts Args
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

instance Binary Operations where
    put Add = putWord8 0
    put Sub = putWord8 1
    put Mul = putWord8 2
    put Div = putWord8 3
    put Eq = putWord8 4
    put Less = putWord8 5

    get = do
        tag <- getWord8
        get' tag
        where
            get' 0 = return Add
            get' 1 = return Sub
            get' 2 = return Mul
            get' 3 = return Div
            get' 4 = return Eq
            get' 5 = return Less
            get' _ = fail "Invalid tag while deserializing Operations"

instance Binary Instructions where
    put (Push val) = putWord8 0 >> put val
    put (JumpIfFalse i) = putWord8 1 >> put i
    put (PushArg i) = putWord8 2 >> put i
    put (PushEnv key) = putWord8 3 >> put key
    put Call = putWord8 4
    put Ret = putWord8 5

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
            get' _ = fail "Invalid tag while deserializing Instructions"


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
    let instructions = [Push (VNum 5), Push (VNum 3), Push (VOp Add), Call, Ret]
    let args = []
    let compiled = Compiled instructions args

    serializeAndWriteCompiled "compiled.bin" compiled

    result <- readAndDeserializeCompiled "compiled.bin"
    case result of
        Left errMsg -> putStrLn $ "Deserialization Error: " ++ errMsg
        Right decodedCompiled@(Compiled inst args) -> print decodedCompiled
            >> case exec [] args inst [] of
                Left errMsg -> putStrLn $ "Execution Error: " ++ errMsg
                Right val -> print val

execOperation :: Operations -> Args -> Either String Val
execOperation Add (VNum a:VNum b:_) = Right (VNum (a + b))
execOperation Add _ = Left ("Add: invalid arguments")
execOperation Sub (VNum a:VNum b:_) = Right (VNum (a - b))
execOperation Sub _ = Left ("Sub: invalid arguments")
execOperation Mul (VNum a:VNum b:_) = Right (VNum (a * b))
execOperation Mul _ = Left ("Mul: invalid arguments")
execOperation Div (VNum 0:VNum _:_) = Left ("Div: division by zero")
execOperation Div (VNum a:VNum b:_) = Right (VNum (a `div` b))
execOperation Div _ = Left ("Div: invalid arguments")
execOperation Eq (a:b:_) = Right (VBool (a == b))
execOperation Eq _ = Left ("Eq: invalid number of arguments")
execOperation Less (VNum a:VNum b:_) = Right (VBool (a < b))
execOperation Less _ = Left ("Less: invalid arguments")

execCall :: VmEnv -> Args -> Val -> Either String Val
execCall env args (VFunction insts) = exec env args insts []
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

exec :: VmEnv -> Args -> Insts -> Stack -> Either String Val
exec env args ((PushEnv envKey):xs) stack = case vmEnvLookup env envKey of
    Just value -> exec env args xs (value : stack)
    Nothing -> Left $ "PushEnv: missing value in env"
exec env args ((Push value):xs) stack = exec env args xs (value : stack)
exec _ _ (Call:_) [] = Left $ "Call: missing value on stack"
exec env args (Call:xs) (call:stack) = execCall env stack call
    >>= exec env args xs . (: stack)
exec _ _ ((Ret):_) (value:_) = Right $ value
exec _ _ ((Ret):_) _ = Left $ "Ret: missing value on stack"
exec env args ((JumpIfFalse _):xs) (VBool True:stack) = exec env args xs stack
exec env args ((JumpIfFalse shift):xs) (VBool False:stack) =
    exec env args (drop shift xs) stack
exec _ _ ((JumpIfFalse _):_) _ = Left $ "JumpIfFalse: missing or invalid value on stack"
exec env args ((PushArg i):xs) stack = getIndexEither i args
    "PushArg: invalid index" >>= exec env args xs . (: stack)
exec _ _ [] _ = Left $ "Missing return instruction"
