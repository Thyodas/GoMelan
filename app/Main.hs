module Main (main) where

import Ast (printTree, SExpr(..))

main :: IO ()
main = case printTree (Number 3) of
    Just str -> putStrLn(str)
    Nothing -> putStrLn("Error")
