module Main where

import AST as AST
import Semantics as SEM
import Parser as PAR 

main :: IO ()
main = do
        putStrLn "Hello, input your program!"
        prog <- getLine 
        putStrLn prog

