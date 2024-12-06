module Main where

import AST as AST
import Semantics as SEM
import Parser as PAR 
import Data.HashMap.Strict as HM
import Flow 

main :: IO ()
main = do
        putStrLn "Hello, input your program!"
        prog <- getLine 
        parsed <- return $ parseString prog 
        eval <- return $ semST parsed HM.empty
        putStrLn "Memory after running the program is as follows: "
        putStrLn $ showMem eval

