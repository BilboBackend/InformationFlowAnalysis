module Main where

import AST as AST
import Semantics as SEM
import Parser as PAR 
import Data.HashMap.Strict as HM

main :: IO ()
main = do
        putStrLn "Hello, input your program!"
        prog <- getLine 
        parsed <- return $ parseString prog 
        eval <- return $ semST parsed HM.empty
        putStrLn $ show eval

