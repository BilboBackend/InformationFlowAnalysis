module Semantics where 

import Data.HashMap.Strict as HM
import AST as AST
import Parser as Parser

import Text.Parsec 
import Text.Parsec.String

-- define member function for AST, and check whether all variables are in memory 
-- if not fail otherwise allow semantic functions

-- For now wack modified lookup 
safelookup :: String -> Memory -> Integer 
safelookup v mem = case HM.lookup v mem of
  Just i -> i 
  Nothing -> 0

semAExpr :: AExpr -> Memory -> Integer 
semAExpr expr mem = case expr of 
  AConst i -> i 
  Var v -> safelookup v mem
  Neg exp -> - semAExpr exp mem 
  ABinop op exp1 exp2 -> semABinop op exp1 exp2 mem

semABinop :: ABinop -> AExpr -> AExpr -> Memory -> Integer 
semABinop op expr1 expr2 mem = case op of
  Add -> (semAExpr expr1 mem) + (semAExpr expr2 mem)
  Sub -> (semAExpr expr1 mem) - (semAExpr expr2 mem)
  Mul -> (semAExpr expr1 mem) * (semAExpr expr1 mem)


semBExpr :: BExpr -> Memory -> Bool 
semBExpr expr mem = case expr of 
  BConst b -> b 
  Not b -> not (semBExpr b mem)
  BBinop op expr1 expr2 -> semBBinop op expr1 expr2 mem 
  RBinop op expr1 expr2 -> semRBinop op expr1 expr2 mem 

semBBinop :: BBinop -> BExpr -> BExpr -> Memory -> Bool
semBBinop op expr1 expr2 mem = case op of
  Or  -> (semBExpr expr1 mem) || (semBExpr expr2 mem) 
  And -> (semBExpr expr1 mem) && (semBExpr expr2 mem) 


semRBinop :: RBinop -> AExpr -> AExpr -> Memory -> Bool 
semRBinop op expr1 expr2 mem = case op of 
  Lt  -> aexpr1 < aexpr2
  Leq -> aexpr1 <= aexpr2 
  Eq  -> aexpr1 == aexpr2 
  Neq ->  aexpr1 /= aexpr2
  Gt  -> aexpr1 > aexpr2  
  Geq -> aexpr1 >= aexpr2 
  where aexpr1 = (semAExpr expr1 mem) 
        aexpr2 = (semAExpr expr2 mem) 


sem :: [Stmt] -> Memory -> Memory 
sem stmtls mem = if length stmtls == 1 then nxtmem else
                  sem (tail stmtls) nxtmem 
                  where nxtmem = semST (head stmtls) mem

semST :: Stmt -> Memory -> Memory 
semST statement mem =  case statement of 
    If bexpr s1 s2 -> if
      (semBExpr bexpr mem) == True then 
        semST s1 mem 
      else semST s2 mem
    Assign v aexpr -> HM.insert v (semAExpr aexpr mem) mem 
    While bexpr s -> if 
      (semBExpr bexpr mem == True) then 
        let mem2 = semST s mem in semST (While bexpr s) mem2 
        else mem 
    Seq stmtls -> sem stmtls mem

-- check addition and assign
-- check1 = Assign ("a" :: Var) (Plus (N 2) (N 3))
-- memory1 = HM.fromList [("a",2)] 

-- check simple counting while
--emptymem = HM.empty 
-- check2 = Seq (Assign ("a" :: Var) (N 0)) (While (Lt (X "a") (N 3)) (Assign ("a" :: Var) (Plus (X "a") (N 1))))

--
-- eitherAExpr :: Either ParseError AExpr -> String
-- eitherAExpr ae = case ae of 
--   Right a -> show $ executeAE a
--   Left e -> show e
-- --
-- eitherStmt :: Either ParseError Stmt -> String 
-- eitherStmt se = case se of 
--   Right s -> show $ executeCode 
--   Left e -> show e
