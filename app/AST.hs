module AST where

import Data.HashMap.Strict as HM

data AExpr = Var String 
  | AConst Integer 
  | Neg AExpr 
  | ABinop ABinop AExpr AExpr
  deriving Show 

data ABinop = Add | Sub | Mul | Div deriving Show 
  

data BExpr = BConst Bool 
  | Not BExpr 
  | BBinop BBinop BExpr BExpr
  | RBinop RBinop AExpr AExpr 
  deriving Show

data BBinop = Or | And
  deriving Show 

data RBinop = Leq 
  | Lt
  | Eq 
  | Neq
  | Gt
  | Geq 
  deriving Show

data Stmt = Seq [Stmt]
  | While BExpr Stmt 
  | If BExpr Stmt Stmt 
  | Assign String AExpr 
  deriving Show 

type Code = [Stmt]

type Memory = HashMap String Integer

--instance Show (HashMap k v) where  
showMem mem = Prelude.foldl (++) "" (Prelude.map (\(k,v) -> k ++ " := " ++ (show v) ++ "; ") (HM.toList mem))

--instance Show Memory where show (Memory mem) = Prelude.foldl (\(k,v) -> show k ++ " : " ++ show v) "" (HM.toList mem)


