module AST where 


import Data.HashMap.Strict as HM

-- Security Class
data C = Low | High

type Var = String
data AExpr = N Integer | X Var  | Plus AExpr AExpr | Mult AExpr AExpr | Minus AExpr AExpr -- | Div AExpr AExpr
data BExpr = T | And BExpr BExpr | Or BExpr BExpr | Neg BExpr | Lt AExpr AExpr | Leq AExpr AExpr| Eq AExpr AExpr


data Stmt = Assign Var AExpr | Input Var Var | Output AExpr Var | IfThenElse BExpr Stmt Stmt | While BExpr Stmt | Seq Stmt Stmt

type Memory = HashMap Var Integer

-- define member function for AST, and check whether all variables are in memory 
-- if not fail otherwise allow semantic functions

-- For now wack modified lookup 
safelookup :: Var -> Memory -> Integer 
safelookup v mem = case HM.lookup v mem of
  Just i -> i 
  Nothing -> 0

semA :: AExpr -> Memory -> Integer
semA expr mem = case expr of 
  N i -> i  
  X v -> safelookup v mem 
  Plus x y -> (semA x mem) + (semA y mem)
  Mult x y -> (semA x mem) * (semA y mem)
  Minus x y -> (semA x mem) - (semA y mem) 
 

semB :: BExpr -> Memory -> Bool
semB expr mem = case expr of
  T -> True 
  And x y -> (semB x mem) && (semB y mem) 
  Or x y -> (semB x mem) || (semB y mem) 
  Neg x -> not (semB x mem) 
  Lt x y -> (semA x mem) < (semA y mem)
  Leq x y -> (semA x mem) <= (semA y mem)
  Eq x y -> (semA x mem) == (semA y mem)

sem :: Stmt -> Memory -> Memory 
sem statement mem = case statement of 
    Assign v aexpr -> HM.insert v (semA aexpr mem) mem 
    Input v1 v2 -> HM.insert v2 (safelookup v1 mem) mem 
    Output aexpr v ->  HM.insert v (semA aexpr mem) mem
    IfThenElse bexpr s1 s2 -> if
      (semB bexpr mem) == True then 
        sem s1 mem 
      else sem s2 mem  
    While bexpr s -> if 
      (semB bexpr mem == True) then 
        let mem2 = sem s mem in sem (While bexpr s) mem2 
        else mem 
    Seq s1 s2 -> sem s2 (sem s1 mem) 


-- check addition and assign
check1 = Assign ("a" :: Var) (Plus (N 2) (N 3))
memory1 = HM.fromList [("a",2)] 

-- check simple counting while
emptymem = HM.empty 
check2 = Seq (Assign ("a" :: Var) (N 0)) (While (Lt (X "a") (N 3)) (Assign ("a" :: Var) (Plus (X "a") (N 1))))


