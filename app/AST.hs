module AST where 


import Data.HashMap.Strict as HM

-- Security Class
data C = Low | High

type Var = String

data AExpr = Plus ATerm AExpr | Minus ATerm AExpr | Term ATerm deriving Show 
data ATerm = Mult AFact ATerm| Fact AFact deriving Show 
data AFact = Paren AExpr | Atom AAtom deriving Show 
data AAtom = N Integer | X Var deriving Show 

data BExpr = T | And BExpr BExpr | Or BExpr BExpr | Neg BExpr | Lt AExpr AExpr | Leq AExpr AExpr| Eq AExpr AExpr
  deriving Show

data Stmt = Assign Var AExpr | Input Var Var | Output AExpr Var | IfThenElse BExpr Stmt Stmt | While BExpr Stmt | Seq Stmt Stmt
  deriving Show

type Code = [Stmt]
type Memory = HashMap Var Integer

-- define member function for AST, and check whether all variables are in memory 
-- if not fail otherwise allow semantic functions

-- For now wack modified lookup 
safelookup :: Var -> Memory -> Integer 
safelookup v mem = case HM.lookup v mem of
  Just i -> i 
  Nothing -> 0

semAA :: AAtom -> Memory -> Integer 
semAA atom mem = case atom of 
  N i -> i 
  X v -> safelookup v mem

semAF :: AFact -> Memory -> Integer 
semAF fact mem = case fact of 
  Paren expr -> semAE expr mem
  Atom a -> semAA a mem

semAT :: ATerm -> Memory -> Integer
semAT term mem = case term of 
  Mult x y -> (semAF x mem)* (semAT y mem)
  Fact x -> semAF x mem

semAE :: AExpr -> Memory -> Integer
semAE expr mem = case expr of 
  Plus x y -> (semAT x mem) + (semAE y mem)
  Minus x y -> (semAT x mem) - (semAE y mem) 
  Term x -> (semAT x mem) 

executeAE :: AExpr -> Integer
executeAE expr = semAE expr HM.empty 
-- semB :: BExpr -> Memory -> Bool
-- semB expr mem = case expr of
--   T -> True 
--   And x y -> (semB x mem) && (semB y mem) 
--   Or x y -> (semB x mem) || (semB y mem) 
--   Neg x -> not (semB x mem) 
--   Lt x y -> (semA x mem) < (semA y mem)
--   Leq x y -> (semA x mem) <= (semA y mem)
--   Eq x y -> (semA x mem) == (semA y mem)
--
-- sem :: Stmt -> Memory -> Memory 
-- sem statement mem = case statement of 
--     Assign v aexpr -> HM.insert v (semA aexpr mem) mem 
--     Input v1 v2 -> HM.insert v2 (safelookup v1 mem) mem 
--     Output aexpr v ->  HM.insert v (semA aexpr mem) mem
--     IfThenElse bexpr s1 s2 -> if
--       (semB bexpr mem) == True then 
--         sem s1 mem 
--       else sem s2 mem  
--     While bexpr s -> if 
--       (semB bexpr mem == True) then 
--         let mem2 = sem s mem in sem (While bexpr s) mem2 
--         else mem 
--     Seq s1 s2 -> sem s2 (sem s1 mem) 
--
--
-- -- check addition and assign
-- check1 = Assign ("a" :: Var) (Plus (N 2) (N 3))
-- memory1 = HM.fromList [("a",2)] 
--
-- -- check simple counting while
-- emptymem = HM.empty 
-- check2 = Seq (Assign ("a" :: Var) (N 0)) (While (Lt (X "a") (N 3)) (Assign ("a" :: Var) (Plus (X "a") (N 1))))
--
--
