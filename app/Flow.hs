
module Flow where 


-- Get a security classification of all variables
-- This could for example be parsed from JSON
-- Make a function which can look up a security classification given a lattice
-- Alternatively if a variable is in the code but has no classification assign it the lowest 
-- There should be a function that given a set of security classifications and a lattice
-- finds the lowest or least restrictive classification. The supremum of the set. 
-- 

import Data.HashMap.Strict as HM
import AST as AST
import Algebra.Lattice as LA 
import Data.Set (Set)
import qualified Data.Set as Set


-- Wanted to implement lattice type, s.t. the function is polymorphic over different lattices. 

weakLookup :: (BoundedLattice a) => String -> HashMap String a -> a 
weakLookup key labels = case HM.lookup key labels of 
  Nothing -> bottom 
  Just l -> l

flowAExpr :: (BoundedLattice a) => AExpr -> HashMap String a -> a 
flowAExpr expr labels = case expr of 
  AConst i -> bottom 
  Var v -> weakLookup v labels 
  Neg expr -> flowAExpr expr labels 
  ABinop op expr1 expr2 -> (flowAExpr expr1 labels) \/ (flowAExpr expr2 labels) 



flowBExpr :: (BoundedLattice a) => BExpr -> HashMap String a -> a 
flowBExpr expr labels = case expr of 
  BConst b -> bottom
  Not b -> flowBExpr b labels
  BBinop op expr1 expr2 -> (flowBExpr expr1 labels) \/ (flowBExpr expr2 labels)
  RBinop op expr1 expr2 -> (flowAExpr expr1 labels) \/ (flowAExpr expr2 labels)


flow :: (BoundedLattice a) => [Stmt] -> HashMap String a -> a
flow stmtls labels = if length stmtls == 1 then [reqs] else
                  reqs : (flow (tail stmtls) labels)
                  where reqs = flowST (head stmtls) labels 

-- Collecting a set of constraints of the form (Security class, Constraint) s.t. (Security class <= constraint) can be evaluated. 
flowST :: (BoundedLattice a) => Stmt -> HashMap String a -> [(a,a)] -> a
flowST statement labels = case statement of 
    If bexpr s1 s2 -> let sc = (flowST s1 labels) /\ (flowST s2 labels) in ((flowBExpr bexpr labels), sc)
    Assign v aexpr -> (flowAExpr aexpr labels, weakLookup v)  
    While bexpr s -> let sc = flowST s labels in (flowBExpr bexpr labels, sc)
    Seq stmtls -> head $ flow stmtls labels

-- The above functions need to have return type a, and just carry the requirements along. 
