-- module Flow where
-- class MyersLiskov a where 
import Algebra.Lattice as LA 
import Data.HashMap.Strict as HM
import Data.Set (Set)

import AST as AST

import qualified Data.Set as Set


data Sec = Low | High 

-- instance Lattice Sec  where 
--   (\/) Low High = High
--   (\/) High Low = High
--   (/\) Low High = Low 
--   (/\) High Low = Low   
--

-- Wanted to implement lattice type, s.t. the function is polymorphic over different lattices. 
sup :: (Lattice a) => a -> a -> a 
sup l1 l2 = l1 \/ l2

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


-- Set.isSubsetOf l1owners l2owners where
--                      l1owners = Set.fromList (HM.keys l1) 
--                      l2owners = Set.fromList (HM.keys l2) 


-- ownerSubset :: Label -> Label -> Bool
-- ownerSubset l1 l2 = Set.isSubsetOf l1owners l2owners where
--                     l1owners = Set.fromList (HM.keys l1) 
--                     l2owners = Set.fromList (HM.keys l2) 
--
-- readersSupset :: Label -> Label -> Bool
-- readersSupset l1 l2 = all (\owner -> Set.isSubsetOf (readers l2 owner) (readers l1 owner)) l1owners 
--   where l1owners = Set.fromList (HM.keys l1) 
--
-- l1_lt_l2 :: Label -> Label -> Bool
-- l1_lt_l2 l1 l2 = (ownerSubset l1 l2) && (readersSupset l1 l2) 
--
-- join :: Label -> Label -> Label 
-- join l1 l2 = HM.unionWith Set.intersection l1 l2 
--
--
