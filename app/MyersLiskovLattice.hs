
module MyersLiskovLattice where 

import Data.HashMap.Strict as HM
import Algebra.Lattice as LAT
import Data.Set (Set)
import qualified Data.Set as Set

type Owner = String
type Reader = String 

type Readers = Set Reader 

type Label = HashMap Owner Readers 

-- instance BoundedLattice Label where 
--   (\/) 
--
readers_test1 = Set.fromList ["Niels","Bjarke","Kim","Lars","Xuan"]
readers_test2 = Set.fromList ["Niels","Lars","Kim"]
readers_test3 = Set.fromList ["Niels","Lars","Kim","Johnny","Renee"]

label_test1 = HM.fromList [("Kurt" ,readers_test2)]
label_test2 = HM.fromList [("Kurt",readers_test3), ("Jannie",readers_test1)]

readers :: Label -> Owner -> Readers 
readers l o = case HM.lookup o l of 
                Just s -> s 
                Nothing -> Set.empty

allReaders :: Label -> Readers 
allReaders l = HM.foldl Set.union Set.empty l 

effectiveReaders :: Label -> Readers 
effectiveReaders l = HM.foldl Set.intersection (allReaders l) l

ownerSubset :: Label -> Label -> Bool
ownerSubset l1 l2 = Set.isSubsetOf l1owners l2owners where
                    l1owners = Set.fromList (HM.keys l1) 
                    l2owners = Set.fromList (HM.keys l2) 

readersSupset :: Label -> Label -> Bool
readersSupset l1 l2 = all (\owner -> Set.isSubsetOf (readers l2 owner) (readers l1 owner)) l1owners 
  where l1owners = Set.fromList (HM.keys l1) 

l1_lt_l2 :: Label -> Label -> Bool
l1_lt_l2 l1 l2 = (ownerSubset l1 l2) && (readersSupset l1 l2) 

join :: Label -> Label -> Label 
join l1 l2 = HM.unionWith Set.intersection l1 l2 


