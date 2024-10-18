
module Flow where 



-- Get a security classification of all variables
-- This could for example be parsed from JSON
-- Make a function which can look up a security classification given a lattice
-- Alternatively if a variable is in the code but has no classification assign it the lowest 
-- There should be a function that given a set of security classifications and a lattice
-- finds the lowest or least restrictive classification. The supremum of the set. 
-- 
data Sec


flowAexpr :: AExpr -> 
