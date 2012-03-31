module CSPframework where

import Data.List
import Data.Maybe

{-
A framework for simple CSP solving.

- Supports named variables, integer values and n-ary constraints.

Petros Papapanagiotou
School of Informatics
University of Edinburgh
2012

- Use the Reference Manual provided for helpful information on 
the datatypes and functions provided.

-}


-- *******************************
-- ** Variables and Assignments **
-- *******************************

-- Named variables as Strings
type Var = String




-- Single variable assignment
newtype AssignedVar = AV (Var,Int)

-- Function to prettyprint single variable assignments.
instance Show AssignedVar where
  show (AV (n,v)) = n ++ "=" ++ (show v)

-- Function to check equality of single variable assignments.
-- Defined as equality of the variable names.
instance Eq AssignedVar where
  (AV (i,_)) == (AV (j,_)) = i == j

-- Retrieve the variable name from a single variable assignment.
var_name :: AssignedVar -> String
var_name (AV (x,_)) = x

-- Retrieve the value of a single variable assignment.
var_value :: AssignedVar -> Int
var_value (AV (_,x)) = x
  


                       
-- Multiple variable assignments.
type Assignment = [AssignedVar] 

-- Retrieves the value of one of the variables in an assignment.
-- Returns Nothing if the variable is unassigned.
-- If multiple assignments of the same variable have been erroneously added, only the first one is returned.
lookup_var :: Assignment -> Var -> Maybe Int
lookup_var a x =  
  case (af) of 
    []     -> Nothing 
    av:_   -> Just (var_value av)
    where af = filter (== AV (x,0)) a
    
-- Checks whether a variable has not been assigned a value yet.
is_unassigned :: Assignment -> Var -> Bool
is_unassigned a x = isNothing (lookup_var a x)

-- Assigns a value to a variable. 
-- Replaces old value if variable was already assigned.
assign :: Assignment -> Var -> Int -> Assignment
assign a v i = x : (delete x a)
  where x = AV (v,i)
    
-- Checks if two values returned from lookup_var are equal. 
-- Returns False if either of them is Nothing.
vals_eq :: Maybe Int -> Maybe Int -> Bool
vals_eq x y 
        | isNothing x = False
        | isNothing y = False
        | otherwise = x == y



-- *******************************
-- ** Relations and Constraints **
-- *******************************

-- A Relation is defined as a function (instead of a set of acceptable tuples).
-- It is given a list of variables (scope) for the constraint and a (partial) variable assignment (tuple).
-- It returns whether the assignment (tuple) is acceptable.
type Relation = [Var] -> Assignment -> Bool

-- Instantiated constraint for a particular CSP.
-- Consists of a string for prettyprinting, the scope (list of variables), and the relation.
newtype Constraint = CT (String,[Var],Relation)

-- Prettyprinting function for Constraints.
instance Show Constraint where
  show (CT (s,_,_)) = s

-- Prettyprinting function for a list of constraints.
-- Prints every constraint on a separate line.
show_constraints :: [Constraint] -> String
show_constraints [] = ""
show_constraints (c:cs) = (show c) ++ "\n" ++ (show_constraints cs)

-- Consistency check for a single constraint on a given assignment.
check_constraint :: Constraint -> Assignment -> Bool
check_constraint (CT (_,vars,rel)) a = rel vars a
  
-- Consistency check for a list of constraints on a given assignment.
check_constraints :: [Constraint] -> Assignment -> Bool
check_constraints cs a = all (\c -> check_constraint c a) cs

-- Returns the scope of a constraint.
scope :: Constraint -> [Var]
scope (CT (_,vars,_)) = vars

-- Returns whether a given variable is in the scope of a constraint.
is_constrained :: Constraint -> Var -> Bool  
is_constrained c v = elem v $ scope c

-- Returns the neighbours of a particular variable in a given constraint.
-- Returns an empty list if the variable is not constrained.
neighbours_of :: Constraint -> Var -> [Var]                                             
neighbours_of c v = if (is_constrained c v) then (delete v $ scope c) else []



-- *************
-- ** Domains **
-- *************


-- Variable domains are lists of possible integers.
type Domain = [Int]

-- Adds a single value to a domain.
domain_add :: Int -> Domain -> Domain
domain_add i d = i:d

-- Deletes a single value from a domain.
domain_del :: Int -> Domain -> Domain
domain_del i d = delete i d


-- A list of variables attached to their respective domains.
type Domains = [(Var,Domain)]

-- Prettyprinting function for Domains list.
show_domains :: Domains -> String
show_domains [] = ""
show_domains ((v,d):ds) = v ++ " @ " ++ (show d) ++ "\n" ++ (show_domains ds)



-- *********************************************
-- ** Constraint Satisfaction Problems (CSPs) **
-- *********************************************


-- CSPs consist of a name, an (initial) Domains list, and a list of Constraints.
newtype CSP = CSP (String,Domains,[Constraint])

-- Prettyprinting function for CSPs.
instance Show CSP where
  show (CSP (s,d,c)) = "CSP: " ++ s 
                       ++ "\n\nDomains:\n--------\n" ++ (show_domains d) 
                       ++ "\n\nConstraints:\n------------\n" ++ (show_constraints c)
                       

-----------------------------
-- Basic getter functions. --
-----------------------------

-- Retrieve the list of variables defined in a CSP.  
vars_of :: CSP -> [Var]
vars_of (CSP (_,d,_)) = map fst d

-- Retrieve the Domains list of a CSP.
domains :: CSP -> Domains
domains (CSP (_,d,_)) = d

-- Retrieve the list of constraints of a CSP.
constraints :: CSP -> [Constraint]    
constraints (CSP (_,_,cs)) = cs


-----------------------
-- Domain functions. --
-----------------------

-- Set a new Domains list for the CSP.
set_domains :: CSP -> Domains -> CSP
set_domains (CSP (s,_,cs)) d' = (CSP (s,d',cs))

-- Retrieves the domain for a particular variable in a CSP.
domain_of :: CSP -> Var -> Domain
domain_of csp v
  | isJust r = fromJust r
  | otherwise = []
    where r = lookup v $ domains csp

-- Updates the domain of a given variable in a Domains list by applying a function to it.
update_domain :: Domains -> Var -> (Domain -> Domain) -> Domains
update_domain [] _ _ = []
update_domain ((v,vals):dm) var f 
             | v == var = (var,f vals):dm 
             | otherwise = (v,vals):(update_domain dm var f)

-- Updates the domain of a given variable in a CSP by applying a function to it.
update_domains :: CSP -> Var -> (Domain -> Domain) -> CSP
update_domains csp v f = set_domains csp $ (\d -> update_domain d v f) $ domains csp

-- Sets a new domain for a variable in a CSP.
set_domain :: CSP -> Var -> Domain -> CSP                                                 
set_domain csp v dom = update_domains csp v (\d -> dom)
                                                 
-- Adds a value to the domain of a variable in a CSP.
add_domain_val :: CSP -> Var -> Int -> CSP
add_domain_val csp v i = update_domains csp v (domain_add i)

-- Deletes a value from the domain of a variable in a CSP.
del_domain_val :: CSP -> Var -> Int -> CSP
del_domain_val csp v i = update_domains csp v (domain_del i)



-------------------------
-- Variable functions. --
-------------------------

-- Returns the first variable from a CSP that is unassigned in a given partial assignment.
get_unassigned_var :: CSP -> Assignment -> Var
get_unassigned_var csp assignment = head $ filter (is_unassigned assignment) $ vars_of csp 
                                    
-- Returns the list of constraints that constrain a given variable in a CSP.
constraints_of :: CSP -> Var -> [Constraint]
constraints_of csp v = filter (\c -> is_constrained c v) $ constraints csp

-- Returns a list of all neighbours from all constraints for a given variable in a CSP.
all_neighbours_of :: CSP -> Var -> [Var]                                             
all_neighbours_of csp v = foldr (++) [] $ map (\c -> neighbours_of c v) $ constraints csp
                                    
-- Returns a list of constraints that involve both of two given variables.
common_constraints :: CSP -> Var -> Var -> [Constraint]                          
common_constraints csp a b = filter (\c -> elem a $ scope c) $ constraints_of csp b


---------------------------
-- Assignment functions. --
---------------------------

-- Checks if a given assignment is complete for a CSP.
is_complete :: CSP -> Assignment -> Bool
is_complete csp assignment = all (\v -> not $ is_unassigned assignment v) $ vars_of csp

-- Checks if an assignment is consistent with respect to the constraints of a CSP.
is_consistent :: CSP -> Assignment -> Bool
is_consistent csp assignment = all (\c -> check_constraint c assignment) $ constraints csp

-- Checks if a suggested value for a variable is consistent w.r.t a partial assignment and a CSP.
is_consistent_value :: CSP -> Assignment -> Var -> Int -> Bool
is_consistent_value csp assignment var val = is_consistent csp $ assign assignment var val
