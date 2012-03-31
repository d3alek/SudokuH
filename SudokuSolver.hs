module SudokuSolver where

import Data.List
import Data.Maybe
import CSPframework

-------------------------------------------------
-- Constraint definitions
-------------------------------------------------

-- N-ary constraint stating that all variables in a list must have have distinct values.
all_diff_constraint :: [Var] -> Constraint
all_diff_constraint vs = CT ("All_Diff: " ++ (show vs),vs,all_diff)

-- Relation that ensures a list of variables are all different.
-- Ignores unassigned variables.
all_diff :: Relation
all_diff vs a = length l == length (nub l) 
  where l = filter isJust $ map (lookup_var a) vs

-------------------------------------------------
-- CSP definitions
-------------------------------------------------

sudoku_vars :: [Var]
sudoku_vars = foldl union [] $ map (\x -> map (\y -> (++) x $ show y) [1..9]) ["a","b","c","d","e","f","g","h","i"]

sudoku_domains :: Domains
sudoku_domains = map (\r -> (r,[1..9])) sudoku_vars

sudoku_csp :: CSP
sudoku_csp = CSP ("Sudoku!",
           sudoku_domains, [
             (all_diff_constraint $ map (\y -> (++) "a" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "b" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "c" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "d" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "e" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "f" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "g" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "h" $ show y) [1..9]),
             (all_diff_constraint $ map (\y -> (++) "i" $ show y) [1..9]),
             
             (all_diff_constraint ["a1","b1","c1","d1","e1","f1","g1","h1","i1"]),
             (all_diff_constraint ["a2","b2","c2","d2","e2","f2","g2","h2","i2"]),
             (all_diff_constraint ["a3","b3","c3","d3","e3","f3","g3","h3","i3"]),
             (all_diff_constraint ["a4","b4","c4","d4","e4","f4","g4","h4","i4"]),
             (all_diff_constraint ["a5","b5","c5","d5","e5","f5","g5","h5","i5"]),
             (all_diff_constraint ["a6","b6","c6","d6","e6","f6","g6","h6","i6"]),
             (all_diff_constraint ["a7","b7","c7","d7","e7","f7","g7","h7","i7"]),
             (all_diff_constraint ["a8","b8","c8","d8","e8","f8","g8","h8","i8"]),
             (all_diff_constraint ["a9","b9","c9","d9","e9","f9","g9","h9","i9"]),
             
             (all_diff_constraint ["a1","a2","a3","b1","b2","b3","c1","c2","c3"]),
             (all_diff_constraint ["a4","a5","a6","b4","b5","b6","c4","c5","c6"]),
             (all_diff_constraint ["a7","a8","a9","b7","b8","b9","c7","c8","c9"]),
             (all_diff_constraint ["d1","d2","d3","e1","e2","e3","f1","f2","f3"]),
             (all_diff_constraint ["d4","d5","d6","e4","e5","e6","f4","f5","f6"]),
             (all_diff_constraint ["d7","d8","d9","e7","e8","e9","f7","f8","f9"]),
             (all_diff_constraint ["g1","g2","g3","h1","h2","h3","i1","i2","i3"]),
             (all_diff_constraint ["g4","g5","g6","h4","h5","h6","i4","i5","i6"]),
             (all_diff_constraint ["g7","g8","g9","h7","h8","h9","i7","i8","i9"])
             ])

sudoku vals = foldl (\c (x,y) -> set_domain c x [y]) sudoku_csp vals

-------------------------------------------------
-- Sample Sudoku CSP
-------------------------------------------------
sudoku_sample :: CSP
sudoku_sample = sudoku [("a1",3),("a3",6),("a4",8),
						("b1",1),("b3",9),("b6",5),
						("c5",7),("c8",2),
						("d1",4),("d4",7),("d9",1),
						("e1",9),("e9",7),
						("f1",6),("f6",8),("f9",5),
						("g2",4),("g5",8),
						("h4",2),("h7",1),("h9",6),
						("i6",1),("i7",8),("i9",3)
						]

-------------------------------------------------
-- Helper functions
-------------------------------------------------

-- (Helper fuction #1) A filter function which takes an assignment and a list of 
-- variables and returns a list of the unassigned variables 
filter_assigned_vars :: Assignment -> [Var] -> [Var]
filter_assigned_vars assignment = filter (is_unassigned assignment)

-- (Helper Function #2) Find the length of the domain of a variable
domain_length :: CSP -> Var -> Int
domain_length csp = length . domain_of csp

-- (Helper Function #3) Returns a list of all the arcs from neighbours of var to var
all_arcs_to :: CSP -> Var -> [(Var, Var)]
all_arcs_to csp var = map (\n -> (n, var)) $ all_neighbours_of csp var

-------------------------------------------------
-- Minimum Remaining Values (MRV) Heuristic
-------------------------------------------------

-- Sorting function for variables based on the MRV heuristic:

mrv_compare :: CSP -> Var -> Var -> Ordering
mrv_compare csp varA varB 
	| length_domainA > length_domainB = GT
	| length_domainA < length_domainB = LT
	| otherwise = EQ
	where 	length_domainA = domain_length csp varA
	      	length_domainB = domain_length csp varB

-- Get next variable according to MRV for the FC algorithm:

get_mrv_variable :: CSP -> Assignment -> Var
get_mrv_variable csp assignment = head_of_mrv_sorted unassigned_vars
	where
		-- extracts the head of an MRV sorted list	
		head_of_mrv_sorted = head . sortBy (mrv_compare csp)

		-- a list of unassigned variables in the CSP. Uses Helper fuction #1
		unassigned_vars = filter_assigned_vars assignment (vars_of csp)

-------------------------------------------------
-- Arc Consistency
-------------------------------------------------


-- Checks if there exists at least one value in a list of values that if 
-- assigned to the given variable the assignment will be consistent.

exists_consistent_value :: CSP -> Var -> Int -> Var -> [Int] -> Bool
exists_consistent_value csp varX valX varY domainY = or $ map is_consistent domainY
	where   
		-- A list of the common constraints of X and Y
		xy_constraints = common_constraints csp varX varY
		-- A boolean function which returns whether the value for Y satisfies the common constraints
		is_consistent valY = check_constraints xy_constraints $ assignment_xy valY
		assignment_xy valY = assign assignment_x varY valY
		assignment_x = assign [] varX valX


-- AC-3 constraint propagation

revise :: CSP -> Var -> Var -> (CSP,Bool)
revise csp varX varY = (set_domain csp varX new_domainX, domain_of csp varX /= new_domainX)
	where 	
		check_consistent valX = exists_consistent_value csp varX valX varY domainY
		domainY = domain_of csp varY
		-- a revised domain for X with the inconsistent values filtered out
		new_domainX = filter check_consistent (domain_of csp varX)


-- AC-3 constraint propagation

ac3_check :: CSP -> [(Var,Var)] -> (CSP,Bool)
ac3_check csp [] = (csp, True)
ac3_check csp ((varX, varY):vars)
	-- uses Helper functions #2, #3

	| rev_happened && domain_length rev_csp varX /= 0 && vars == [] 
		= ac3_check rev_csp $ all_arcs_to csp varX

	| rev_happened && domain_length rev_csp varX /= 0 
		= ac3_check rev_csp $ concat [vars, all_arcs_to csp varX]

	| rev_happened && domain_length rev_csp varX == 0 
		= (rev_csp, False)

	| otherwise 
		= ac3_check csp vars

	where (rev_csp, rev_happened) = revise csp varX varY

-- MAC+MRV Algorithm

mac_mrv_recursion :: Assignment -> CSP -> (Maybe Assignment, Int)
mac_mrv_recursion assignment csp =
	if (is_complete csp assignment) then (Just assignment,0)
	else find_consistent_value $ domain_of csp var
		where 
			var = get_mrv_variable csp assignment --MRV heuristic
			find_consistent_value vals = 
				case vals of  	-- recursion over the possible values 
                            	-- instead of for-each loop
				[]     -> (Nothing,0)
				val:vs ->
		  			-- check if AC-3 found any inconsistencies
					if (ac3_consistent) 
					then if (isNothing result) 
						then (ret,nodes+nodes'+1)
						else (result,nodes+1)
					else (ret,nodes'+1)
					where 
						modif_csp = set_domain csp var [val]
						(result,nodes) = mac_mrv_recursion (assign assignment var val) ac3_csp
						(ret,nodes') = find_consistent_value vs
						(ac3_csp, ac3_consistent) = ac3_check modif_csp all_affected_arcs
						-- run the AC-3 constraint propagation only on the affected arcs, 
						-- i.e. the ones from the neighbours of var to var
						-- uses Helper Function #3
						all_affected_arcs = all_arcs_to csp var

mac_mrv :: CSP -> (Maybe Assignment,Int)
mac_mrv csp = mac_mrv_recursion [] csp 

-- A function called from the GUI module
-- Takes a list of (name, digit) tuples. Digit is either set (1-9) or unset (-1).
-- Returns a list of (name, digit*) tuples, where digit* is calculated by the
-- MAC+MRV algorithm. Returns an empty list if no solution is found.

solveCSP :: [(String, Int)] -> [(String, Int)]
solveCSP vals 
	| isNothing solutionJ = []
	| otherwise = assignmentVals
	where
		filteredVals = filter (\(n,v) -> v /= -1) vals
		csp = sudoku filteredVals
		(solutionJ, steps) = mac_mrv csp
		solution = fromJust solutionJ
		updateValue (name, value) = (name, fromJust (lookup_var solution name))
		assignmentVals = map updateValue vals
