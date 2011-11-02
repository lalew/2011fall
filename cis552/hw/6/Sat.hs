{-# OPTIONS -Wall -fwarn-tabs #-} 
module Sat where

import Data.Map (Map)
import qualified Data.Map as Map


import Test.QuickCheck
import Test.HUnit

-- | An expression in CNF, the conjunction of clauses
newtype CNF = CNF [ Clause ] deriving (Eq, Ord, Show)
unCNF :: CNF -> [ Clause ]
unCNF (CNF cs) = cs

-- | A clause -- the disjunction of a number of literals
type Clause = [ Lit ]

-- | A literal, either a positive or negative variable
type Lit    = Int 

-- | invert the polarity of a literal
invert :: Lit -> Lit
invert a = negate a

example_formula :: CNF
example_formula = CNF [[1],[2,-1]]

example_assignment :: Map Lit Bool
example_assignment = Map.fromList [(1, True), (2, True)]

valid :: Map Lit Bool  -> Bool
valid m = Map.foldrWithKey (\ lit b1 val -> 
                              case Map.lookup (invert lit) m of 
                                 Just b2 -> b1 == not b2 && val
                                 Nothing -> True && val) True m

test :: Test
test =  valid 
   (Map.fromList ( [(-2,True),(-1,True),(2,True)])) ~?= False

interp :: (Map Lit Bool) -> CNF -> Bool
interp m (CNF f) = all (any (findLit m)) f where
  findLit m' k = 
     case (Map.lookup k m', Map.lookup (invert k) m') of
          (Just b1, Just b2) | b1 == not b2 -> b1
          (Just _, Just  _)  -> error "invalid map"
          (Just b, Nothing)  -> b
          (Nothing, Just b)  -> not b
          (Nothing, Nothing) -> True  

dpll :: CNF -> Maybe (Map Lit Bool)
dpll = undefined

-- | Given a list of literals, create the trivial assignment 
-- that satisfies that list (if one exists). 
satisfy :: Clause -> Maybe (Map Lit Bool)
satisfy = undefined

-- | If a propositional variable occurs with only one polarity in the
-- formula, it is called pure. Pure literals can always be assigned in
-- a way that makes all clauses containing them true. Thus, these
-- clauses do not constrain the search anymore and can be deleted. 
-- This function collects all pure literals from the formula and 
-- returns the assignment paired with the refactored formula 
-- that reflects that assignment.
pureLitAssign :: CNF -> (Map Lit Bool, CNF)
pureLitAssign = undefined

-- | If a clause is a unit clause, i.e. it contains only a single
-- unassigned literal, this clause can only be satisfied by assigning
-- the necessary value to make this literal true. This function collects
-- all unit clauses from the formula and returns the assignment paired 
-- with the refactored formula that reflects that assignment.
unitPropagate :: CNF -> (Map Lit Bool, CNF)
unitPropagate = undefined



prop_dpll :: CNF -> Property
prop_dpll c = 
  
  case dpll c of 
    Just m -> if valid m then
       (property (interp m c))
      else property False
    Nothing ->  (property True)  
    


