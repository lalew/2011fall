-- Advanced Programming, HW 6
-- Christian DeLozier <delozier>, Zi Yan <yanzi>

{-# OPTIONS -Wall -fwarn-tabs #-} 
module Sat where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM,liftM2)

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
dpll cnf = let (a, b) = pureLitAssign cnf
               (c, d) = unitPropagate b
               e      = foldr step (Just Map.empty) (unCNF d)
           in case e of
                   Nothing -> Nothing
                   Just f  -> Just $ Map.unions [a, c, f]
 where step :: Clause -> Maybe (Map Lit Bool) -> Maybe (Map Lit Bool)
       step _ Nothing = Nothing
       step c mmap    = liftM2 (Map.union) mmap (satisfy c) 

-- | Given a list of literals, create the trivial assignment 
-- that satisfies that list (if one exists). 
satisfy :: Clause -> Maybe (Map Lit Bool)
satisfy = foldr step Nothing
          where step:: Lit -> Maybe (Map Lit Bool) -> Maybe (Map Lit Bool)
                step _ Nothing   = Nothing
                step l (Just ms) = 
                     case (Map.lookup l ms, Map.lookup (invert l) ms) of
                          (Just b1, Just b2) | b1 == not b2 -> Just ms
                          (Just _, Just _)   -> Nothing
                          (Just _, Nothing)  -> Just ms
                          (Nothing, Just _)  -> Just ms
                          (Nothing, Nothing) -> Just (Map.insert l True ms)

prop_satisfy :: [Lit] ->Property
prop_satisfy c =
      case satisfy cz of
         Just m -> if valid m then
            (property (interp m (CNF [cz])))
                   else property False
         Nothing -> property True
      where cz = filter (/=0) c

-- | If a propositional variable occurs with only one polarity in the
-- formula, it is called pure. Pure literals can always be assigned in
-- a way that makes all clauses containing them true. Thus, these
-- clauses do not constrain the search anymore and can be deleted. 
-- This function collects all pure literals from the formula and 
-- returns the assignment paired with the refactored formula 
-- that reflects that assignment.
pureLitAssign :: CNF -> (Map Lit Bool, CNF)
pureLitAssign (CNF [])     = (Map.empty, CNF []) 
pureLitAssign (CNF xs) = foldr step (Map.empty, CNF []) xs
 where step :: Clause -> (Map Lit Bool, CNF) -> (Map Lit Bool, CNF)
       step c (m, CNF rs) = let (a, b) = removeP c xs
                    in case b of
                            Nothing  -> (m, CNF (c:rs))
                            Just _ -> (Map.union a m, CNF (rs))

-- | Remove the clause if there is any Lit in it is
--  pure.
removeP :: Clause -> [Clause] -> (Map Lit Bool, Maybe Clause)
removeP _ []        = (Map.empty, Nothing)
removeP [] _        = (Map.empty, Nothing)
removeP c@(x:xs) cs = if (ifPure x cs)
                    then (Map.fromList [(x, True)], Just c)
                    else removeP xs cs

-- | If l::Lit 's inversion does not appear in 
--  any clause of the CNF
ifPure :: Lit -> [Clause] -> Bool
ifPure l cs = all (\c -> notElem (invert l) c) cs


-- | If a clause is a unit clause, i.e. it contains only a single
-- unassigned literal, this clause can only be satisfied by assigning
-- the necessary value to make this literal true. This function collects
-- all unit clauses from the formula and returns the assignment paired 
-- with the refactored formula that reflects that assignment.
unitPropagate :: CNF -> (Map Lit Bool, CNF)
unitPropagate (CNF []) = (Map.empty, CNF [])
unitPropagate (CNF ps) = aux (Map.empty, CNF ps)      
 where aux :: (Map Lit Bool, CNF) -> (Map Lit Bool, CNF)
       aux (a, CNF cs) = let us = collectU cs
                         in if (null us) then (a, CNF cs)
                            else aux (Map.union a (mapToTrue us), 
                                      CNF $ removeU us cs)

       mapToTrue :: [Lit] -> Map Lit Bool
       mapToTrue []     = Map.empty
       mapToTrue (x:xs) = Map.insert x True (mapToTrue xs)
       collectU :: [Clause] -> [Lit]
       collectU []       = []
       collectU ([x]:xs) = x:(collectU xs)
       collectU (_:xs)     = collectU xs
       removeU :: [Lit] -> [Clause] -> [Clause]
       removeU _ []      = []
       removeU us (x:xs) | any (\a -> elem a us) x = removeU us xs
                         | otherwise = 
                           [r| r<-x, notElem (invert r) us] : (removeU us xs)

prop_dpll :: CNF -> Property
prop_dpll c = 
  case dpll c of 
    Just m -> if valid m then
       (property (interp m c))
      else property False
    Nothing ->  (property True)  
    
myLit :: Gen Lit
myLit = oneof [choose (-10,-1), choose (1,10)]


myClause :: Gen Clause
myClause = frequency [ (1, return []),
                       (9,liftM2 (:) myLit myClause)]

myCs :: Gen [Clause]
myCs = frequency [(1, return []),
                  (9, liftM2 (:) myClause myCs)]

instance Arbitrary CNF where
         arbitrary = liftM CNF myCs

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO () 
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }
