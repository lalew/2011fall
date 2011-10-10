-- Advanced Programming, HW 3
-- by <YOUR NAME HERE> <pennid> (and  <YOUR PARTNERS NAME> <pennid>)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude hiding (mapM)
import Data.Char (isAlpha, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State hiding (when, mapM, foldM)
import Test.HUnit hiding (State)

main :: IO ()
main = return ()

-- Problem 0

data Seq a = Nil | Single a | Concat (Seq a) (Seq a)

--  (a) 

instance Show a => Show (Seq a) where
   show xs = show (toList xs)

--instance Foldable Seq where
--   foldMap f Nil = mempty
--   foldMap f (Single x) = f x
--   foldMap f (Concat l r) = foldMap f l  `mappend` foldMap f r

foldSeq :: (a -> b -> b) -> b -> Seq a -> b
foldSeq _ z Nil = z
foldSeq f z (Single x) = f x z
foldSeq f z (Concat l r) = foldSeq f (foldSeq f z r) l

toList :: Seq a -> [a]
toList = foldSeq (\x xs->x:xs) [] 
--toList Nil = []
--toList (Single x)     = [x]
--toList (Concat s1 s2) = toList s1 ++ toList s2
 

t0a :: Test
t0a = toList (Concat (Concat (Concat (Concat (Single 0) 
                                             (Single 1)) 
                                     (Single 2))
                             (Single 3)) 
                     (Single 4)) ~?= [0,1,2,3,4]

-- (b)

instance Functor Seq where
   fmap _ _  = error "TBD"

instance Monad Seq where
   return = error "TBD"
 
   _ >>= _ = error "TBD"



first :: Seq a -> Maybe a 
first = error "TBD"

-- Problem 1

-- (a)

pickyMap2 :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
pickyMap2 = error "TBD"



-- (b) 

transpose :: [[a]] -> Maybe [[a]]
transpose = error "TBD"



-- (c)

partialPickyMap2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
partialPickyMap2 = error "TBD"



-- Problem 2
-- (a)

sumFirstHundred :: Int
sumFirstHundred = error "TBD"



-- (b)

pyths :: Int -> [(Int,Int,Int)]
pyths = error "TBD"



-- (c)

sieveSundaram :: Int -> [Int]
sieveSundaram = error "TBD"



-- Problem 3

-- (a) Define another monadic generalization of map (do not use any 
--     functions defined in Control.Monad for this problem). 

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = error "TBD"

safeUpper :: Char -> Maybe Char
safeUpper x = if isAlpha x then Just (toUpper x) else Nothing

t3a :: Test
t3a = TestList [ mapM safeUpper "sjkdhf"  ~?= Just "SJKDHF", 
                 mapM safeUpper "sa2ljsd" ~?= Nothing ]

-- (b) Define a monadic generalization of foldr (again, do not use any 
--     functions defined in Control.Monad for this problem).

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM = error "TBD"

t3b :: Test
t3b = TestList [ addEven [1,2,3]  ~=? Nothing, 
                 addEven [2,4]    ~=? Just 6]

addEven :: [Int] -> Maybe Int
addEven xs = foldM f 0 xs where 
               f x y | even x    = Just (x + y)
                     | otherwise = Nothing

-- Problem 4

type Variable = String

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Eq, Show)

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v 
  | Op  Bop Expression Expression
  deriving (Eq, Show)

data Bop = 
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool 
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Eq, Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Eq, Show)

type Store = Map Variable Value

evalE :: Expression -> State Store Value

evalE (Var _)    = error "TBD"
evalE (Val _)    = error "TBD"
evalE (Op _ _ _) = error "TBD"
 



evalS :: Statement -> State Store ()

evalS (While _ _)      = error "TBD" 
evalS Skip             = error "TBD"
evalS (Sequence _ _  ) = error "TBD"
evalS (Assign _ _)     = error "TBD"
evalS (If _ _ _ )      = error "TBD"

execS :: Statement -> Store -> Store
execS = error "TBD"

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:" 
              putStrLn $ show $ execS stmt Map.empty

w_test :: Statement
w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

w_fact :: Statement
w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

t4a :: Test 
t4a = execS w_test Map.empty ~?= 
        Map.fromList [("X",IntVal 0),("Y",IntVal 10)]

t4b :: Test
t4b = execS w_fact Map.empty ~?=
        Map.fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]

