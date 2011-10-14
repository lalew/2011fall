-- Advanced Programming, HW 3
-- by Zi Yan yanzi (and  Adrian Benton adrianb)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude hiding (mapM)
import Data.Char (isAlpha, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State hiding (when, mapM, foldM)
import Test.HUnit hiding (State)
import Data.Maybe (fromMaybe)


main :: IO ()
main = runTestTT allTest>>return ()

allTest :: Test
allTest = TestList [test0, test1, test2, test3, test4]

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
foldSeq f z (Concat hd tl) = foldSeq f (foldSeq f z tl) hd

toList :: Seq a -> [a]
toList = foldSeq (\x xs->x:xs) [] 
--toList Nil = []
--toList (Single x)     = [x]
--toList (Concat s1 s2) = toList s1 ++ toList s2
 

t0a :: Test
t0a = TestList [toList (Concat (Concat (Concat (Concat (Single 0) 
                                             (Single 1)) 
                                     (Single 2))
                             (Single 3)) 
                     (Single 4)) ~?= [0,1,2,3,4],
                toList (Concat (Concat (Single 1) (Single 2))
                               (Concat (Single 3) (Single 4))) ~?= [1,2,3,4]]

-- (b)

instance Functor Seq where
   fmap _ Nil = Nil
   fmap f (Single a) = Single (f a)
   fmap f (Concat hd tl) = Concat (fmap f hd) (fmap f tl)

instance Monad Seq where
   -- return :: a -> Seq a
   return a = Single a
 
   -- >>= :: Seq a -> (a -> Seq b) -> Seq b
   Nil >>= _ = Nil
   Single a >>= f = f a
   Concat hd tl >>= f = Concat (hd >>= f) (tl >>= f)

sngl2 :: Seq Int
sngl2 = Single 2

con04 :: Seq Int
con04 = (Concat (Concat (Concat (Concat (Single 0) 
                                             (Single 1)) 
                                     (Single 2))
                             (Single 3)) 
                     (Single 4))

t0b1 :: Test
t0b1 = TestList [toList (fmap (+1) Nil) ~?= fmap (+1) (toList Nil),
                 toList (fmap (+1) sngl2) ~?= fmap (+1) (toList sngl2),
                 toList (fmap (+1) con04) ~?= fmap (+1) (toList con04)]

t0b2 :: Test
t0b2 = TestList [toList (return 9) ~?= return 9,
                 toList (return [1,2]) ~?= return [1,2]]

incSeq :: Int -> Seq Int
incSeq a = Single (a+1)

t0b3 :: Test
t0b3 = TestList [(toList (Nil >>= incSeq)) ~?= (toList Nil >>= (toList . incSeq)),
                 (toList (sngl2 >>= incSeq)) ~?= (toList sngl2 >>= (toList . incSeq)),
                 (toList (con04 >>= incSeq)) ~?= (toList con04 >>= (toList .incSeq))]


-- Works correctly because the definition of `mplus` states that
-- the first value that if both arguments evaluate to Just, then
-- the first will be returned.   By Adrian
first :: Seq a -> Maybe a 
first Nil = Nothing
first (Single a) = Just a
first (Concat hd tl) = first hd `mplus` first tl

t0b4 :: Test
t0b4 = TestList[first (Nil::Seq Int) ~?= Nothing,
                first (Single 8) ~?= Just 8,
                first con04 ~?= Just 0,
                first (Concat Nil sngl2) ~?= Just 2]

test0 :: Test
test0 = TestList [t0a, t0b1, t0b2, t0b3, t0b4]

-- Problem 1

-- (a)

pickyMap2 :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
pickyMap2 f (x:xs) (y:ys) = Just (f x y) `maybeCon` pickyMap2 f xs ys
pickyMap2 _ [] [] = Just []
pickyMap2 _ [] _ = Nothing
pickyMap2 _ _ [] = Nothing

maybeCon :: Maybe a -> Maybe [a] -> Maybe [a]
maybeCon Nothing _ = Nothing
maybeCon _ Nothing = Nothing
maybeCon (Just a) (Just as) = Just (a:as)

t1a :: Test

t1a = TestList [pickyMap2 (+) [1,2] [3,4] ~?= Just [4,6],
                pickyMap2 (+) [1,2] [3,4,5] ~?= Nothing,
                pickyMap2 (+) [1,2,3] [3,4] ~?= Nothing]

-- (b) 

transpose :: [[a]] -> Maybe [[a]]
transpose [] = Just []
transpose (xs:xss) = aux (length xs) (xs:xss)
  where aux n (ys:yss) = pickyMap2 (:) 
                                   ys 
                                   (fromMaybe (iter ((length ys) +1) []) 
                                              (aux n yss))
  -- in case ys is [], the default case of fromMaybe 
  -- should incur Nothing in pickyMap2
        aux n []       = Just (iter n [])
        iter 0 _       = []
        iter n z       = z:iter (n-1) z

t1b :: Test
t1b = TestList [transpose [[1,2,3], [4,5,6]] ~?= Just [[1,4],[2,5],[3,6]],
                transpose [[1,2,3],[4,5]] ~?= Nothing,
                transpose [[1,2],[4,5,6]] ~?= Nothing,
                transpose [[], [1,2]] ~?= Nothing ]

-- (c)

partialPickyMap2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
partialPickyMap2 f (x:xs) (y:ys) = f x y `maybeCon` partialPickyMap2 f xs ys
partialPickyMap2 _ [] [] = Just []
partialPickyMap2 _ [] _ = Nothing
partialPickyMap2 _ _ [] = Nothing

code :: [(Int, Char)]
code = [(1,'a'),(2,'b'),(3,'c')]

t1c :: Test
t1c = TestList [partialPickyMap2 lookup [1,2,3] [code,code,code] ~?= Just "abc",
                partialPickyMap2 lookup [1,4,3] [code,code,code] ~?= Nothing]

test1 :: Test
test1 = TestList [t1a, t1b, t1c]

-- Problem 2
-- (a)

sumFirstHundred :: Int
sumFirstHundred = sum [x*x | x <-[1..100]]

t2a :: Test
t2a = sumFirstHundred ~?= 338350


-- (b)

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z)| x<-[1..n], y<-[1..n], z<-[1..n], x*x+y*y==z*z]

t2b :: Test
t2b = TestList [pyths 1 ~?= [],
                pyths 10 ~?= [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]]


-- (c)

sieveSundaram :: Int -> [Int]
sieveSundaram n = 2:[2*x+1| x<-[1..n], notElem x (ruleOut n) ]

ruleOut:: Int ->[Int]
ruleOut n = [i+j+2*i*j|i<-[1..n],j<-[1..n],i<=j,i+j+2*i*j<=n]


t2c :: Test
t2c = TestList [sieveSundaram 10 ~?= [2,3,5,7,11,13,17,19]]

test2 :: Test
test2 = TestList [t2a, t2b, t2c]
-- Problem 3

-- (a) Define another monadic generalization of map (do not use any 
--     functions defined in Control.Monad for this problem). 

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = conM (f x) (mapM f xs)

conM :: Monad m => m a -> m [a] -> m [a]
conM a b = do
           x <- a
           xs <- b
           return (x:xs)

safeUpper :: Char -> Maybe Char
safeUpper x = if isAlpha x then Just (toUpper x) else Nothing

t3a :: Test
t3a = TestList [ mapM safeUpper "sjkdhf"  ~?= Just "SJKDHF", 
                 mapM safeUpper "sa2ljsd" ~?= Nothing ]

-- (b) Define a monadic generalization of foldr (again, do not use any 
--     functions defined in Control.Monad for this problem).

-- this signature is foldl-like, not foldr-like

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ z [] = return z
foldM f z (b:bs) = do
                   res <- f z b
                   foldM f res bs

t3b :: Test
t3b = TestList [ addEven [1,2,3]  ~=? Nothing, 
                 addEven [2,4]    ~=? Just 6,
                 addEven [2]      ~?= Just 2]

addEven :: [Int] -> Maybe Int
addEven xs = foldM f 0 xs where 
               f x y | even x    = Just (x + y)
                     | otherwise = Nothing


test3 :: Test
test3 = TestList [t3a, t3b]
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

--evalE (Var v)    = state (\pre -> (fromMaybe (IntVal 0) (Map.lookup v pre), pre))
evalE (Var x) = do
                  pre<-get
                  return (fromMaybe (IntVal 0) (Map.lookup x pre))
evalE (Val v)    = return v
evalE (Op bop le re) = do
                        --pre<-get
                        l<- evalE le
                        r<- evalE re
                        return (case bop of
                                Plus   -> bPlus l r
                                Minus  -> bMinus l r
                                Times  -> bTimes l r
                                Divide -> bDivide l r
                                Gt     -> bGt l r 
                                Ge     -> bGe l r
                                Lt     -> bLt l r
                                Le     -> bLe l r
                               )
bPlus :: Value -> Value -> Value
bPlus (IntVal a) (IntVal b) = IntVal (a+b)
bPlus _ _ = IntVal 0

bMinus :: Value -> Value -> Value   -- -  :: Int  -> Int  -> Int
bMinus (IntVal a) (IntVal b) = IntVal (a-b)
bMinus _ _ = IntVal 0

bTimes :: Value -> Value -> Value   -- *  :: Int  -> Int  -> Int
bTimes (IntVal a) (IntVal b) = IntVal (a*b)
bTimes _ _ = IntVal 0
  
bDivide :: Value -> Value -> Value  -- /  :: Int  -> Int  -> Int
bDivide _ (IntVal 0) = error "divided by 0"
bDivide (IntVal a) (IntVal b) = IntVal (a `div` b)
bDivide _ _ = IntVal 0

bGt :: Value -> Value -> Value -- >  :: Int -> Int -> Bool
bGt (IntVal a) (IntVal b) = BoolVal (a>b)
bGt _ _ = error "wrong typed"


bGe :: Value -> Value -> Value       -- >= :: Int -> Int -> Bool
bGe (IntVal a) (IntVal b) = BoolVal (a>=b)
bGe _ _ = error "wrong typed"

bLt :: Value -> Value -> Value       -- <  :: Int -> Int -> Bool
bLt (IntVal a) (IntVal b) = BoolVal (a<b)
bLt _ _ = error "wrong typed"

bLe :: Value -> Value -> Value     -- <= :: Int -> Int -> Bool
bLe (IntVal a) (IntVal b) = BoolVal (a<=b)
bLe _ _ = error "wrong typed"

bIsTrue :: Value -> Bool
bIsTrue (BoolVal a) = a
bIsTrue _ = error "wrong typed"
                             


evalS :: Statement -> State Store ()
evalS (While cond stmt)= do
                            res <- evalE cond
                            if (bIsTrue res)
                            then evalS (Sequence stmt (While cond stmt)) 
                            else return ()
evalS Skip             = return ()
evalS (Sequence s1 s2) = evalS s1>>evalS s2
evalS (Assign v e)     = do 
                           val<- evalE e
                           pre<- get
                           put (Map.insert v val pre)

evalS (If cond s1 s2)  = do
                           res <- evalE cond
                           if (bIsTrue res)
                           then evalS s1
                           else evalS s2

execS :: Statement -> Store -> Store
execS stmt = execState $ evalS stmt

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

t4c :: Test
t4c = execS (Sequence t_1 t_2) Map.empty ~?= Map.fromList [("X",IntVal 8),("Y", IntVal 8)]

t_1 :: Statement
t_1 = Assign "X" (Val (IntVal 8))

t_2 :: Statement
t_2 = Assign "Y" (Var "X")

test4 :: Test
test4 = TestList [t4a, t4b, t4c]