HW 3 # Monads
=============

> -- Advanced Programming, HW 3
> -- Solution and variations

Submission Instructions
=======================

To complete this homework, download [the nonliterate version of the
file](Main.hs) and answer each question, filling in code where noted
(where it says "TBD").  Your code must typecheck against the given
type signatures.  Also remember to add your own tests to this file to
exercise the functions you write, and make your own 'main' function to
run those tests.

Also, this assignment should be done in PAIRS. Only one version of the 
homework should be submitted from each group.

> {-# OPTIONS  -fno-warn-type-defaults #-}
> {-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

> module Main where

> import Prelude hiding (mapM)
> import Data.Char (isAlpha, toUpper)
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Control.Monad.State hiding (when, mapM, foldM)
> import Test.HUnit hiding (State)
> import Test.QuickCheck
> import Test.QuickCheck.Function

>  
> main :: IO ()
> main = do  
>  _ <- runTestTT $ TestList [ test0, test1, test2, test3, test4 ] 
>  return ()
>     where
>         test0 = TestList [t0a, t0b1, t0b2, t0b3, t0c]
>         test1 = TestList [t1a, t1b, t1c]
>         test2 = TestList [t2a, t2b, t2c]
>         test3 = TestList [t3a, t3b]
>         test4 = TestList [t4a, t4b]
> 

Functors and Monads
===================

> -- Problem 0

The following parameterized data structure, called a sequence, 

> data Seq a = Nil | Single a | Concat (Seq a) (Seq a) deriving (Eq)

is a list optimized for concatenation. Indeed, two sequences can be
concatenated in constant time, using the `Concat` constructor. 

> --  (a) 

It is also possible to convert a sequence into a normal list, which 
is useful for displaying sequence values concisely.

> instance Show a => Show (Seq a) where
>    show xs = show (toList xs)

However, the implementation of toList below is inefficient for
left-biased sequences. For example, those of the form: 

    Concat (Concat (Concat (Concat (Single a) 
                                   (Single b))
                           (Single c))
                   (Single d))
            (Single e))

In converting such sequences to lists, the append operation will need
to traverse the intermediate result lists multiple times. (Recall the
implementation of (++) is not constant time---it must loop over its
first argument.)

Your first job is to replace this version of toList with a more
efficient version.

--toList Nil = []
--toList (Single x)     = [x]
--toList (Concat s1 s2) = toList s1 ++ toList s2





> toList0 :: Seq a -> [a]
> toList0 Nil              = []
> toList0 (Single x)       = [x]
> toList0 (Concat s1 s2) =  mplus (toList s1) (toList s2)



> lconcat :: [a] -> [a] -> [a]
> lconcat (x:xs) ys = x : lconcat xs ys
> lconcat [] ys = ys

> lconcat' xs ys = aux ys (reverse xs) where
>     aux acc []     = acc
>     aux acc (x:xs) = aux (x:acc) xs


> toList1 :: Seq a -> [a]
> toList1 Nil            = []
> toList1 (Single x)     = [x]
> toList1 (Concat s1 s2) = reverse $ aux [] (Concat s1 s2) where
>                          aux seqs Nil               = seqs
>                          aux seqs (Single x)        = x:seqs
>                          aux seqs (Concat s1' s2')  = aux (aux seqs s1') s2'



> foldSeq :: (a -> b -> b) -> b -> Seq a -> b
> foldSeq _ z Nil = z
> foldSeq f z (Single x) = f x z
> foldSeq f z (Concat hd tl) = foldSeq f (foldSeq f z tl) hd

> toList2 :: Seq a -> [a]
> toList2 = foldSeq (:) [] 





> toList3 :: Seq a -> [a]
> toList3 x = toList' x []  
>  where
>    toList' :: Seq a -> [a] -> [a]
>    toList' Nil x'            = x' 
>    toList' (Single x') xs    = x' : xs
>    toList' (Concat s1 s2) x' = toList' s1 (toList' s2 x')





> toList :: Seq a -> [a]
> toList s = reverse (aux s []) where
>     aux Nil            l = l
>     aux (Single x)     l = (x:l)
>     aux (Concat s1 s2) l = l2 where
>       l1 = aux s1 l 
>       l2 = aux s2 l1
> 

> t0a :: Test
> t0a = toList1 (Concat (Concat (Concat (Concat (Single 0) 
>                                              (Single 1)) 
>                                      (Single 2))
>                              (Single 3)) 
>                      (Single 4)) ~?= [0,1,2,3,4]

> -- (b)

Complete the Functor and Monad instances for sequences. Because we're
practicing, do not use 'toList' in your implementation.

> instance Functor Seq where
>    
>    fmap _ Nil = Nil
>    fmap f (Single a) = Single (f a)
>    fmap f (Concat s1 s2) = Concat (fmap f s1) (fmap f s2)
> 

How do we know if this is the correct implementation of Functor?
Let's test it to see if it satisfy the functor laws.

> instance Arbitrary a => Arbitrary (Seq a) where
>    arbitrary = sized arbn where
>      arbn n = frequency [ (1, return Nil), 
>                           (1, liftM Single arbitrary),
>                           (n, liftM2 Concat (arbn (n `div` 2))
>                                             (arbn (n `div` 2))) ]      

First law, mapping the id function is an identity:                       
    
> prop_f1 :: Seq Int -> Bool
> prop_f1 x = fmap id x == id x

> -- prop_f2a :: (String -> Int) -> (Bool -> String) -> Seq Bool -> Bool
> -- prop_f2a f g x = (fmap ( f . g ) x) == (fmap f . fmap g) x

> data BB = BB (Bool -> Bool) String
> instance Show BB where
>    show (BB _ s) = s
> instance Arbitrary BB where
>   arbitrary = elements [ BB id "id", BB (const True) "true", 
>                          BB (const False) "false", BB not "not"]


> prop_f2 :: BB -> BB -> Seq Bool -> Bool
> prop_f2 (BB f _) (BB g _) x = fmap (f . g) x == (fmap f . fmap g) x



> prop_f2' :: (Fun Int Int) -> (Fun Int Int) -> Seq Int -> Bool
> prop_f2' (Fun _ f) (Fun _ g) x = fmap (f . g) x == (fmap f . fmap g) x


> {- instance Monad Seq where
>    
>    return = Single
>    
>    Nil            >>= _ = Nil
>    (Single a)     >>= f = f a
>    (Concat s1 s2) >>= f = Concat (s1 >>= f) (s2 >>= f)
> -} 

The monad laws:

> prop_m1 :: Bool -> (Fun Bool (Seq Bool)) -> Bool
> prop_m1 a (Fun _ k) = (return a >>= k) == k a

> prop_m2 :: Seq Bool -> Bool
> prop_m2 m = (m >>= return) == m

> prop_m3 :: Seq Bool -> (Fun Bool (Seq Bool)) -> (Fun Bool (Seq Bool)) -> Bool
> prop_m3 m (Fun _ k) (Fun _ h) = 
>    (m >>= (\x -> k x >>= h)) == ((m >>= k) >>= h)

> prop_mf :: (Fun Bool Bool) -> Seq Bool -> Bool
> prop_mf (Fun _ f) xs = (fmap f xs) == (xs >>= return . f)

=========================================

> 
> instance Monad Seq where
>   return = Single
>   xs >>= f = seqConcat (fmap f xs)

> seqConcat :: Seq (Seq a) -> Seq a 
> seqConcat Nil = Nil
> seqConcat (Single x) = x
> seqConcat (Concat x y) = Concat (seqConcat x) (seqConcat y)
> 

=========================================

The functor and monad instance for sequences should be the equivalent
to the ones for lists. More formally, we can say that the following
list equalities should hold, no matter what values are used for
f,s,x,m and k.

      toList (fmap f s) == fmap f (toList s)
         where s :: Seq a 
               f :: a -> b
      
      toList (return x) == return x 
         where x :: a
               
      toList (m >>= k) == toList m >>= (toList . k)
         where m :: Seq a 
               k :: a -> Seq b  
  
Write (at least) three test cases that test these three identities.

> 
> t0b1 :: Test
> t0b1 = "t0b1" ~: toList (fmap f s) ~?= fmap f (toList s)
>    where s = seq1
>          f = (+1)  
> 
> t0b2 :: Test
> t0b2 = "t0b2" ~: toList (return x) ~?= return x 
>    where x = 3
>          
> t0b3 :: Test
> t0b3 = "t0b3" ~: toList (m >>= k) ~?= (toList m >>= (toList . k))
>    where m = seq1
>          k = Single 
> 
> seq1 :: Seq Int
> seq1 = Concat (Concat Nil (Single 3)) (Concat (Single 4) (Single 5))
> 

--- And, we can use quickcheck for these properties too!

> prop_l1 :: (Fun Bool Bool) -> Seq Bool -> Bool
> prop_l1 (Fun _ f) s = toList (fmap f s) == fmap f (toList s)

> prop_l2 :: Bool -> Bool
> prop_l2 x = toList (return x) == return x 

> prop_l3 :: (Seq Bool) -> (Fun Bool (Seq Bool)) -> Bool
> prop_l3 m (Fun _ k) = (toList (m >>= k)) == (toList m >>= (toList . k))




The drawback of sequences is that accessing the first element may not
be a constant time operation.  Nevertheless, it is possible to write a
concise function to find this element, taking advantage of the fact
that `Maybe` is a member of the `MonadPlus` type class. Read this
[wikipage](http://en.wikibooks.org/wiki/Haskell/MonadPlus) for more
information, and write this operation as succinctly as you can.

> first :: Seq a -> Maybe a 
> 
> first Nil            = Nothing 
> first (Single x)     = Just x
> first (Concat s1 s2) = first s1 `mplus` first s2
> 
> t0c :: Test
> t0c = "t0c" ~: first seq1 ~=? Just 3
> 

> first0 :: Seq a -> Maybe a 
> first0 = msum . toList . fmap Just

Maybe Monad Practice
====================

> -- Problem 1

Rewrite the map2 function from homework 1 so that it is more
"picky". If the two input lists are different lengths, then the
function should return Nothing instead of truncating one of the lists.

> -- (a)

> pickyMap2a :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
> pickyMap2a f xs ys = if length xs /= length ys then Nothing 
>                                  else Just (zipWith f xs ys)




> pickyMap2b :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
> pickyMap2b f (x:xs) (y:ys) = let r = pickyMap2b f xs ys in
>  case r of
>    Nothing -> Nothing
>    Just q  -> Just ((f x y) : q) 
> pickyMap2b _ [] [] = Just []
> pickyMap2b _ _ _ = Nothing




> pickyMap2c :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
> pickyMap2c _ [] []          = Just [] 
> pickyMap2c _ [] _           = Nothing
> pickyMap2c _ _ []           = Nothing
> pickyMap2c f (x:xs) (y:ys)  = 
>    (pickyMap2c f xs ys) >>= \t -> Just ((f x y) : t)







> pickyMap2d :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
> pickyMap2d _ [] []           = Just []
> pickyMap2d f (x:xs) (y:ys)   = do
>                                zs <- pickyMap2d f xs ys
>                                return (f x y : zs)
> pickyMap2d _ _ _             = Nothing






> pickyMap2e :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
> 
> pickyMap2e _ [] []         = return []
> pickyMap2e f (x:xs) (y:ys) = liftM (f x y :) (pickyMap2e f xs ys)
> pickyMap2e _ _ _           = Nothing
> 



> 
> t1a :: Test
> t1a = TestList [ pickyMap2a (+) [1] [] ~?= Nothing,
>                  pickyMap2a (+) [1] [2] ~?= Just [3],
>                  pickyMap2a (+) [] [] ~?= Just ([] :: [Int]) ]
> 

> -- (b) 

Now use pickyMap2 to rewrite the transpose function so that if the
inner lists are not all the same length, then transpose returns
Nothing.

> transpose0 :: [[a]] -> Maybe [[a]]
> 
> transpose0 [] = Just []
> transpose0 (xs : xss) = aux (length xs) (xs : xss)
>    where aux n (ys : yss) = do 
>                rs <- (aux n yss) 
>                pickyMap2a (:) ys rs
>          aux n []         = return (iter n [])
>          iter 0 _         = []
>          iter n z         = z : iter (n - 1) z
> 

> transpose1 :: [[a]] -> Maybe [[a]]
> transpose1 []     = Just []
> transpose1 [r]    = Just $ map (:[]) r
> transpose1 (r:rs) = do
>  mat' <- transpose1 rs    -- transpose rs gives a maybe monad of the result
>                          -- of the rest of the matrix except row r
>  pickyMap2a (:) r mat'    -- and the result is combined with r by appending
>                          -- elems of r to the result of rest matrix

> transpose2 :: [[a]] -> Maybe [[a]]
> transpose2 = foldr helper (Just [])
>  where
>    helper :: [a] -> Maybe [[a]] -> Maybe [[a]]
>    helper l acc = 
>        do
>          ls <- acc
>          case ls of 
>            [] -> return $ fmap (:[]) l
>            _  -> pickyMap2a (:) l ls 


> 
> t1b :: Test
> t1b = "1b" ~: TestList [ 
>    transpose0 [[1,2,3],[4,5,6]] ~?= Just [[1,4],[2,5],[3,6]], 
>    transpose0 [[1,2],[]] ~?= Nothing ]
> 

> -- (c)

Next rewrite map2 again so that the function passed in can also be
partial. If any application of the function returns 'Nothing' then the 
entire result should be 'Nothing'

> partialPickyMap2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
> 
> partialPickyMap2 _ [] [] = return []
> partialPickyMap2 f (x:xs) (y:ys) = liftM2 (:) (f x y) (partialPickyMap2 f xs ys) 
> partialPickyMap2 _ _ _ = Nothing
> 

> 
> safediv :: Int -> Int -> Maybe Int
> safediv x y = if y == 0 then Nothing else Just (x `div` y)
> 
> t1c :: Test
> t1c = "1c" ~: TestList [
>   partialPickyMap2 safediv [3,4] [1,2] ~?= Just [3,2],
>   partialPickyMap2 safediv [3,4] [1,0] ~?= Nothing,
>   partialPickyMap2 safediv [3,4] [] ~?= Nothing ]
> 

List Comprehension Practice
===========================

> -- Problem 2
> -- (a)

Using a list comprehension, give an expression that calculates the
sum of the first one hundred integer squares (i.e. 1^2 + 2^2 + ... +
100^2).

> sumFirstHundred :: Int
> 
> sumFirstHundred = sum [ i * i | i <- [1 .. 100]]
> 

> 
> t2a :: Test
> t2a = "t2a" ~: sumFirstHundred ~?= 338350 
> 
  
> -- (b)

A triple (x,y,z) is Pythagorean if x^2 + y^2 = z^2. Use a list
comprehension to produce a list of pythagorean triples whose
components are at most a given limit.  For example, pyths 10 returns
[(3,4,5), (4,3,5), (6,8,10), (8,6,10)]

> pyths :: Int -> [(Int,Int,Int)]
> 
> pyths n = [ (x,y,z) | x <- [1.. n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
> 

> 
> t2b :: Test
> t2b = pyths 10 ~?= [(3,4,5), (4,3,5), (6,8,10), (8,6,10)]
> 

> -- (c)

Read about the [Sieve of
Sundaram](http://en.wikipedia.org/wiki/Sieve_of_Sundaram) and
implement it using (nested) list comprehensions. You do not need to
worry about the running time and memory usage, instead make sure that
your code clearly implements the algorithm described on that page.

> sieveSundaram :: Int -> [Int]
> 
> sieveSundaram n = 
>    [ 2 * k + 1 | k <- [1 .. n] , not (k `elem` xs) ] where
>       xs = [ (i+j+2*i*j) | i <- [1 .. n], 
>                            j <- [1 .. n], 
>                            i < j && i+j+2*i*j <= n ] 
> 

> 
> t2c :: Test
> t2c = sieveSundaram 10 ~?= [3,5,7,9,11,13,17,19]
> 

General Monadic Functions 
=========================

> -- Problem 3

> -- (a) Define another monadic generalization of map (do not use any 
> --     functions defined in Control.Monad for this problem). 
	
> mapM :: Monad m => (a -> m b) -> [a] -> m [b]
> 
> mapM _ []     = return []
> mapM f (x:xs) = do
>    b <- f x 
>    bs <- mapM f xs
>    return (b:bs)
> 

> safeUpper :: Char -> Maybe Char
> safeUpper x = if isAlpha x then Just (toUpper x) else Nothing

> t3a :: Test
> t3a = TestList [ mapM safeUpper "sjkdhf"  ~?= Just "SJKDHF", 
>                  mapM safeUpper "sa2ljsd" ~?= Nothing ]

> -- (b) Define a monadic generalization of foldr (again, do not use any 
> --     functions defined in Control.Monad for this problem).

> foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
> 
> foldM _ a [] = return a
> foldM f a (b:bs) = do 
>     a' <- foldM f a bs
>     f a' b
> 


> t3b :: Test
> t3b = TestList [ addEven [1,2,3]  ~=? Nothing, 
>                  addEven [2,4]    ~=? Just 6]

> addEven :: [Int] -> Maybe Int
> addEven xs = foldM f 0 xs where 
>                f x y | even x    = Just (x + y)
>                      | otherwise = Nothing

An Interpreter for WHILE 
========================

> -- Problem 4

In this problem, you will use monads to build an evaluator for a
simple imperative language, called WHILE. In this language, we will
represent different program variables as

> type Variable = String

Programs in the language are simply values of the type

> data Statement =
>     Assign Variable Expression          -- x = e
>   | If Expression Statement Statement   -- if (e) {s1} else {s2}
>   | While Expression Statement          -- while (e) {s}
>   | Sequence Statement Statement        -- s1; s2
>   | Skip                                -- no-op
>   deriving (Eq, Show)

where expressions are variables, constants or 
binary operators applied to sub-expressions

> data Expression =
>     Var Variable                        -- x
>   | Val Value                           -- v 
>   | Op  Bop Expression Expression
>   deriving (Eq, Show)

and binary operators are simply two-ary functions

> data Bop = 
>     Plus     -- +  :: Int  -> Int  -> Int
>   | Minus    -- -  :: Int  -> Int  -> Int
>   | Times    -- *  :: Int  -> Int  -> Int
>   | Divide   -- /  :: Int  -> Int  -> Int
>   | Gt       -- >  :: Int -> Int -> Bool 
>   | Ge       -- >= :: Int -> Int -> Bool
>   | Lt       -- <  :: Int -> Int -> Bool
>   | Le       -- <= :: Int -> Int -> Bool
>   deriving (Eq, Show)

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Eq, Show)

We will represent the *store* i.e. the machine's memory, as an associative
map from `Variable` to `Value` 

> type Store = Map Variable Value

**Note:** we don't have exceptions (yet), so if a variable
is not found (eg because it is not initialized) simply return 
the value `0`. In future assignments, we will add this as a 
case where exceptions are thrown (the other case being type errors.)

We will use the standard library's `State` 
[monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
to represent the world-transformer.
Intuitively, `State s a` is equivalent to the world-transformer 
`s -> (a, s)`. See the above documentation for more details. 
You can ignore the bits about `StateT` for now.

Expression Evaluator
--------------------

First, write a function 

> evalE :: Expression -> State Store Value

that takes as input an expression and returns a state-transformer that
returns a value. Yes, right now, the transformer doesn't really transform
the world, but we will use the monad nevertheless as later, the world may
change, when we add exceptions and such.

Again, we don't have any exceptions or typechecking, so the
interpretation of an ill-typed binary operation (such as '2 + True')
should return always 0.

**Hint:** The value `get` is of type `State Store Store`. Thus, to extract 
the value of the "current store" in a variable `s` use `s <- get`.

> 
> evalE (Var x)      = do  
>   m <- get 
>   case (Map.lookup x m) of
>     Just v ->  return v
>     Nothing -> return (IntVal 0)
> evalE (Val v)      = return v
> evalE (Op o e1 e2) = liftM2 (evalB o) (evalE e1) (evalE e2) 
> 

> 
> evalB :: Bop -> Value -> Value -> Value
> evalB Plus   (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
> evalB Minus  (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
> evalB Times  (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
> evalB Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
> evalB Gt     (IntVal i1) (IntVal i2) = BoolVal (i1 > i2)
> evalB Ge     (IntVal i1) (IntVal i2) = BoolVal (i1 >= i2)
> evalB Lt     (IntVal i1) (IntVal i2) = BoolVal (i1 < i2)
> evalB Le     (IntVal i1) (IntVal i2) = BoolVal (i1 <= i2)
> evalB _ _ _ = IntVal 0
> 

evalBop :: Bop -> Value -> Value -> Value
evalBop = aux
  where
    aux Plus   = ret IntVal  (+)
    aux Minus  = ret IntVal  (-)
    aux Times  = ret IntVal  (*)
    aux Divide = ret IntVal  div
    aux Gt     = ret BoolVal (>)
    aux Ge     = ret BoolVal (>=)
    aux Lt     = ret BoolVal (<)
    aux Le     = ret BoolVal (<=)
    ret constructor f (IntVal a) (IntVal b) = constructor $ f a b
    ret _ _ _ _ = IntVal 0



Statement Evaluator
-------------------

conditional :: Value -> Bool
conditional (BoolVal b) = b                   
conditional (IntVal v) | v == 0 = False
                       | otherwise = True


Next, write a function

> evalS :: Statement -> State Store ()

that takes as input a statement and returns a world-transformer that
returns a unit. Here, the world-transformer should in fact update the input
store appropriately with the assignments executed in the course of
evaluating the `Statement`.

**Hint:** The value `put` is of type `Store -> State Store ()`. 
Thus, to "update" the value of the store with the new store `s'` 
do `put s`.


evalS (While e s)      = do 
  cond <- evalE e                         -- First evaluate the condition exp
  let again = evalS (While e s) in
    case cond of
      BoolVal True  -> do                 -- if exp is true eval the statement
        evalS s
        again                             -- and evaluate the whole while
                                          -- exp based on the new state
      BoolVal False -> return ()          -- if exp is false then return 
      _             -> return ()          -- otherwise return

> 
> evalS w@(While e s)    = do 
>   v <- evalE e
>   case v of    
>     BoolVal True  -> evalS (Sequence s w)
>     BoolVal False -> return ()
>     IntVal  _     -> return ()

evalS Skip                   = do
                                 s <- get
                                 put s


> evalS Skip             = return ()

evalS (Sequence s1 s2) = 
  do
    st <- get
    let st' = execState (evalS s1) st
    put $ execState (evalS s2) st'
    return ()



> evalS (Sequence s1 s2) = evalS s1 >> evalS s2



> evalS (Assign x e)     = do 
>     v <- evalE e
>     m <- get
>     put (Map.insert x v m)
> evalS (If e s1 s2)      = do
>     v <- evalE e 
>     case v of 
>       BoolVal True  -> evalS s1
>       BoolVal False -> evalS s2
>       IntVal _ -> return ()
> 

In the `If` and `While` cases, if `e` evaluates to a non-boolean
value, just skip. Finally, write a function

> execS :: Statement -> Store -> Store
> execS =  execState . evalS  

such that `execS stmt store` returns the new `Store` that results
from evaluating the command `stmt` from the world `store`. 
**Hint:** You may want to use the library function 

~~~~~{.haskell}
execState :: State s a -> s -> s
~~~~~

When you are done with the above, the following function will 
"run" a statement starting with the `empty` store (where no 
variable is initialized). Running the program should print 
the value of all variables at the end of execution.

> run :: Statement -> IO ()
> run stmt = do putStrLn "Output Store:" 
>               putStrLn $ show $ execS stmt Map.empty

Here are a few "tests" that you can use to check your implementation.
(You do not need to fit these test cases into 80 columns.)

> w_test :: Statement
> w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

> w_fact :: Statement
> w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

When you are done, the following tests should pass:

> t4a :: Test 
> t4a = execS w_test Map.empty ~?= 
>         Map.fromList [("X",IntVal 0),("Y",IntVal 10)]

> t4b :: Test
> t4b = execS w_fact Map.empty ~?=
>         Map.fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]

~~~~~{.haskell}
ghci> run w_test
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> run w_fact
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~

Credit: Original version of Problem 4 from [UCSD](http://cseweb.ucsd.edu/classes/wi11/cse230/homeworks/hw3.html)
