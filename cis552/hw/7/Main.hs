-- by Christian DeLozier <delozier> and Zi Yan <yanzi>
 
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where

import Test.QuickCheck
import Control.Monad.Error

data AVL e = E           -- empty tree
           | N           -- non-empty tree
               Int           -- balance factor 
               Int           -- height
               (AVL e)       -- left subtree
               e             -- value
               (AVL e)       -- right subtree
  deriving Show


instance Eq e => Eq (AVL e) where
    E == E = True
    (N b1 h1 l1 v1 r1) == (N b2 h2 l2 v2 r2) 
     = b1 == b2 && h1 == h2 && l1 == l2 &&
       v1 == v2 && r1 == r2
    _ == _ = False

-- | an empty AVL tree
avlEmpty :: AVL e
avlEmpty = E

-- | create a new AVL tree with the given element and left and 
-- right subtrees
node :: AVL e -> e -> AVL e -> AVL e
node l x r = N bf ht l x r where
  ht = 1 + max (height l) (height r)
  bf = height l - height r

-- | Access the height component of the AVL tree
height :: AVL e -> Int
height E              = 0
height (N _ ht _ _ _) = ht

-- | Determine if an element is contained within the tree
avlLookup :: Ord e => e -> AVL e -> Bool
avlLookup _ E                          = False
avlLookup a (N _ _ l x r) | a == x     = True
                          | a < x      = avlLookup a l
                          | otherwise  = avlLookup a r  

-- | The height at each node is correctly calculated. 
prop_ht :: Show e => AVL e -> Either String ()
prop_ht E = Right ()
prop_ht (N _ ht l x r) = do _ <- prop_ht l
                            _ <- prop_ht r
                            if ht == (max ((height l) + 1) ((height r) + 1))
                              then Right () else throwError errorMsg
  where
    errorMsg = "Node with value " ++ (show x) 
                            ++ " has height " ++ (show ht) ++ 
                            " but left sub-tree has height " ++ 
                            (show $ height l) ++ 
                            " and right sub-tree has height " ++ 
                            (show $ height r)

-- | The balance factor at each node is correctly calculated.  
prop_bf :: Show e => AVL e -> Either String ()
prop_bf E = Right ()
prop_bf (N bf _ l x r) = do _ <- prop_bf l
                            _ <- prop_bf r
                            if bf == (height l) - (height r) then
                              Right () else throwError errorMsg
  where
    errorMsg = "Node with value " ++ (show x) ++ " has balance factor " 
               ++ (show bf) ++ " but left sub-tree has height " ++ 
               (show $ height l) ++ " and right sub-tree has height " ++ 
               (show $ height r)
  
-- | The balance factor at each node is between -1 and +1.  
prop_balance :: Show e => AVL e -> Either String ()    
prop_balance E = Right ()
prop_balance (N bf _ l x r) = do _ <- prop_balance l
                                 _ <- prop_balance r
                                 if (abs bf) <= 1 
                                    && (abs (height l - height r)) <= 1 then
                                   Right () else throwError errorMsg
  where
    errorMsg = "Node with value " ++ (show x) ++
               " has balance factor " ++ (show bf)

isOrdered :: (Ord e) => AVL e -> e -> AVL e -> Bool
isOrdered (N _ _ _ a _) b (N _ _ _ c _) = (a < b) && (b < c)
isOrdered (N _ _ _ a _) b E             = a < b
isOrdered E             b (N _ _ _ c _) = b < c
isOrdered E             _ E             = True

-- | The items stored in the tree are in strictly increasing order.
prop_inorder :: (Ord e, Show e) => AVL e -> Either String ()
prop_inorder E = Right ()
prop_inorder (N _ _ l x r) = do _ <- prop_inorder l
                                _ <- prop_inorder r
                                if isOrdered l x r
                                   then Right () else throwError errorMsg
  where
    errorMsg = "Node with value " ++ (show x) ++ " is out-of-order"

tVal :: AVL e -> Maybe e
tVal E = Nothing
tVal (N _ _ _ x _) = Just x

t0 :: AVL Int
t0 = E

t1 :: AVL Int
t1 = N 0 1 E 5 E

t2 :: AVL Int
t2 = N 1 2 (N 0 1 E 2 E) 5 E

t3 :: AVL Int
t3 = N (-1) 3 (N 0 1 E 1 E) 2 (N 1 2 (N 0 1 E 3 E) 4 E)

-- Exhibits incorrect height
bad1 :: AVL Int
bad1 = N 1 1 (N 0 1 E 5 E) 6 E

-- Exhibits incorrect balance factor
bad2 :: AVL Int
bad2 = N 0 2 (N 0 1 E 5 E) 6 E

-- Exhibits balance factor outside of [-1,1]
bad3 :: AVL Int
bad3 = N 2 3 (N 1 2 (N 0 1 E 1 E) 2 E) 3 E

-- Exhibits out-of-order tree
bad4 :: AVL Int
bad4 = N 0 2 (N 0 1 E 5 E) 4 (N 0 1 E 6 E)

-- | Check all invariants of an AVL tree
check :: (Ord e, Show e) => AVL e -> Either String ()
check x = do { prop_ht x; prop_bf x; prop_balance x; prop_inorder x}

main :: IO ()
main = do { aux (check t0); aux (check t1); 
            aux (check t2); aux (check t3); 
            aux (check bad1); aux (check bad2); 
            aux (check bad3); aux (check bad4) }
  where
    aux r = case r of
      Right () -> return ()
      Left  s  -> putStrLn $ "Error: " ++ s

rebalance :: (Ord e) => AVL e -> AVL e
rebalance (N 2 h5 (N 1 h4 st3 v4 c) v5 d)
  = N 0 h4 st3 v4 (N 0 (h5-2) c v5 d)
rebalance (N 2 h5 (N (-1) h3 a v3 (N _ h4 b v4 c)) v5 d)
  = N 0 (h4+1) (N b3' (h3-1) a v3 b) v4 (N b5' (h5-2) c v5 d)
    where b3' = (height a) - (height b)
          b5' = (height c) - (height d)
rebalance (N (-2) h3 a v3 (N (-1) h4 b v4 st5))
  = N 0 h4 (N 0 (h3-2) a v3 b) v4 st5 
rebalance (N (-2) h3 a v3 (N 1 h5 (N _ h4 b v4 c) v5 d))
  = N 0 (h4+1) (N b3' (h3-2) a v3 b) v4 (N b5' (h5-1) c v5 d)
    where b3' = (height a) - (height b)
          b5' = (height c) - (height d) 
rebalance t = t

-- | Insert a new element into a tree, returning a new tree
avlInsert :: (Ord e) => e -> AVL e -> AVL e
avlInsert x E = N 0 1 E x E
avlInsert x s@(N _ _ l y r)
         | x < y = rebalance (node (avlInsert x l) y r)
         | x > y = rebalance (node l y (avlInsert x r))
         | otherwise = s 

avlIn :: Ord e => e -> AVL e -> Bool
avlIn _ E = False
avlIn x (N _ _ l y r)
  | x < y     = avlIn x l
  | x > y     = avlIn x r
  | otherwise = True

avlElem :: Ord e => AVL e -> [e]
avlElem t = aux t []
 where aux E xs = xs
       aux (N _ _ l y r) xs = aux l (y: aux r xs) 

checkInsert :: (Show e, Ord e) => e -> AVL e -> Either String (AVL e)
checkInsert x avl = let res  = avlInsert x avl
                        ele  = x:avlElem avl
                        test = check res
                    in case test of
                            Left st  -> Left st
                            Right () -> if all (\a -> avlIn a res) ele
                                        then Right res
                                        else Left "inserted element is missing"

prop_insert :: (Show e, Ord e) => [e] -> Bool
prop_insert xs = let avl = foldr (\x res -> avlInsert x res) avlEmpty xs
                     test = foldr step (Right avlEmpty) xs
                 in case test of 
                         Left _ -> False
                         Right a -> a == avl
  where step _ (Left a) = Left a
        step x (Right avl) = checkInsert x avl 

avlDelete :: Ord e => e -> AVL e -> AVL e
avlDelete _ E = E
avlDelete x (N _ _ E y E) | x == y = E
                            | otherwise = error "not a member"
avlDelete x s@(N _ _ l y r)
         | x < y = rebalance (node (avlDelete x l) y r)
         | x > y = rebalance (node l y (avlDelete x r))
         | otherwise = let res = findDelSucc s
                       in case res of 
                               Nothing -> error "wrong place"
                               Just s' -> rebalance s'

-- | find and delete the successor of the root of AVL e
-- return the successor's value as well, including some
-- other corner cases, e.g. either child is empty
findDelSucc :: Ord e => AVL e -> Maybe (AVL e)
findDelSucc E = Nothing
findDelSucc (N _ _ E _ rt) = Just rt
findDelSucc (N _ _ lt _ E) = Just lt
findDelSucc (N _ _ lt _ rt) = let (rt', vres) = aux rt
                              in Just $ node lt vres rt'
 where aux (N _ _ l v r) | l == E && r == E = (E, v)
                         | l == E && r /= E = (r, v)
                         | otherwise = let (l', v') = aux l
                                       in (node l' v r, v')
       aux E = error "wrong place"

prop_del :: (Show e, Ord e) => e -> [e] -> Bool
prop_del x xs = 
         not (avlLookup x 
              (avlDelete x (avlInsert x 
                            (foldr (\s res -> avlInsert s res) avlEmpty xs))))

quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

genAVL :: Gen (AVL Int)
genAVL = frequency [(1, return E),
                    (2, liftM2 avlInsert arbitrary genAVL)]

instance Arbitrary (AVL Int) where
  arbitrary = genAVL

prop_AVL :: AVL Int -> Bool
prop_AVL t = case check t of
  Right () -> True
  Left  _  -> False