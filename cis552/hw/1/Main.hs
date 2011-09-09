-- Advanced Programming, HW 1
-- by Zi Yan

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude hiding (all, reverse, takeWhile, zip)
import Test.HUnit 

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ test0, test1, test2, test3 ]
   return ()

-- Part 0 (a)
abc:: Bool->Bool->Bool->Bool
abc x y z =
  x && (y || (x && z))
 
 
t0a :: Test
t0a = "0a1" ~: TestList [abc True False True ~?= True, 
                         abc True False False ~?= False,
                         abc False True True ~?= False]

-- 0 (b)

arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =
           (b*f - c*e, c*d - a*f, a*e - b*d)
 

t0b :: Test
t0b = "0b" ~: TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3), 
                        arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

-- 0 (c)

cmax :: [Int] -> Int -> Int
cmax l t 
 = g (length l - 1) t 
     where g n t = if n < 0 then t else 
                   if (l !! n) > t then g (n-1) (l !! n) else g (n-1) t
 
                

t0c :: Test
t0c ="0c" ~: TestList[ cmax [1,4,2] 0 ~?= 4, 
                       cmax []      0 ~?= 0,
                       cmax [5,1,5] 0 ~?= 5 ]

-- 0 (d)
 
reverse l  = reverse_aux l [] where
  reverse_aux l acc = 
    if null l then acc
       else reverse_aux (tail l) (head l : acc) 
 

t0d :: Test
t0d = "0d" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                        reverse [1]     ~?= [1] ]

test0 :: Test
test0 = "test0" ~: TestList [ t0a , t0b, t0c, t0d ]


-- Part 1 (a)  

toDigits :: Integer -> [Integer]
toDigits = error "unimplemented"

toDigitsRev :: Integer -> [Integer]
toDigitsRev = error "unimplemented"

t1a :: Test
t1a = "1a" ~: toDigitsRev 1234 ~?= [4,3,2,1]

-- 1 (b)  

doubleEveryOther :: [Integer] -> [Integer] 
doubleEveryOther = error "unimplemented"

t1b :: Test
t1b = "1b" ~: doubleEveryOther [8,7,6,5] ~?= [8,14,6,10]

-- 1 (c) 

sumDigits :: [Integer] -> Integer
sumDigits = error "unimplemented"

t1c :: Test
t1c = "1c" ~: sumDigits[8,14,6,10] ~?= 20

-- 1 (d) 

validate :: Integer -> Bool
validate = error "unimplemented"

t1d :: Test
t1d = "1d" ~: validate 4012888888881881 ~?= True

test1 :: Test
test1 = TestList [ t1a, t1b, t1c, t1d ]

-- 2a)

-- | The conv function takes two lists of numbers, reverses the 
-- second list, multiplies their elements together pointwise, and sums
-- the result.  This function assumes that the two input
-- lists are the same length.
 
conv :: [Int] -> [Int] -> Int
conv xs ys = error "conv: unimplemented"

t2a :: Test
t2a = "2a" ~: conv [2,4,6] [1,2,3] ~?= 20

-- 2b) 

-- | The normalize function adds extra zeros to the beginning of each
-- list so that they each have length 2n + 1, where n is 
-- the length of the longer number.   
 
normalize :: [Int] -> [Int] -> ([Int], [Int])
normalize = error "normalize: unimplemented"

t2b :: Test
t2b = "2b" ~: normalize [1] [2,3] ~?= ([0,0,0,0,1], [0,0,0,2,3])

-- 2c)

-- | multiply two numbers, expressed as lists of digits using 
-- the Ūrdhva Tiryagbhyām algorithm.
 
multiply :: [Int] -> [Int] -> [Int]
multiply = error "unimplemented"

t2c :: Test
t2c = "2c" ~: multiply [2,4,6][1,2,3] ~?= [0,0,3,0,2,5,8]

-- 2d) OPTIONAL CHALLENGE PROBLEM 

convAlt :: [Int] -> [Int] -> Int
convAlt = error "unimplemented"

t2d :: Test
t2d = "2d" ~: convAlt [2,4,6][1,2,3] ~=? 20

test2 :: Test
test2 = TestList [t2a,t2b,t2c,t2d]

test3 :: Test
test3 = "test3" ~: TestList [t3a, t3b, t3c, t3d, t3e, t3f, t3g, t3h]

-- 3 (a)

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

t3a :: Test
t3a = "3a" ~: assertFailure "testcase for 4a"


-- 3 (b)

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

t3b :: Test
t3b = "3b" ~: assertFailure "testcase for 4b"
 

-- 3 (c)

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

t3c :: Test
t3c = "3c" ~: assertFailure "testcase for 4c"
 

-- 3 (d)

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

t3d :: Test
t3d = "3d" ~: assertFailure "testcase for 4d"
 

-- 3 (e)

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

t3e :: Test
t3e = "3e" ~: assertFailure "testcase for 4e"
 

-- 3 (f)

-- map2 f xs ys returns the list obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one list is longer than the other, then the extra elements 
-- are ignored.
-- i.e. 
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1] 
--        returns [f x1 y1, f x2 y2, ..., f xn yn]



t3f :: Test
t3f = "3f" ~: assertFailure "testcase for 4f"

-- 3 (g)

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:  
--    zip [1,2] [True] returns [(1,True)]



t3g :: Test
t3g = "3g" ~: assertFailure "testcase(s) for zip"

-- 3 (h)  WARNING this one is tricky!

-- The transpose function transposes the rows and columns of its argument. 
-- If the inner lists are not all the same length, then the extra elements
-- are ignored.
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]



t3h :: Test
t3h = "3h" ~: assertFailure "testcase for 4h"

