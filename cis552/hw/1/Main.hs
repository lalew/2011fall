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

cmax [] t = t
cmax l t
     = cmax (tail l) (if (head l > t) 
                      then head l
                      else t)
                

t0c :: Test
t0c ="0c" ~: TestList[ cmax [1,4,2] 0 ~?= 4, 
                       cmax []      0 ~?= 0,
                       cmax [5,1,5] 0 ~?= 5 ]

-- 0 (d)

reverse :: [a] -> [a] 

reverse [] = []
reverse (hd:tl) = reverse tl ++ [hd]

--reverse l  = reverse_aux l [] where
--  reverse_aux l acc = 
--    if null l then acc
--       else reverse_aux (tail l) (head l : acc) 
 

t0d :: Test
t0d = "0d" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                        reverse [1]     ~?= [1] ]

test0 :: Test
test0 = "test0" ~: TestList [ t0a , t0b, t0c, t0d ]


-- Part 1 (a)  

toDigits :: Integer -> [Integer]

toDigits num = if num >= 10 
               then toDigits (div num 10) ++ [mod num 10]
               else [num]

toDigitsRev :: Integer -> [Integer]

toDigitsRev num = if num >= 10 
                  then [mod num 10] ++ toDigitsRev (div num 10)
                  else [num]

t1a :: Test
t1a = "1a" ~: toDigitsRev 1234 ~?= [4,3,2,1]

-- 1 (b)  

doubleEveryOther :: [Integer] -> [Integer] 
doubleEveryOther l = dHelper l 0
                     where dHelper [] _ = []
                           dHelper (hd:tl) sign = if sign == 0
                                                then [hd]++dHelper tl 1
                                                else [2*hd] ++ dHelper tl 0

t1b :: Test
t1b = "1b" ~: doubleEveryOther [8,7,6,5] ~?= [8,14,6,10]

-- 1 (c) 

sumDigits :: [Integer] -> Integer

sumDigits [] = 0
sumDigits (hd:tl) = if hd >= 10 then sumDigits (toDigits hd) + sumDigits tl
                                else hd + sumDigits tl

t1c :: Test
t1c = "1c" ~: sumDigits[8,14,6,10] ~?= 20

-- 1 (d) 

validate :: Integer -> Bool
validate num = mod (sumDigits (doubleEveryOther (toDigitsRev num))) 10 == 0

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
conv [] [] = 0
conv [] _ = error "unmatched list"
conv _ [] = error "unmatched list"
conv xs ys = (head xs)*(last ys) --was (head xs)*(head (reverse ys))
           + conv (tail xs) (reverse (tail (reverse ys)))

t2a :: Test
t2a = "2a" ~: conv [2,4,6] [1,2,3] ~?= 20

-- 2b) 

-- | The normalize function adds extra zeros to the beginning of each
-- list so that they each have length 2n + 1, where n is 
-- the length of the longer number.   
 
normalize :: [Int] -> [Int] -> ([Int], [Int])
normalize xs ys = let lx = length xs
                      ly = length ys
                  in if lx > ly then (nHelper xs (2*lx+1 - lx), 
                                      nHelper ys (2*lx+1 - ly))
                                else (nHelper xs (2*ly+1 - lx), 
                                      nHelper ys (2*ly+1 - ly))
                  where nHelper str 0 = str
                        nHelper str n = [0] ++ nHelper str (n - 1)

t2b :: Test
t2b = "2b" ~: normalize [1] [2,3] ~?= ([0,0,0,0,1], [0,0,0,2,3])

-- 2c)

-- | multiply two numbers, expressed as lists of digits using 
-- the Ūrdhva Tiryagbhyām algorithm.
 
multiply :: [Int] -> [Int] -> [Int]
multiply xs ys = let (left, right) = normalize xs ys
                 in mHelper left right 0  (length left)  (length left - 1)
                 where mHelper _ _ _ _ 0 = [0]
                       mHelper l r c len i 
                        = let s = conv (drop i l) (drop i r)
                              z = (s + c) `mod` 10
                              c'= (s + c) `div` 10
                          in mHelper l r c' len (i - 1) ++ [z]
                          

t2c :: Test
t2c = "2c" ~: multiply [2,4,6][1,2,3] ~?= [0,0,3,0,2,5,8]

-- 2d) OPTIONAL CHALLENGE PROBLEM 

convAlt :: [Int] -> [Int] -> Int
convAlt [] [] = 0
convAlt xs ys = (head xs)*(last ys) 
           + convAlt (tail xs) (init ys) -- was (reverse (tail (reverse ys)))

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
intersperse' :: a->[a]->[a]
intersperse' _ [a] = [a]
intersperse' e list = [head list]
                         ++ [e] 
                         ++ (intersperse' e (tail list))

t3a :: Test
t3a = "3a" ~: intersperse' ',' "abcde" ~=? "a,b,c,d,e"


-- 3 (b)

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

invert' :: [(a,b)]->[(b,a)]

invert' [] = []
invert' lst = [(snd (head lst), fst (head lst))] ++ (invert' (tail lst))

t3b :: Test
t3b = "3b" ~: invert' [("a",1),("a",2)] ~=? [(1,"a"),(2,"a")] 
 

-- 3 (c)

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile' :: (a -> Bool)->[a]->[a]

takeWhile' _ [] = []
takeWhile' p lst = if p (head lst) 
                   then [head lst] ++ takeWhile' p (tail lst)
                   else []

t3c :: Test
t3c = "3c" ~: takeWhile' (< 3) [1,2,3,4,1,2,3,4] ~=? [1,2] 

 

-- 3 (d)

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find' :: (a->Bool)->[a]->Maybe a

find' _ [] = Nothing
find' p (hd:tl) = if p hd 
                 then Just hd
                 else find' p tl

t3d :: Test
t3d = "3d" ~: find' odd [0,2,6,4] ~=? Nothing
 

-- 3 (e)

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all' :: (a->Bool)->[a]->Bool

all' _ [] = True
all' p (hd:tl) = (p hd) && all' p tl


t3e :: Test
t3e = "3e" ~: all' odd [1,2,3] ~=? False
 

-- 3 (f)

-- map2 f xs ys returns the list obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one list is longer than the other, then the extra elements 
-- are ignored.
-- i.e. 
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1] 
--        returns [f x1 y1, f x2 y2, ..., f xn yn]

map2' :: (a->b->c)->[a]->[b]->[c]

map2' _ [] _ = []
map2' _ _ [] = []
map2' f xs ys = [f (head xs) (head ys)] ++ map2' f (tail xs) (tail ys)

t3f :: Test
t3f = "3f" ~: map2' (*) [1,2,3] [4,5,6] ~=? [4,10,18]

-- 3 (g)

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:  
--    zip [1,2] [True] returns [(1,True)]

zip' :: [a]->[b]->[(a,b)]

zip' [] _ = []
zip' _ [] = []
zip' xs ys = [(head xs, head ys)] ++ zip' (tail xs) (tail ys)


t3g :: Test
t3g = "3g" ~:  zip' [1,2,4] [True,False] ~=? [(1,True),(2,False)]

-- 3 (h)  WARNING this one is tricky!

-- The transpose function transposes the rows and columns of its argument. 
-- If the inner lists are not all the same length, then the extra elements
-- are ignored.
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]

transpose' ::[[a]]->[[a]]

transpose' [] = []
transpose' lst = if all' (\l-> not (null l)) lst
                 then [headers lst] ++ transpose' (tails lst)
                 else [[]]
-- provide all the first element in each list in the outter list
headers :: [[a]]->[a]

headers [] = []
headers lst = if all' (\l-> not (null l)) lst 
              then [head (head lst)] ++ headers (tail lst)
              else []

-- provide all the rest elements in each list in the outter list
tails ::[[a]]->[[a]]

tails [] = []
tails lst = if all' (\l-> not (null (tail l))) lst
            then [tail (head lst)] ++ tails (tail lst)
            else []

t3h :: Test
t3h = "3h" ~: transpose' [[1,2,3],[4,5,6]] ~=? [[1,4],[2,5],[3,6]]
