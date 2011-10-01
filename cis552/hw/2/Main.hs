-- Advanced Programming, HW 2
-- by <YOUR NAME HERE> <pennid>
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-} 

module Main where
import Prelude hiding (takeWhile,all)
import Test.HUnit      -- unit test support
import Graphics.Gloss  -- graphics library for problem 1
import XMLTypes        -- support file for problem 2 (provided)
import Play            -- support file for problem 2 (provided)
import Data.List (subsequences,elemIndex)

doTests :: IO ()
doTests = do 
  _ <- runTestTT $ TestList [ test0, test1, test2, test3, test4 ]
  return ()

main :: IO ()
main = do 
       doTests
       drawCircles   --- graphics demo, change with drawTree for your HW
       return ()

-- | a basic tree data structure
data Tree a = Tip | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Tip     = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Tip (\x t1 t2 -> Branch (f x) t1 t2) 

-- 0 (a)

-- invert tree returns a tree with each pair reversed. 
-- for example:
--   invert (Branch ("a",1) Tip Tip) returns Branch (1,"a") Tip Tip

invert :: Tree (a, b) -> Tree (b, a)
invert = mapTree (\(x, y) -> (y, x)) 

t0a :: Test
t0a = "0a" ~: TestList [invert (Branch ("a", 1) Tip Tip) ~?= Branch (1,"a") Tip Tip,
                        invert (Tip::Tree (Int,Char)) ~?= (Tip::Tree (Char,Int)),
                        invert (Branch ("z", 2) (Branch ("s", 5) Tip Tip) Tip) ~?=
                                Branch (2, "z") (Branch (5, "s") Tip Tip) Tip]
 

-- 0 (b)

-- takeWhileTree, applied to a predicate p and a tree t, 
-- returns the longest subtree (possibly empty) of t of elements 
-- that satisfy p:
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Tip Tip) (Branch 3 Tip Tip)

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree p = foldTree Tip (\a n1 n2 -> if (p a) 
                                            then Branch a n1 n2 
                                            else Tip)

--     takeWhileTree (< 3) tree1  returns Branch 1 (Branch 2 Tip Tip) Tip
--     takeWhileTree (< 9) tree1  returns tree1
--     takeWhileTree (< 0) tree1  returns Tip

t0b :: Test
t0b = "0b" ~: TestList [takeWhileTree (< 3) tree1 ~?= Branch 1 
                                                      (Branch 2 Tip Tip) Tip,
                        takeWhileTree (< 9) tree1  ~?= tree1,
                        takeWhileTree (< 0) tree1  ~?= Tip]
 

-- 0 (c) 
 
-- allTree pred tree returns False if any element of tree 
-- fails to satisfy pred and True otherwise.
-- for example:
--    allTree odd tree1 returns False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = foldTree True (\a n1 n2 -> (p a) && n1 && n2)

t0c :: Test
t0c = "0c" ~: TestList [allTree odd tree1 ~?= False,
                        allTree (<9) tree1 ~?= True]
 

-- 0 (d)

-- map2Tree f xs ys returns the list obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one branch is longer than the other, then the extra elements 
-- are ignored.
-- for example:
--    map2Tree (+) (Branch 1 Tip (Branch 2 Tip Tip)) (Branch 3 Tip Tip)
--        should return (Branch 4 Tip Tip)

map2Tree :: (a->b->c) -> Tree a -> Tree b -> Tree c
map2Tree f (Branch x l1 r1) (Branch y l2 r2) = Branch (f x y) (map2Tree f l1 l2) 
                                                              (map2Tree f r1 r2)
map2Tree _ Tip _ = Tip
map2Tree _ _ Tip = Tip

-- attempt
--map2Tree f  = 
--     foldTree Tip (helper)
--     where helper (Branch x l1 r1) (Branch y l2 r2) = Branch (f x y) (l1 r1) (l2 r2)
          
--   where helper :: Tree a -> Tree b -> Tree c


--map2Tree :: (a->b->c) -> Tree a -> Tree b -> Tree c

--foldTree::Tree c ->(?->Tree c->Tree c->Tree c) -> Tree ? -> Tree c



t0d :: Test
t0d = "0d" ~: TestList [map2Tree (+) (Branch 1 Tip (Branch 2 Tip Tip)) 
                                     (Branch 3 Tip Tip) ~?= Branch 4 Tip Tip,
                        map2Tree (-) Tip (Branch 3 Tip Tip) ~?= Tip] 

-- 0 (e) 

-- zip takes two trees and returns a tree of corresponding pairs. If
-- one input branch is smaller, excess elements of the longer branch are
-- discarded.
-- for example:  
--    zipTree (Branch 1 (Branch 2 Tip Tip) Tip) (Branch True Tip Tip) returns 
--            (Branch (1,True) Tip Tip)

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree = map2Tree (,)

t0e :: Test
t0e = "0e" ~: TestList [zipTree (Branch 1 (Branch 2 Tip Tip) Tip) 
                                (Branch True Tip Tip) ~?= (Branch (1,True) Tip Tip),
                        zipTree (Tip::Tree Int) (Branch True Tip Tip) 
                            ~?= (Tip::Tree (Int,Bool))]

test0 :: Test
test0 = TestList [ t0a, t0b, t0c, t0d, t0e ]

-- | a picture composed of concentric circles
circles :: Picture
circles = pictures (reverse colorCircles) where
   -- a list of circle pictures with increasing radii
   bwCircles :: [Picture]
   bwCircles = map (\f -> circleSolid (25.0 * f)) [1.0 ..]  
   -- a list of builtin colors
   colors :: [Color]
   colors    = [red, blue, green, cyan, magenta, yellow]
   -- a list of colored circles
   colorCircles :: [Picture]
   colorCircles = zipWith color colors bwCircles

-- | draw the concentric circle picture in a window
-- this variable is an "action" of type IO (). Running the action
-- in the main program will open a window (of size (600,600)) and 
-- display the circles.

drawCircles :: IO ()
drawCircles = displayInWindow "Circles" (600,600) (10,10) black circles

-- | a right triangle at position `x` `y` with side length `size`
triangle :: Float -> Float -> Float -> Picture
triangle x y size = line [(x,y), (x+size, y), (x, y-size), (x,y)]

minSize :: Float
minSize = 8.0

-- | a sierpinski triangle
sierpinski :: Float -> Float -> Float -> [ Picture ]
sierpinski x y size = 
  if size <= minSize
  then [ triangle x y size ] 
  else let size2 = size / 2 
       in sierpinski x y size2 ++ 
          sierpinski x (y-size2) size2 ++
          sierpinski (x+size2) y size2

-- | the action to draw the triangle on the screen
drawSierpinski :: IO ()
drawSierpinski = 
   displayInWindow "Sierpinski" (600,600) (10,10) white sierpinskiPicture where
      sierpinskiPicture =  (color blue (pictures (sierpinski 0 0 256)))

-- 1 (a)

calcSize :: Float -> Float -> Tree Float
calcSize r s | s < minSize = Tip
             | otherwise = Branch s (calcSize r (s*r)) (calcSize r (s*r))

t1a :: Test
t1a = "1a" ~: calcSize 0.5 25 ~=?
         Branch 25.0 (Branch 12.5 Tip Tip) (Branch 12.5 Tip Tip)

-- 1 (b)

fractal :: Float -> Float -> Float -> Float -> Tree Float -> Tree Picture
fractal _ _ _ _ Tip                      = Tip
fractal delta angle x y (Branch a n1 n2) = Branch (Line[(x,y), (x', y')])
                                           (fractal delta (angle+delta) x' y' n1)
                                           (fractal delta (angle-delta) x' y' n2)
                                           where x' = a*(cos angle) + x
                                                 y' = a*(sin angle) + y


t1b :: Test
t1b = "1b" ~: fractal (pi/2) 0 0 0 (calcSize 0.5 25) ~=? 
               Branch (Line [(0.0,0.0),(25.0,0.0)]) 
                  (Branch (Line [(25.0,0.0),(25.0,12.5)]) Tip Tip) 
                  (Branch (Line [(25.0,0.0),(25.0,-12.5)]) Tip Tip)

-- 1 (c) 

join :: Tree Picture -> Picture
join = foldTree Blank (\a t1 t2 -> pictures ([a] ++ [t1] ++ [t2]))

t1c :: Test
t1c = "1c" ~: join (Branch Blank Tip Tip) ~?= Pictures [Blank, Blank, Blank]

-- | create a fractal tree with some initial parameters. Try changing 
-- some of these values to see how that changes the tree. In particular, 
-- try changing the delta for the angle of the branches (initially pi/6 below).
fractalTree :: Picture
fractalTree = color blue (join (fractal (pi/6) (pi/2) 0 0 sizeTree)) where
   sizeTree = calcSize 0.6 150.0

drawTree :: IO ()
drawTree = displayInWindow "MyWindow" (700,700) (10,10) white fractalTree

test1 :: Test
test1 = TestList [t1a,t1b,t1c]

-- 2 

formatPlay :: SimpleXML -> SimpleXML
--formatPlay _ = Element "body" [Element "h1" [PCDATA "TITLE"], Element "h2" [PCDATA "Drama"], PCDATA "PERSON1", Element "br" []]

formatPlay (PCDATA xs) = PCDATA xs
formatPlay (Element _ xs) = Element "html" [Element "body" res] --checked
                         where res :: [SimpleXML]
                               res = concatMap convertChild xs  

convertChild :: SimpleXML -> [SimpleXML]
convertChild (PCDATA x) = [PCDATA x]
convertChild (Element eName xs) = 
                     case eName of 
                                "TITLE"    -> let xs' = xs in [Element "h1" res]     
                                "PERSONAE" -> let xs' = xs in [Element "h2" [PCDATA "Dramatis Personae"]]++res
                                "PERSONA"  -> let xs' = xs in xs'++[Element "br" []]
                                "ACT"      -> let x = head xs 
                                                  xs'=tail xs 
                                                  in [Element "h2" [PCDATA (renderValue $ head $ getChild x)]]++res
                                _  -> []
                     where res::[SimpleXML]
                           res = concatMap convertChild xs'

renderValue :: SimpleXML -> String
renderValue (PCDATA xs) = xs
renderValue (Element eName _) = eName

getChild :: SimpleXML -> [SimpleXML]
getChild (PCDATA _) = []
getChild (Element _ xs) = xs


firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
    | c==d = firstDiff cs ds 
    | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

-- | Test the two files, character by character to determine whether they
-- match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs,ds) -> assertFailure msg where
      msg  = "Results differ: '" ++ (take 20 cs) ++ 
            "' vs '" ++ (take 20 ds)
 

test2 :: Test
test2 = TestCase $ do 
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

-- 3 (a)

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse :: a -> [a] -> [a]
intersperse a = foldr step []
                where step x [] = [x]
                      step x xs = x:a:xs
t3a :: Test
t3a = "3a" ~: TestList [intersperse 'a' [] ~?= [],
                        intersperse ',' "abcde" ~?= "a,b,c,d,e"]


-- 3 (b)

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

invert' :: [(a,b)] -> [(b,a)]
invert' = map (\(x,y) -> (y,x))

t3b :: Test
t3b = "3b" ~: (invert' [("a",1),("a",2)]) ~?= [(1,"a"),(2,"a")]
 

-- 3 (c)

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a->Bool)->[a]->[a]
takeWhile p = foldr (\x xs -> if (p x) then x:xs
                                       else []) []

t3c :: Test
t3c = "3c" ~: TestList [takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2],
                        takeWhile (< 9) [1,2,3] ~?= [1,2,3],
                        takeWhile (< 0) [1,2,3] ~?= []]
 

-- 3 (d)

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find :: (a->Bool) -> [a] -> Maybe a
find p = foldr (\x xs -> if (p x) then Just x
                                  else xs) Nothing

t3d :: Test
t3d = "3d" ~: TestList [find odd  [0,2,3,4] ~?= Just 3,
                        find (>9) [0,2,3,4] ~?= Nothing]
 

-- 3 (e)

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all :: (a->Bool)->[a]->Bool
all p = foldr (\x xs -> (p x) && xs) True

t3e :: Test
t3e = "3e" ~: TestList [all odd  [1,2,3]   ~?= False,
                        all (>0) [1,2,3,4] ~?= True]
 

test3 :: Test
test3 = TestList [t3a, t3b, t3c, t3d, t3e]

lcs :: String -> String -> String 
lcs xs ys = let xss = subsequences xs
                yss = subsequences ys
            in
            foldr (\x res -> if (elemIndex x yss == Nothing)
                             then res
                             else if (length x > length res)
                                  then x
                                  else res ) 
                  "" 
                  xss

{-lcs xs ys = aux (subsequences xs) (subsequences ys)
          where aux :: [String]->[String]->String
                aux [] _          = ""
                aux (ss:ssx) ss'x = let ss' = aux ssx ss'x in
                                    if (elemIndex ss ss'x == Nothing)
                                    then ss'
                                    else if (length ss) > (length ss')
                                         then ss
                                         else ss'
-}
test4 :: Test
test4 = "4" ~: TestList [ lcs "Advanced" "Advantaged" ~?= "Advaned" ]

test4a :: Test
test4a = "4a" ~: lcs "abcd" "acbd" ~?= "acd"

test4b :: Test
test4b = "4b" ~: lcs "abcd" "acbd" ~?= "abd"



