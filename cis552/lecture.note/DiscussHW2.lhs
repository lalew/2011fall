HW 2 - Datatypes and Trees
==========================

Due Monday, October 3, at noon
------------------------------

> -- Advanced Programming, HW 2
> -- by <YOUR NAME HERE> <pennid> 

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

> {-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-} 

The goal of this homework assignment is practice with
user-defined datatypes and trees in Haskell. 

This homework is composed of three files, two support files
[XMLTypes.hs](XMLTypes.hs) and [Play.hs](Play.hs), and the main part
of the assignment. Furthermore, for testing, you will also need the 
file [sample.html](sample.html). To complete the homework, you should only edit the
file [Main.hs](Main.hs), and submit only it through the [course web
page](https://alliance.seas.upenn.edu/~cis552/cgi-bin/submit.php).

> module Main where
> import Prelude hiding (takeWhile,all)
> import Test.HUnit      -- unit test support
> import Graphics.Gloss  -- graphics library for problem 1
> import XMLTypes        -- support file for problem 2 (provided)
> import Play            -- support file for problem 2 (provided)

> import Control.Applicative

To compile this assignment you will first need to install the
[Gloss](http://hackage.haskell.org/packages/archive/gloss/1.3.4.1/doc/html/Graphics-Gloss.html)
graphics library. You can do so using the command:

       cabal install gloss

Because one problem in this assignment (problem 1 below) involves
graphics, the main method for this assignment both runs the tests for
each problem and draws a picture on the screen. To kill the picture, 
press the `Esc` key.

You will be able to run the unit tests from ghci after the XMLTypes
and Play modules have been compiled. However, ghci may not be able to
display the picture, so to see the results of the graphics you will
need to compile this file on the command line with the following
command:

       ghc --make Main.hs

After the file compiles, you can run the resulting executable (named
`Main`, `a.out` or `Main.exe` depending on your system).

> doTests :: IO ()
> doTests = do 
>   _ <- runTestTT $ TestList [ test0, test1, test2, test3, test4 ]
>   return ()

> main :: IO ()
> main = do 
>        doTests
>        -- drawCircles   --- graphics demo, change with drawTree for your HW
>        return ()

Problem 0 - Simple tree-based processing
----------------------------------------

This first problem involves writing some library functions for a simple
tree data structures, like we discussed in class. The following datatype
defines a binary tree, storing data at each internal node.

> -- | a basic tree data structure
> data Tree a = Tip | Branch a (Tree a) (Tree a) deriving (Show, Eq)

Now consider definitions of fold and map for this data structure.

> foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
> foldTree e _ Tip     = e
> foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

> mapTree :: (a -> b) -> Tree a -> Tree b
> mapTree f = foldTree Tip (\x t1 t2 -> Branch (f x) t1 t2) 

Use these two functions to define the following operations over trees.

> -- 0 (a)

> -- invertTree returns a tree with each pair reversed. 
> -- for example:
> --   invertTree (Branch ("a",1) Tip Tip) returns Branch (1,"a") Tip Tip


> invert0 :: Tree (a,b) -> Tree (b,a)
> invert0 = mapTree (\(a,b) -> (b,a))

> 
> invertTree :: Tree (a,b) -> Tree (b,a)
> invertTree = mapTree swap where
>     swap (a,b) = (b,a)

> t0a :: Test
> t0a = "0a" ~: invertTree (Branch ("a",1) Tip Tip) ~?= Branch (1,"a") Tip Tip

> 

> -- 0 (b)

> -- takeWhileTree, applied to a predicate p and a tree t, 
> -- returns the longest subtree (possibly empty) of t of elements 
> -- that satisfy p:
> -- For example, given the following tree

> tree1 :: Tree Int
> tree1 = Branch 1 (Branch 2 Tip Tip) (Branch 3 Tip Tip)

> --     takeWhileTree (< 3) tree1  returns Branch 1 (Branch 2 Tip Tip) Tip
> --     takeWhileTree (< 9) tree1  returns tree1
> --     takeWhileTree (< 0) tree1  returns Tip

> 

> takeWhileTree0 :: forall a. (a -> Bool) -> Tree a -> Tree a
> takeWhileTree0 p = foldTree Tip (\ x t1 t2 -> trim p (Branch x t1 t2))
>   where
>     trim :: (b -> Bool) -> Tree b -> Tree b
>     trim _ Tip               = Tip
>     trim p' (Branch x t3 t4) | p' x      = Branch x t3 t4
>                              | otherwise = Tip
> 


> takeWhileTree :: forall a. (a -> Bool) -> Tree a -> Tree a
> takeWhileTree f = foldTree Tip trim where
>     trim :: a -> Tree a -> Tree a -> Tree a 
>     trim a t1 t2 | f a       = Branch a t1 t2
>                  | otherwise = Tip
> 

>
> t0b :: Test
> t0b = "0b"  ~: TestList [  takeWhileTree (< 3) tree1 ~?= 
>                               Branch 1 (Branch 2 Tip Tip) Tip,
>                            takeWhileTree (< 9) tree1 ~?= tree1,
>                            takeWhileTree (< 0) tree1 ~?= Tip ]

> 

> -- 0 (c) 
>  
> -- allTree pred tree returns False if any element of tree 
> -- fails to satisfy pred and True otherwise.
> -- for example:
> --    allTree odd tree1 returns False


> allTree0 :: (a -> Bool) -> Tree a -> Bool
> allTree0 f x = foldTree True (\o p q -> o && p && q) (mapTree f x)







> allTree :: (a -> Bool) -> Tree a -> Bool
> allTree f = foldTree True (\x t1 t2 -> f x && t1 && t2) 

> t0c :: Test
> t0c = "0c" ~:  (allTree odd tree1) ~?= False

> 

> -- 0 (d)

> -- map2Tree f xs ys returns the list obtained by applying f to 
> -- to each pair of corresponding elements of xs and ys. If 
> -- one branch is longer than the other, then the extra elements 
> -- are ignored.
> -- for example:
> --    map2Tree (+) (Branch 1 Tip (Branch 2 Tip Tip)) (Branch 3 Tip Tip)
> --        should return (Branch 4 Tip Tip)

> map2Tree0 f Tip _ = Tip
> map2Tree0 f _ Tip = Tip
> map2Tree0 f (Branch x t1 t2) (Branch y s1 s2) = 
>    Branch (f x y) (map2Tree0 f t1 s1) (map2Tree0 f s1 s2)








> map2Tree1 :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
> map2Tree1 _ _ _ = Tip -- o_O






> map2Tree2 :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
> map2Tree2 f x y = aux2 (mapTree f x) y   

> aux2 :: Tree (a -> b) -> Tree a -> Tree b
> aux2 Tip _                        = Tip
> aux2 _ Tip                        = Tip
> aux2 (Branch a b c)(Branch x y z) = 
>          Branch (a x) (aux2 b y)(aux2 c z)

> instance Functor Tree where
>    fmap = mapTree

> instance Applicative Tree where
>    -- pure :: a -> Tree a
>    pure x = Branch x (pure x) (pure x) 
>    (<*>) = aux2

> map1Tree  f t1       = pure f <*> t1
> map2Tree3 f t1 t2    = pure f <*> t1 <*> t2
> map3Tree  f t1 t2 t3 = pure f <*> t1 <*> t2 <*> t3








> map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
> map2Tree f t1 t2 = foldTree (\ _ -> Tip) br t1 t2 where
>    br x l r (Branch y s1 s2) = Branch (f x y) (l s1) (r s2)
>    br _ _ _ Tip = Tip
> 




> t0d :: Test
> 
> t0d = "0d" ~: map2Tree (+) (Branch 1 Tip Tip) (Branch 3 Tip Tip) ~?= (Branch 4 Tip Tip)
> 


> -- 0 (e) 

> -- zip takes two trees and returns a tree of corresponding pairs. If
> -- one input branch is smaller, excess elements of the longer branch are
> -- discarded.
> -- for example:  
> --    zipTree (Branch 1 (Branch 2 Tip Tip) Tip) (Branch True Tip Tip) returns 
> --            (Branch (1,True) Tip Tip)

> 
> zipTree0 :: Tree a -> Tree b -> Tree (a,b)
> zipTree0 t1 t2 = foldTree (const Tip) br t1 t2 where
>    br x l r (Branch y s1 s2) = Branch (x , y) (l s1) (r s2)
>    br _ _ _ Tip = Tip
> 

> zipTree = map2Tree (,)

> t0e :: Test
> 
> t0e = "0e" ~: zipTree (Branch 1 (Branch 2 Tip Tip) Tip) (Branch True Tip Tip)  
>               ~?= (Branch (1,True) Tip Tip)
> 

> test0 :: Test
> test0 = TestList [ t0a, t0b, t0c, t0d, t0e ]

Problem 1 - Fractal Trees
-------------------------

The Haskell library "Gloss" provides a simple way of creating
pictures. The first part of this problem are just a few demos of 
the drawing library. The actual problem starts below the 
line marked -- 1 (a).

A Gloss "Picture" is just an element of the following 
datatype:


       data Picture = 
          Blank                -- A blank picture, with nothing in it.
        | Polygon Path         -- A polygon filled with a solid color.
        | Line Path	       -- A line along an arbitrary path.
        | Circle Float         -- A circle with the given radius.
        | Text String          -- Some text
        | Color Color Picture  -- A picture drawn with this color.
            -- Note, there is no typo here. The name of the data constructor 
            -- is 'Color' and it takes an argument of type 'Color' as well          
            -- as one of type 'Picture'          
        | Translate Float Float Picture	
            -- A picture translated by the given x and y coordinates.
        | Rotate Float Picture	
            -- A picture rotated by the given angle (in degrees).
        | Scale Float Float Picture	
            -- A picture scaled by the given x and y factors.
        | Pictures [Picture]	
            -- A picture consisting of several others. 
        | .... etc

Pictures can be displayed with gloss using the command
`displayInWindow`. 

For example, we can create a picture by assembling several shapes
together and coloring them. The following definition creates an
aggregate picture of concentric circles of various colors.

> -- | a picture composed of concentric circles
> circles :: Picture
> circles = pictures (reverse colorCircles) where
>    -- a list of circle pictures with increasing radii
>    bwCircles :: [Picture]
>    bwCircles = map (\f -> circleSolid (25.0 * f)) [1.0 ..]  
>    -- a list of builtin colors
>    colors :: [Color]
>    colors    = [red, blue, green, cyan, magenta, yellow]
>    -- a list of colored circles
>    colorCircles :: [Picture]
>    colorCircles = zipWith color colors bwCircles

> -- | draw the concentric circle picture in a window
> -- this variable is an "action" of type IO (). Running the action
> -- in the main program will open a window (of size (600,600)) and 
> -- display the circles.

> drawCircles :: IO ()
> drawCircles = displayInWindow "Circles" (600,600) (10,10) black circles

As another example, we can define a function that creates a triangle 
picture from three lines. 

> -- | a right triangle at position `x` `y` with side length `size`
> triangle :: Float -> Float -> Float -> Picture
> triangle x y size = line [(x,y), (x+size, y), (x, y-size), (x,y)]

The next demo is Sierpinski’s Triangle – a fractal consisting of repeated drawing of a
triangle at successively smaller sizes. Note that we separate actually
drawing the picture from the pure computation of creating it.

> minSize :: Float
> minSize = 8.0

> -- | a sierpinski triangle
> sierpinski :: Float -> Float -> Float -> [ Picture ]
> sierpinski x y size = 
>   if size <= minSize
>   then [ triangle x y size ] 
>   else let size2 = size / 2 
>        in sierpinski x y size2 ++ 
>           sierpinski x (y-size2) size2 ++
>           sierpinski (x+size2) y size2

> -- | the action to draw the triangle on the screen
> drawSierpinski :: IO ()
> drawSierpinski = 
>    displayInWindow "Sierpinski" (600,600) (10,10) white sierpinskiPicture where
>       sierpinskiPicture =  (color blue (pictures (sierpinski 0 0 256)))

> -- 1 (a)

Your job is to create a picture of a fractal tree. A fractal tree is a
nested sequence of branches, where each branch is some percentange
smaller than its parent and contains two subtrees, both angling away
from the main branch at equal angles. 

An example drawing of a fractal tree is [here](fractalTree.png), where
each branch is 60% of the length of the parent and at an angle of 30
degrees.

![](fractalTree.png)

The first step is to create the tree by calculating the sizes of each
branch.  Given a ratio for the child sizes, and an initial size, the
output should be a tree data structure storing the size of each
branch.  Like the Sierpinkski triangle above, we'll cut off the tree
when the branch size gets below the minimum amount.

> calcSize :: Float -> Float -> Tree Float
> 
> calcSize ratio = aux where
>    aux size = 
>     if size <= minSize then 
>       Tip
>     else Branch size sub sub where 
>        sub = aux (size * ratio)
> 

> t1a :: Test
> t1a = "1a" ~: calcSize 0.5 25 ~=?
>          Branch 25.0 (Branch 12.5 Tip Tip) (Branch 12.5 Tip Tip)

> -- 1 (b)

The next step is to transform this tree of sizes into a tree of
pictures. We need a little trigonometry for that.  The function
fractal delta angle x y sizeTree should return a tree of pictures,
where the root of the tree starts at position (x,y) and draws a line
of the given angle. 

> fractal :: Float -> Float -> Float -> Float -> Tree Float -> Tree Picture
> 
> fractal delta = aux where
>    aux _ _ _ Tip = Tip
>    aux angle x y (Branch size left right) = 
>      Branch (line [(x,y), (x', y')]) 
>         (aux (angle + delta) x' y' left)
>         (aux (angle - delta) x' y' right)
>      where 
>         x' = x + (size * cos angle)
>         y' = y + (size * sin angle)
>  

> t1b :: Test
> t1b = "1b" ~: fractal (pi/2) 0 0 0 (calcSize 0.5 25) ~=? 
>                Branch (Line [(0.0,0.0),(25.0,0.0)]) 
>                   (Branch (Line [(25.0,0.0),(25.0,12.5)]) Tip Tip) 
>                   (Branch (Line [(25.0,0.0),(25.0,-12.5)]) Tip Tip)

> -- 1 (c) 

Finally, we need to convert the tree of pictures into a single picture.


> join0 :: Tree Picture -> Picture
> join0 Tip                = Blank
> join0 (Branch pic t1 t2) = Pictures [pic, join0 t1, join0 t2]









> join :: Tree Picture -> Picture
> 
> join = foldTree Blank (\p x y -> Pictures [p, x, y] )
> 

> t1c :: Test
> t1c = "1c" ~: join (Branch Blank Tip Tip) ~?= Pictures [Blank, Blank, Blank]

> -- | create a fractal tree with some initial parameters. Try changing 
> -- some of these values to see how that changes the tree. In particular, 
> -- try changing the delta for the angle of the branches (initially pi/6 below).
> fractalTree :: Picture
> fractalTree = color blue (join (fractal (pi/6) (pi/2) 0 0 sizeTree)) where
>    sizeTree = calcSize 0.6 150.0

> drawTree :: IO ()
> drawTree = displayInWindow "MyWindow" (700,700) (10,10) white fractalTree

> test1 :: Test
> test1 = TestList [t1a,t1b,t1c]

Problem 2 - Tree transformation
--------------------------------

> -- 2 

WARNING, this next problem requires some design as well as 
implementation.

This problem involves transforming XML document`s. To keep things
simple, we will not deal with the full generality of XML, or with
issues of parsing. Instead, we will represent XML documents as
instances of the following simplified type:

    data SimpleXML =
          PCDATA  String
        | Element ElementName [SimpleXML]

    type ElementName = String

That is, a SimpleXML value is either a PCDATA ("parsed character
data") node containing a string or else an Element node containing a
tag and a list of sub-nodes. 

The goal of this exercise is to write a transformation function
'formatPlay', which takes a play in an XML format *specific for plays*
and converts it to html (which is an XML format).

> 
> br :: SimpleXML
> br = Element "br" []

> formatPlay :: SimpleXML -> SimpleXML
> formatPlay (Element "PLAY" (title : personae : acts)) = 
>     Element "html" [ Element "body" $
>                      (formatTitle title) ++
>                      (formatPersonae personae) ++ 
>                      (concatMap formatAct acts) ] where
>       formatTitle :: Processor
>       formatTitle (Element "TITLE" [PCDATA title]) = 
>           [ Element "h1" [ PCDATA title] ]
>       formatTitle _ = error "invalid title"
>       
>       formatPersonae :: Processor
>       formatPersonae (Element "PERSONAE" persona) = 
>          Element "h2" [PCDATA "Dramatis Personae"] : 
>             (concatMap formatPersona persona) 
>       formatPersonae _ = error "invalid personae"
>       
>       formatPersona :: Processor 
>       formatPersona (Element "PERSONA" [ p ] ) = [ p , br ]
>       formatPersona _ = error "invalid persona"
>       
>       formatAct :: Processor
>       formatAct (Element "ACT" (Element "TITLE" [actTitle] : scenes)) = 
>          Element "h2" [actTitle] : concatMap formatScene scenes
>       formatAct _ = error "invalid act"
>       
>       formatScene :: SimpleXML -> [SimpleXML]
>       formatScene (Element "SCENE" 
>                          (Element "TITLE" [sceneTitle] : speeches)) =
>          Element "h3" [sceneTitle] : concatMap formatSpeech speeches
>       formatScene _ = error "invalid scene"

> {-
>       format t1 t1' p (Element t2 (Element "TITLE" [actTitle] : scenes)) 
>              | t1 == t2 =
>          (Element t1' [actTitle] : concatMap p scenes)
>       format _ _ _ _ = error ""
> -}

>       formatSpeech :: SimpleXML -> [SimpleXML]
>       formatSpeech (Element "SPEECH" (Element "SPEAKER" [speaker] : ls )) =
>          Element "b" [speaker] : br : (concatMap formatLine ls) 
>       formatSpeech _ = error "invalid speech"
>       
>       formatLine :: SimpleXML -> [SimpleXML]
>       formatLine (Element "LINE" [ l ]) = [ l, br ]
>       formatLine _ = error "invalid line"
> formatPlay _ = error "invalid play" where
> 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> 
> formatPlay' = head . (forTag "PLAY" $
>   (tag "html") . 
>      (tag "body") . 
>        (formatTitle `cons`  (formatPersonae `cons`  concatMap formatAct)))
>  where
>    formatTitle = forTag "TITLE" (tag "h1")                   
>
>    formatPersonae = forTag "PERSONAE" 
>     (\ personae ->
>       (Element "h2" [PCDATA "Dramatis Personae"]) : 
>       (concatMap (forTag "PERSONA" (++ [br])) personae))
>
>    formatAct    = forTag "ACT" 
>                    (forTag "TITLE" (tag "h2") `cons`
>                       (concatMap formatScene))
>
>    formatScene  = forTag "SCENE" 
>                    (forTag "TITLE" (tag "h3") `cons` 
>                       (concatMap formatSpeech))
>
>    formatSpeech = forTag "SPEECH" 
>                      (forTag "SPEAKER" (tag "b") `cons` 
>                         ((br :) . (concatMap formatLine)))
> 
>    formatLine   = forTag "LINE" (++ [br])

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


> type Processor      = SimpleXML -> [SimpleXML]
> type ListProcessor  = [SimpleXML] -> [SimpleXML] 

> tag :: ElementName -> ListProcessor
> tag n t = [Element n t]

> forTag :: ElementName -> ListProcessor -> Processor 
> forTag i1 f (Element i2 l) | i1 == i2 = f l
> forTag i1  _ _ = error $ "invalid " ++ i1

> cons :: Processor -> ListProcessor -> ListProcessor
> cons p1 p2 ( l1 : l2 ) = p1 l1 ++ p2 l2
> cons p1 p2 []          = error "list not of expected form"

> matchNameTitle name title newname f = 
>    forTag name (forTag title (tag newname) `cons` f) 



The input format is demonstrated by the sample file Play.hs. To avoid
getting into details of parsing actual XML concrete syntax, we’ll work
with just this one value for purposes of this assignment.

The XML value in Play.hs has the following structure (in standard XML syntax): 

     <PLAY>
            <TITLE>TITLE OF THE PLAY</TITLE>
            <PERSONAE>
              <PERSONA> PERSON1 </PERSONA>
              <PERSONA> PERSON2 </PERSONA>
              ... -- MORE PERSONAE
           </PERSONAE>
            <ACT>
               <TITLE>TITLE OF FIRST ACT</TITLE>
               <SCENE>
                   <TITLE>TITLE OF FIRST SCENE</TITLE>
                   <SPEECH>
                       <SPEAKER> PERSON1 </SPEAKER>
                       <LINE>LINE1</LINE>
                       <LINE>LINE2</LINE>
                       ... -- MORE LINES
                   </SPEECH>
                   ... -- MORE SPEECHES
               </SCENE>
               ... -- MORE SCENES
            </ACT>
            ... -- MORE ACTS
         </PLAY>
     

The output format is demonstrated by the file sample.html. This file
contains a (very basic) HTML rendition of the same information as
Play.hs. You may want to have a look at it in your favorite browser.
The HTML in sample.html has the following structure (with whitespace
added for readability). Note that the '<br/>' tags below should be 
represented as br elements with no children.

      <html>
        <body>
          <h1>TITLE OF THE PLAY</h1>
          <h2>Dramatis Personae</h2>
          PERSON1<br/>
          PERSON2<br/>
          ...
          <h2>TITLE OF THE FIRST ACT</h2>
          <h3>TITLE OF THE FIRST SCENE</h3>
          <b>PERSON1</b><br/>
          LINE1<br/>
          LINE2<br/>
          ...
          <b>PERSON2</b><br/>
          LINE1<br/>
          LINE2<br/>
          ...
          
          <h3>TITLE OF THE SECOND SCENE</h3>
          <b>PERSON3</b><br/>
          LINE1<br/>
          LINE2<br/>
          ...
        </body>
      </html>

Note that your version of 'formatPlay' should add no whitespace except
what's in the textual data in the original XML.

The test below uses your function to generate a file "dream.html" from
the sample play. The contents of this file after your program runs
must be character for character identical to "sample.html". 

> firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
> firstDiff [] [] = Nothing
> firstDiff (c:cs) (d:ds) 
>     | c==d = firstDiff cs ds 
>     | otherwise = Just (c:cs, d:ds)
> firstDiff cs ds = Just (cs,ds)

> -- | Test the two files, character by character to determine whether they
> -- match.
> testResults :: String -> String -> IO ()
> testResults file1 file2 = do 
>   f1 <- readFile file1
>   f2 <- readFile file2
>   case firstDiff f1 f2 of
>     Nothing -> return ()
>     Just (cs,ds) -> assertFailure msg where
>       msg  = "Results differ: '" ++ (take 20 cs) ++ 
>             "' vs '" ++ (take 20 ds)
>

> test2 :: Test
> test2 = TestCase $ do 
>   writeFile "dream.html" (xml2string (formatPlay' play))
>   testResults "dream.html" "sample.html"

Important: The purpose of this assignment is not just to “get the job
done”—i.e., to produce the right HTML. A more important goal is to
think about what is a good way to do this job, and jobs like it.

To this end, your solution should be organized into two parts:

1. a collection of generic functions for transforming XML structures
that have nothing to do with plays, plus

2. a short piece of code (a single definition or a collection of short
definitions) that uses the generic functions to do the particular job
of transforming a play into HTML.

Obviously, there are many ways to do the first part. The main
challenge of the assignment is to find a clean design that matches the
needs of the second part.  You will be graded not only on correctness
(producing the required output), but also on the elegance of your
solution and the clarity and readability of your code and
documentation. Style counts.

It is strongly recommended that you rewrite this part of the
assignment a couple of times: get something working, then step back
and see if there is anything you can abstract out or generalize,
rewrite it, then leave it alone for a few hours or overnight and
rewrite it again. Try to use some of the higher-order programming
techniques we’ve been discussing in class.





Problem 3 - foldr practice for lists
------------------------------------

Go back to the following problems in HW #1 (problem 3) and redefine
them in terms of `map` or `foldr`.

> -- 3 (a)

> -- The intersperse function takes an element and a list 
> -- and `intersperses' that element between the elements of the list. 
> -- For example,
> --    intersperse ',' "abcde" == "a,b,c,d,e"

> intersperse0 :: a -> [a] -> [a]
> intersperse0 c es = tail (foldr (\x xs -> c:x:xs) [] es)

> t1 = intersperse0 ',' "abc" 
> t2 = intersperse0 ',' "" 
> t3 = intersperse0 "" ["abc", "cde"]

> intersperse1 ::  a->[a] -> [a]
> intersperse1 _ [] = [] 
> intersperse1 c (z:zs)= z : (concat (map (\x -> [c,x]) zs))

> intersperse1a ::  a->[a] -> [a]
> intersperse1a _ [] = [] 
> intersperse1a c (z:zs)= z : (foldr (\x xs -> c:x:xs) [] zs)

> t11 = intersperse1a ',' "abc" 
> t21 = intersperse1a ',' "" 
> t31 = intersperse1a "" ["abc", "cde"]




> intersperse2 :: a -> [a] -> [a]
> intersperse2 c l = init $ foldr (\x xs -> [x] ++ [c] ++ xs) [] l

> 


> intersperse :: a -> [a] -> [a]
> intersperse c = foldr (\ x ys -> x : (if (null ys) then [] else (c : ys))) []
> --   g x [] = [x]
> --   g x ys = x : c : ys

> intersperse5 c = foldr g [] where
>   g x ys | null ys    = [x]
>          | otherwise  = x : c : ys

"length xs == 0" -> "null xs"


> intersperse4 c = foldl g []  where
>    g [] x = [x] 
>    g ys x = ys ++ [c] ++ [x]

-- foldr f (1:2:3:[])   ===   f 1 (f 2 (f 3 b))
-- foldl f (1:2:3:[])   ===   f (f (f b 1) 2) 3

> t3a :: Test
> t3a = "3a" ~: TestList [ "3a0" ~: intersperse ',' "abcde" ~=? "a,b,c,d,e", 
>                          "3a1" ~: intersperse 0 []        ~=? [] ]
>  

> -- 3 (b)

> -- invert lst returns a list with each pair reversed. 
> -- for example:
> --   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

> 
> invert :: [(a,b)] -> [(b,a)]
> invert = map swap where
>   swap (x,y) = (y,x)

> t3b :: Test
> t3b = "3b" ~:  (invert [("a",1),("a",2)]) ~?= [(1,"a"),(2,"a")]
> 

> -- 3 (c)

> -- takeWhile, applied to a predicate p and a list xs, 
> -- returns the longest prefix (possibly empty) of xs of elements 
> -- that satisfy p:
> -- For example, 
> --     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
> --     takeWhile (< 9) [1,2,3] == [1,2,3]
> --     takeWhile (< 0) [1,2,3] == []

>  
> takeWhile :: (a -> Bool) -> [a] -> [a]
> takeWhile f xs = foldr (\y ys -> if f y then y : ys else []) [] xs

> t3c :: Test
> t3c = "3c"  ~: TestList [  takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2],
>                            takeWhile (< 9) [1,2,3] ~?= [1,2,3],
>                            takeWhile (< 0) [1,2,3] ~?= [] ]

> 

> -- 3 (d)

> -- find pred lst returns the first element of the list that 
> -- satisfies the predicate. Because no element may do so, the 
> -- answer is returned in a "Maybe".
> -- for example: 
> --     find odd [0,2,3,4] returns Just 3

> 
> find :: (a -> Bool) -> [a] -> Maybe a
> find f xs = foldr (\y r -> if f y then Just y else r) Nothing xs

> t3d :: Test
> t3d = "3d" ~: find odd [0,2,3,4] ~?= Just 3 
> 


> -- 3 (e)

> -- all pred lst returns False if any element of lst 
> -- fails to satisfy pred and True otherwise.
> -- for example:
> --    all odd [1,2,3] returns False

> 
> all  :: (a -> Bool) -> [a] -> Bool
> all f xs = foldr (&&) True (map f xs)

> t3e :: Test
> t3e = "3e" ~:  (all odd [0,2,3,4]) ~?= False

> 

> test3 :: Test
> test3 = TestList [t3a, t3b, t3c, t3d, t3e]

Problem 4 - Longest Common Subsequence
-----------------------------------

Define the function 

> {-
> lcs0 :: String -> String -> String 
> lcs0 x y = maxString seqs where
>   sxs  = subsequences x
>   sys  = subsequences y
>   
>   seqs = intersect sxs sys
> 
> maxString :: [String] -> String
> maxString (x : xs) = if length x > length longest then x else longest
>                        where longest = maxString xs
> maxString []       = []
>  -}

> lcs :: String -> String -> String 
> 
> lcs l r = fst (aux l r) where
>    aux [] _ = ([], 0)
>    aux _ [] = ([], 0)
>    aux (x : xs) (y : ys) = 
>      if x == y 
>      then let ans   = aux xs ys in (x : fst ans, 1 + snd ans) 
>      else if snd left > snd right then left else right where
>            left  = aux (x:xs) ys 
>            right = aux xs (y:ys)

> 



which computes the longest common subsequence of its two arguments. The
longest common subsequence is the longest a sequence of characters that
occurs, in the same order, in both of the arguments.

For example, 

> test4 :: Test
> test4 = "4" ~: TestList [ lcs "Advanced" "Advantaged" ~?= "Advaned", 
>     lcs_fast "Advanced" "Advantaged" ~?= "Advaned", 
>     lcs_fast "abcd" "abd" ~?= "abd",
>     lcs_fast "add" "ad" ~?= "ad",
>     lcs_fast "advanced" "advantage" ~?= "advane", 
>     test4med ]

The longest common subsequence may not be unique. For example, your 
implmentation will satisfy only one of the following tests. Either 
is correct.

> test4a :: Test
> test4a = "4a" ~: lcs "abcd" "acbd" ~?= "acd"

> test4b :: Test
> test4b = "4b" ~: lcs "abcd" "acbd" ~?= "abd"

There are several ways to approach this problem, and some of them are
more efficient than others. Even if your tests pass the problems
above, think about other ways to solve this problem. In particular,
think about memoization, and optimizing the cases where the beginning
and the ends of the two strings match.  

If you are rusty on your algorithms, the [wikipedia
page](http://en.wikipedia.org/wiki/Longest_common_subsequence_problem)
for longest common subsequence may be useful. The simple, exponential
solution will be sufficient for full credit. However, optimizing this
solution is a fun challenge problem.

We can speed things up a bit by chopping off any initial and final 
prefixes that already match.

> -- | Given two strings, determine their initial segment in 
> -- common. i.e. 
> --   commonPrefix (xs ++ ys) (xs ++ zs) = (xs, ys, zs)
> commonPrefix :: Eq a => [a] -> [a] -> ([a],[a],[a])
> commonPrefix (l:ls) (r:rs) | l == r = (l : hds, ml, mr) where
>   (hds, ml, mr) = commonPrefix ls rs
> commonPrefix (l:ls) (r:rs) = ([], l : ls, r : rs)
> commonPrefix [] [] = ([], [], [])
> commonPrefix [] rs = ([], [], rs)
> commonPrefix ls [] = ([], ls, [])
> 

> lcs_med :: String -> String -> String
> lcs_med xs ys = pre ++ reverse (lcs xs'' ys'') ++ reverse suff where
>     (pre, xs',ys')     = commonPrefix xs ys
>     (suff, xs'', ys'') = commonPrefix (reverse xs') (reverse ys')

> test4med :: Test
> test4med = TestList [ lcs_med "abcd" "abcd" ~?= "abcd", 
>                       lcs_med "DFohsfdhoifdlkdsfjkldsfjk"
>                               "DFohsfdhoifdlAkdsfjkldsfjk" ~?= 
>                               "DFohsfdhoifdlkdsfjkldsfjk" ]


An even better solution is based on memoization, working from the ends
of the strings to the beginning. Suppose we are comparing the string
"abc" with "acd". Consider the following table. At position i,j, the
table contains the lcs for the tail of the first string from position
i and tail of the second from position j.

      a    c     d 
   -------------------
a  | "ac"  "c"    ""
b  | "c"   "c"    ""
c  | "c"   "c"    ""

To calculate any square in this table, one needs only to look to the
neighbors to the right and on the line below. If the characters are
different, the cell should contain the longer of the strings directly
to the right and directly below. (If there is no such string, use the
empty string).  If the characters match, then the cell should add that
character to the string in the cell diagonally to the right and below
the current one. (Again, using the empty string if there is no such
cell.)

Note that we never need to store the whole table. To compute a line of
this table cell-by-cell (working right to left) we only need the line
immediately below the current one and the partial line to the right of
the current cell.

Now lets implement it. We'll represent a line as a list of cells. 

> type Line = [Cell]

In each cell we'll store a string paired with its length. (So that we
don't have to recalculate it.)

> type Cell = (String, Int)

Next, we define a function that calculates a cell in the table. This
function is given the two characters to compare, the line of cells
immediately below the current cells and the line of cells immediately
to the right.

> computeCell :: Char -> Char -> Line -> Line -> Cell
> computeCell x y down right | x == y    = addPrev down
>                            | otherwise = longest down right where
>  
>       addPrev :: Line -> Cell
>       addPrev []            = ([x],1)
>       addPrev [_]           = ([x], 1)
>       addPrev (_:(xs,xl):_) = ((x:xs), xl+1)
>
>       longest :: Line -> Line -> Cell
>       longest []     []     = ("",0)
>       longest []     (r:_)  = r
>       longest (d:_)  []     = d
>       longest ((d,dl):_) ((r,rl):_)   =
>          if dl > rl then (d,dl) else (r,rl)

We use computeCell to compute each line in the table.  Given the
string of ys, the current x character, and the line below, iterate
over the ys and the line below to produce the next line, working from
right to left.

> computeLine :: String -> Char -> Line -> Line 
> computeLine []     _ _       = []
> computeLine (y:ys) x (d:ds)  = computeCell x y (d:ds) right : right  where
>   right = computeLine ys x ds
> computeLine (_:_)  _ []      = error "BUG: impossible case"

Finally, we can put fold computeLine over the first string to compute 
each line in the table in turn. We also remove the common prefix and suffix 
of the string using the function from before.

> lcs_fast :: String -> String -> String
> lcs_fast xs ys = case foldr (computeLine ys'') firstLine xs'' of 
>      [] -> pre ++ reverse suff 
>      ((x,_):_) -> pre ++ reverse x ++ reverse suff
>   where 
>     (pre, xs',ys')     = commonPrefix xs ys
>     (suff, xs'', ys'') = commonPrefix (reverse xs') (reverse ys')
>     firstLine          = map (const ("",0)) ys''


> 

