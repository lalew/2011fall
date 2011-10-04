CIS 552 Lecture 1
=================

A literate Haskell file is one where the file name ends in .lhs, and
all lines prefixed by '>' are Haskell source code. All other lines are
ignored by the Haskell compiler. The
[html](http://www.cis.upenn.edu/~cis552/11fa/lectures/lec1.html) is generated
directly from the
[lhs](http://www.cis.upenn.edu/~cis552/11fa/lectures/lec1.lhs) version of the
lecture notes. Feel free to download the source code and play with it
yourself after class.

Every Haskell file begins with a few lines naming the module (must
start with a capital letter and be the same as the file name) and
(optionally) importing definitions from other modules.

> module Lec1 where
> import Test.HUnit   -- operators & functions for unit testing

Programming in Haskell is about substitutituting equals by equals
------------------------------------------------------------------

    3 * (4 + 5)

    { addition }

    3 * 9

    { multiplication }

    27

That's it!








What is Abstraction?
--------------------

* Pattern Recognition

     31 * (42 + 56)

     70 * (12 + 95)

     90 * (68 + 12)


* Generalize to a function by defining an equation
 
> pat x y z = x * ( y + z )

     pat 31 42 56

     { function call } 

     31 * (42 + 56) 

     { addition }

     31 * 98

     { multiplication }

     3038





The GHC System
--------------

* Batch compiler "ghc"
  Compile and run large programs

     ghc --make Main.lhs

* Iteractive shell "ghci"

     :load foo.hs
     expression
     :type expression (:t)
     :info variable   (:i)

* Package management "cabal"
   Download and install libraries 

     cabal install hunit






Elements of Haskell
-------------------

* Everything is an *expression*
* Expressions evaluate to *values* 
* Every expression has a *type*







Basic types 
-----------

> bigInt = 31 * (42 + 56)  :: Int       -- word-sized integers

> ii = 31 * (42 + 56) :: Integer   -- arbitrarily large integers

> d = 3 * (4.2 + 5.6) :: Double    -- double precision floating point

> c = 'a' :: Char 

> b = True :: Bool 

(Note, numbers, `+` and `*` are overloaded) 








Function types
--------------

      A -> B

Function taking an input of `A`, yielding output of `B`

> pos :: Integer -> Bool
> pos x = (x > 0)








Multi-argument function types
-----------------------------

       A1 -> A2 -> A3 -> B

Function taking an input of type `A1`, `A2`, `A3`, giving out `B`

> arith :: Int -> Int -> Int -> Int
> arith x y z = x * (y + z)




Note that parens around a symbolic (infix) name turns it into a regular one.

> plus :: Int -> Int -> Int 
> plus x y = x + y 



Likewise can use alphabetic name in backquotes as infix.

> p1 :: Int
> p1 = 1 `plus` 2


> pulak :: Int -> Int -> Int -> Int
> pulak x y z = x + y + z





Tuples
------

        (A1, ..., An)

Bounded sequence of values of type `A1`, .. `An` 

> t1 = ('a', 5)      :: (Char, Int)
> t2 = ('a', 5.2, 7) :: (Char, Double, Int)
> t3 :: (Alex, Bool)
> t3 = ((7, 5.2), True) 

> type Alex = (Int, Double)
> 


Extracting values from tuples 
-----------------------------

Pattern Matching extracts values from tuples

> pat' :: (Int, Int, Int) -> Int
> pat' (x, y, z) = x * (y + z)


Optional values
---------------

        Maybe A

Perhaps a value of type `A`, or nothing. 

> m1 :: Maybe Int
> m1 = Just 3 

> m2 :: Maybe Int
> m2 = Nothing




Extracting values from 'Maybe's 
--------------------------------

Pattern Matching extracts values from maybes, need a pattern for each
case.

> pat'' :: Maybe Int -> Int
> pat'' (Just x) = (x + 3)
> pat'' Nothing  = 0


Patterns can be nested, too.

> join :: Maybe (Maybe a) -> Maybe a
> join (Just (Just x)) = Just x
> join (Just Nothing ) = Nothing
> join Nothing         = Nothing





Good for partial functions
--------------------------------------

> location :: String -> Maybe String
> location "cis501" = Just "Wu & Chen"
> location "cis502" = Just "Heilmeier"
> location "cis520" = Just "Wu & Chen"
> location "cis552" = Just "Towne 321"
> location _        = Nothing            -- wildcard pattern, matches anything


