
Higher-Order Programming Patterns
=================================

Based on notes by Ranjit Jhala, Winter 2011

> {-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
> module Main where

> import Prelude hiding (map, foldr, filter, pred, sum, product)
> import Data.Char
> import Test.HUnit

Polymorphism
============

Last time, we used `doTwice` to repeat several different operations.
Note that the actual body of the function is oblivious to how `f`
behaves:

> doTwice :: (a -> a) -> a -> a
> doTwice f x = f $ f x

We say that `doTwice` is *polymorphic* in that it works with different
types of values, eg functions that increment integers and concatenate
strings.  This is vital for *abstraction*.  The general notion of
repeating, ie *doing twice* is entirely independent from the types of
the operation that is being repeated, and so we shouldn't have to
write separate repeaters for integers and strings.  Polymorphism
allows us to *reuse* the same abstraction `doTwice` in different
settings.

Of course, with great power, comes great responsibility.

The section `(10 <)` takes an integer and returns `True` 
iff the integer is greater than `10`

> greaterThan10 :: Int -> Bool
> greaterThan10 = (10 <)

However, because the input and output types are different, it doesn't
make sense to try:

   doTwice greaterThan10

A quick glance at the type of doTwice would tell us this:

   doTwice :: (a -> a) -> a -> a

The `a` above is a *type variable*.  The signature above states that
the first argument to `doTwice` must be a function that maps values of
type `a` to `a`, i.e., must produce an output that has the same type
as its input (so that that output can be fed into the function
again!).  The second argument must also be an `a` at which point we
may are guaranteed that the result from `doTwice` will also be an `a`.
The above holds for *any* `a` which allows us to safely re-use
`doTwice` in different settings.

Of course, if the input and output type of the input function are
different, as in `greaterThan10`, then the function is incompatible
with `doTwice`.

Ok, to make sure you're following, can you figure out what this does?

> ex1 :: (a -> a) -> a -> a
> ex1 = doTwice doTwice


Polymorphic Data Structures
---------------------------

Polymorphic functions which can *operate* on different kinds of values
are often associated with polymorphic data structures which can
*contain* different kinds of values.  These are also represented by
types containing type variables.

For example, the list length function

> len :: [a] -> Int
> len []     = 0
> len (_:xs) = 1 + len xs

doesn't peek inside the specific entries in the list; it only counts
how many there are.  This property is crisply specified in the
function's signature, which states that we can invoke `len` on any
kind of list.  The type variable `a` is a placeholder that is replaced
with the actual type of the list at different application sites.  Thus,
in the below instances, `a` is replaced with `Double`, `Char` and
`[Int]` respectively.

     len [1.1, 2.2, 3.3, 4.4] :: Int
        
     len "mmm donuts!"  :: Int

     len [[], [1], [1,2], [1,2,3]] :: Int

Most standard list manipulating functions, for example those in the
standard library [Data.List][1] have generic types.  You'll find that
the type signature contains a surprising amount of information about
how the function behaves.

    (++) :: [a] -> [a] -> [a]
    
    head :: [a] -> a
    
    tail :: [a] -> [a]

Bottling Computation Patterns With Polymorphic Higher-Order Functions
=====================================================================

The tag-team of polymorphism and higher-order functions is the secret
sauce that makes FP so tasty.  It allows us to take arbitrary,
*patterns of computation* that reappear in different guises in
different places, and crisply specify them as safely reusable
strategies.  That sounds very woolly (I hope), lets look at some
concrete examples.


Computation Pattern: Iteration
------------------------------

Lets write a function that converts a string to uppercase.  Recall
that in Haskell, a `String` is just a list of `Char`s.  We must start
with a function that will convert an individual `Char` to its
uppercase version. Once we find this function, we will simply *walk
over the list*, and apply the function to each `Char`.

How might we find such a transformer?  Lets query [Hoogle][2] for a
function of the appropriate type!  Ah, we see that the module
`Data.Char` contains a function.

     toUpper :: Char -> Char

Now, we can write the simple recursive function:

> toUpperString :: String -> String
> toUpperString []     = []
> toUpperString (c:cs) = toUpper c : toUpperString cs

As you might imagine, this sort of recursion appears all over the
place.  For example, suppose I represent a location on the plane using
a pair of `Double`s (for the x- and y- coordinates) and I have a list
of points that represent a polygon.

> type XY      = (Double, Double)
> type Polygon = [XY]

Now, its easy to write a function that *shifts* a point by a specific
amount.

> shiftXY :: XY -> XY -> XY
> shiftXY (dx, dy) (x, y) = (x + dx, y + dy)

How would we translate a polygon?  Just walk over all the points in
the polygon and translate them individually.

> shiftPoly :: XY -> Polygon -> Polygon
> shiftPoly _ []       = []
> shiftPoly d (xy:xys) = shiftXY d xy : shiftPoly d xys

Now, in a lesser language, you might be quite happy with the above
code. But what separates a good programmer from a great one is the
ability to *abstract*.

The functions `toUpperString` and `shiftPoly` share the same
computational structure - they walk over a list and apply a function
to each element.  We can abstract this common pattern out as 
a higher-order function, `map`.  The two differ only in what
function they apply to each list element, so we'll just take
that as an input!

> map :: (a -> b) -> [a] -> [b]
> map _ []     = []
> map f (x:xs) = (f x) : (map f xs)

The type of `map` tells us exactly what it does:

   map :: (a -> b) -> [a] -> [b]

That is, it takes an `a -> b` transformer and list of `a` values, and
transforms each value to return a list of `b` values.  We can now
safely reuse the pattern, by *instantiating* the transformer with
different specific operations.

> toUpperString' :: String -> String
> toUpperString' = map toUpper

> shiftPoly' :: XY -> Polygon -> Polygon
> shiftPoly' d = map (shiftXY d)

Much better.

By the way, what happened to the list parameters of `toUpperString`
and `shiftPoly`?  Two words: *partial application*.  In general, in
Haskell, a function definition equation

   f x = e x

is identical to 

  f = e

as long as `x` doesn't appear in `e`.  Thus, to save ourselves the
trouble of typing, and the blight of seeing the vestigial `x`, we
often prefer to just leave it out altogether.

As an exercise, to prove to yourself using just equational reasoning 
(using the different equality laws we have seen) that the above versions 
of `toUpperString` and `shiftPoly` are equivalent.

We've already seen a few other examples of the map pattern.  Recall the
`listIncr` function, which added 1 to each element of a list:

> listIncr :: [Int] -> [Int]
> listIncr []     = []
> listIncr (x:xs) = (x+1) : listIncr xs

We can write this more cleanly with map, of course:

> listIncr' :: [Int] -> [Int]
> listIncr' = map (+1)


Computation Pattern: Folding 
----------------------------

Once you've put on the FP goggles, you start seeing computation
patterns everywhere.

Lets write a function that *adds* all the elements of a list.

> sum :: [Int] -> Int
> sum []     = 0
> sum (x:xs) = x + (sum xs)

Next, a function that *multiplies* the elements of a list.

> product :: [Int] -> Int
> product []     = 1
> product (x:xs) = x * (product xs)

Can you see the pattern?  Again, the only bits that are different are
the `base` case value, and the function being used to combine the list
element with the recursive result at each step.  We'll just turn those
into parameters, and lo!

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr _ base []     = base
> foldr f base (x:xs) = f x (foldr f base xs) 

Now, each of the individual functions are just specific instances of the 
general `foldr` pattern.

> sum', product' :: [Int] -> Int
> sum'     = foldr (+) 0
> product' = foldr (*) 1

To develop some intuition about `foldr` lets "run" it a few times by hand.

~~~~~
foldr f base [x1,x2,...,xn] 

  == f x1 (foldr f base [x2,...,xn])           {- unfold -} 

  == f x1 (f x2 (foldr f base [...,xn]))       {- unfold -} 

  == f x1 (f x2 (... (f xn base)))             {- unfold -} 

~~~~~

Aha! It has a rather pleasing structure that mirrors that of lists;
the `:` is replaced by the `f` and the `[]` is replaced by `base`.
Thus, can you see how to use it to eliminate recursion from the
recursion from our list length function:

    len :: [a] -> Int
    len []     = 0
    len (x:xs) = 1 + len xs

> len' :: [a] -> Int
> len' = foldr (\_ acc -> 1 + acc) 0


How would you use it to eliminate the recursion from:

> factorial :: Int -> Int
> factorial 0 = 1
> factorial n = n * factorial (n-1)

Like this:

> factorial' :: Int -> Int
> factorial' n = foldr (*) 1 [1..n]

OK, one more.  The standard list library function `filter` has this
type:

> filter :: (a -> Bool) -> [a] -> [a]

The idea is that it the output list should contain only the elements
of the first list for which the input function returns `True`.

So:

> filterTests :: Test
> filterTests = TestList 
>      [ filter (>10) [1..20] ~?= [11..20],
>        filter (\l -> sum l <= 42) [ [10,20], [50,50], [1..5] ]
>          ~?= [[10,20],[1..5]] ]

Can we implement filter using foldr?  Sure!

> filter pred = foldr (\a rest -> if pred a then a : rest else rest) []


Which is more readable? HOFs or Recursion
-----------------------------------------

As a beginner, you might think that the explicitly recursive versions
of some of these functions are easier to follow than the `map` and
`foldr` versions.  However, as you write more Haskell, you will find
the latter are far easier to follow, because `map` and `foldr`
encapsulate such common patterns that you'll become completely
accustomed to thinking in terms of them and other similar
abstractions.

In contrast, explicitly writing out the recursive pattern matching is
lower-level.  Every time you see a recursive function, you have to
understand how the knots are tied, and worse, there is potential for
making silly off-by-one type errors if you re-jigger the basic
strategy every time.

As an added bonus, it can be quite useful and profitable to
*parallelize* and *distribute* the computation patterns (like `map`
and `foldr`) in just one place, thereby allowing arbitrary hundreds or
thousands of instances to [benefit in a single shot!][3].

We'll see some other similar patterns later on.


Spotting Patterns In The "Real" World
=====================================

It was all well and good to see the patterns in tiny "toy" functions.
Needless to say, these patterns appear regularly in "real" code, if
only you know to look for them.  Next we will develop a library to
encode and decode text files using a secret code of our own devising.
We'll do it in two stages:

1) We will develop a [Haskell program](SecretCode.lhs) to encode files
   from disk.

2) We will [modify the program](SecretCode2.lhs) so that it can both
   encode and decode files.



[1]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html "Data.List"
[2]: http://haskell.org/hoogle "Hoogle Query: Char -> Char"
[3]: http://en.wikipedia.org/wiki/MapReduce "MapReduce"
