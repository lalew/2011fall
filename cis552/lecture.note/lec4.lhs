
Lecture 4: Homework 1 review, User-defined datatypes
====================================================

> {-# OPTIONS -Wall -fno-warn-type-defaults #-}
> module Main where

> import Prelude hiding (Maybe,Just,Nothing,Either,Left,Right)
> import Test.HUnit

HW 1 review
======================

(In class we went over many of the problems with a focus on good
Haskell style.  Please see the sample solutions.)

User-defined datatypes
======================

So far, we've mostly talked about how to use the types that appear in
the Haskell standard library.  We also discussed a few type synonyms,
like

     type XY = (Double,Double)

from the last lecture, but we haven't described any ways to define
really _new_ types.

As a motivating example, suppose you are writing an application that
deals with calendars and you need to represent the days of the week.
You might be tempted to use `String`s or `Int`s, but both of these
choices have issues.  If you use

  type Day = String

there will be lots of `Day`s that don't actually represent real days.
Also, you will need to devise and adhere to some sort of
standardization - is Monday represented by "Monday", "monday",
"MONDAY", or "Mon"?  Should you handle more than one?

The choice

  type Day = Int

has similar problems.  There are lots of Ints that won't represent
valid days.  Also you'll have to remember whether you pick Sunday or
Monday to be the first day of the week, and whether it is represented
by 0 or 1.

Haskell has a better solution: user-defined datatypes:

> data Day = Monday
>          | Tuesday
>          | Wednesday
>          | Thursday
>          | Friday
>          | Saturday
>          | Sunday

The new values (`Monday`, `Tuesday`, etc.) are called "constructors".
This is a very simple example of a datatype (basically just an
enumeration), but we'll see more examples in a minute.

We can define functions on datatypes by pattern matching!  For example:

> nextWeekday :: Day -> Day
> nextWeekday Monday    = Tuesday
> nextWeekday Tuesday   = Wednesday
> nextWeekday Wednesday = Thursday
> nextWeekday Thursday  = Friday
> nextWeekday Friday    = Monday
> nextWeekday Saturday  = Monday
> nextWeekday Sunday    = Monday

This is great.  Now we don't have to worry about the difference
between "Monday" and "monday" or which Int corresponds to which day.
If we make a typo (for example, write Frday instead of Friday), the
compiler will warn us _at compile time_.  And if we forget to handle
one of the days in some function, the compiler will warn us about that
too (inexhaustive pattern match warning).

Let's write one more function on `Day`s - we can now easily compute
when a package will arrive by "two day" shipping:

> twoBusinessDays :: Day -> Day
> twoBusinessDays = nextWeekday . nextWeekday

Datatypes can hold data, too.  For example, here is a datatype for
representing shapes[1]:

> data Shape =
>    Circle    Float Float Float
>  | Rectangle Float Float Float Float

Here, `Circle` and `Rectangle` are the constructors - every `Shape`
must be one or the other.  Each constructors has some arguments:

- A `Circle` is specified by three `Floats`.   These represent the x
  and y coordinates of the center and the radius.

- A `Rectangle` is specifed by four `Floats`.  The first two are the
  coordinates of the lower left corner, and the second two are the
  coordinates of the upper right corner.

We can pattern match on shapes.  For example, here is a function that
computes the area of any `Shape`:

> area :: Shape -> Float
> area (Circle _ _ r)          = pi * (r ^ 2)
> area (Rectangle x1 y1 x2 y2) = abs $ (x2 - x1) * (y2 - y1)

Note that constructors are first-class Haskell values just all the
other values we have seen.  Like any value, they have types.  For
example the types of `Monday` and `Tuesday` shouldn't surprise you:

        Monday  :: Day
        Tuesday :: Day

The constructors that take arguments have _function_ types.  For
example, you must apply `Circle` to three `Float`s to get a `Shape`:

         Circle    :: Float -> Float -> Float -> Shape

         Rectangle :: Float -> Float -> Float -> Float -> Shape


Recursive Types
===============

Datatypes can be defined recursively.  That is, their constructors can
take other elements of the same type as arguments.

For example, here is one way to define the type of natural numbers:

> data Nat = Zero
>          | Succ Nat

Every natural number is either Zero, or the successor of some other
natural number.  For example, we represent 2 as:

> natTwo :: Nat
> natTwo = Succ (Succ Zero)

We could write a function convert `Nat`s to `Int`s.  Of course, we'll
do it with pattern matching and recursion:

> natToInt :: Nat -> Int
> natToInt Zero     = 0
> natToInt (Succ n) = 1 + (natToInt n)

We could also define a function to add two natural numbers:

> natPlus :: Nat -> Nat -> Nat
> natPlus Zero     m = m
> natPlus (Succ n) m = Succ (natPlus n m)

Do they work?

> natToIntTests :: Test
> natToIntTests = TestList [ natToInt Zero ~?= 0,
>                            natToInt (Succ (Succ (Succ Zero))) ~?= 3 ]

> natPlusTests :: Test
> natPlusTests = 
>   TestList [ natToInt (natPlus (Succ (Succ Zero)) Zero) ~?= 2,
>              natToInt (natPlus (Succ Zero) (Succ (Succ Zero))) ~?= 3]



As another example, we could define a type representing lists of
integers:

> data IntList = INil
>              | ICons Int IntList

So that the list 1,2,3 is represented as:

> oneTwoThree :: IntList
> oneTwoThree = ICons 1 (ICons 2 (ICons 3 INil))

For comparison with Haskell's built-in lists, it might help to think
of this as:

> oneTwoThree' :: IntList
> oneTwoThree' = 1 `ICons` (2 `ICons` (3 `ICons` INil))

We can define functions by recursion on these too, of course:

> sumOfIntList :: IntList -> Int
> sumOfIntList INil        = 0
> sumOfIntList (ICons n l) = n + sumOfIntList l



Polymorphic Datatypes
=====================

It would sure be annoying to have a seperate kind of list for each
type of data!  Luckily, we know Haskell's list type is polymorphic -
you can have a list of type `[a]` for any `a`.

Users can define polymorphic datatypes too.  For example, here is the
definition of the `Maybe` type that we've used in past lectures:

> data Maybe a = Nothing | Just a

Notice that the type `Maybe` itself takes an argument now - the type
variable `a`.  We're allowed to use that type variable in the
constructors.  So `Just` is a constructor that can be applied to
values of any type and will create a `Maybe` of the same type:

    Just :: a -> Maybe a

Thus, `Just` and `Nothing` work at any type:

> justThree :: Maybe Int
> justThree = Just 3

> noInt :: Maybe Int
> noInt = Nothing

> justTrue :: Maybe Bool
> justTrue = Just True

A number of other polymorphic datatypes appear in the standard
library.  For example, here's a datatype that's useful when you
want to carry around values that could have either of two types:

> data Either a b = Left a | Right b

`Either` is often useful for error handling.  Sometimes returning a
`Maybe a` isn't quite good enough because you'd like to give a helpful
error message.  `Either String a` works a little better - you can use
`Left msg` in the case of an error, and `Right v` in case things
are... all right.

For example, here's a safer division function:

> safeDiv :: Int -> Int -> Either String Int
> safeDiv _ 0 = Left "You can't divide by zero, silly."
> safeDiv x y = Right $ x `div` y

Of course, Either is more useful when things can go wrong in more
than one way.



Trees
=====

OK, now let's play a bit with a bigger example: trees.  Here's
one way to define binary trees that have data at the internal nodes
in Haskell:

> data Tree a = Leaf | Branch a (Tree a) (Tree a)

For example, we can represent the following tree:


        5
      /   \
     2     9
    / \     \
   1   4     7


Like this:

> exTree :: Tree Int
> exTree = Branch 5 (Branch 2 (Branch 1 Leaf Leaf) (Branch 4 Leaf Leaf))
>                   (Branch 9 Leaf (Branch 7 Leaf Leaf))


We can write simple functions by recursion:

> treePlus :: Tree Int -> Int -> Tree Int
> treePlus Leaf             _ = Leaf
> treePlus (Branch n t1 t2) m =
>     Branch (n + m) (treePlus t1 m) (treePlus t2 m)


> infixOrder :: Tree a -> [a]
> infixOrder Leaf             = []
> infixOrder (Branch x t1 t2) = (infixOrder t1) ++ [x] ++ (infixOrder t2)

Of course, what we should really do is reimplement our higher-order
patterns for trees!

> tMap :: (a -> b) -> Tree a -> Tree b
> tMap _ Leaf             = Leaf
> tMap f (Branch a t1 t2) = Branch (f a) (tMap f t1) (tMap f t2)

The type of fold will have to change a little bit.  Now the function
that tells us how to combine the current element with the recursive
result will have to take two results - one for each branch!

> tFold :: (a -> b -> b -> b) -> b -> Tree a -> b
> tFold _ base Leaf             = base
> tFold f base (Branch a t1 t2) = 
>     f a (tFold f base t1) (tFold f base t2)

Now, let's reimplement infixOrder:

> infixOrder' :: Tree a -> [a]
> infixOrder' = tFold (\a l1 l2 -> l1 ++ [a] ++ l2) []

And we can get the other orderings just as quickly!

> prefixOrder :: Tree a -> [a]
> prefixOrder = tFold (\a l1 l2 -> [a] ++ l1 ++ l2) []

> postfixOrder :: Tree a -> [a]
> postfixOrder = tFold (\a l1 l2 -> l1 ++ l2 ++ [a]) []

[1] This example taken from "Learn You a Haskell for Great Good".
    http://learnyouahaskell.com/
