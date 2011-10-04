Introduction to Monads
======================

Announcements
-------------
* HW 2 available on the course website. Due in one week.
* Let me know if you are having trouble with GHC on lab computers
* Use Piazza. Are you registered? Code is weirich552











> {-# LANGUAGE NoImplicitPrelude, KindSignatures #-}
> import Prelude hiding (Functor,fmap)


Quiz from last time 
-------------------

Recall the definition of the functor type class. 

> class Functor (f :: * -> *) where
>   fmap :: (a -> b) -> f a -> f b

Your job is to create an instance of this class for the type
constructor `Maybe`.

~~~~~~~{.haskell}
data Maybe a = Just a | Nothing
~~~~~~~

> instance Functor Maybe where
>   -- fmap :: (a -> b) -> Maybe a -> Maybe b
>   fmap f Nothing  = Nothing
>   fmap f (Just x) = Just (f x)




Note: Some type classes (like Show) must be instantiated with normal
types. Others, such as Functor, must be instantiated with
*parameterized types*. To keep this straight, types have "types",
known as kinds.







Programming With Effects
========================

Shall we be pure or impure?
---------------------------

The functional programming community divides into two camps:

- "Pure" languages, such as Haskell, are based directly
  upon the mathematical notion of a function as a
  mapping from arguments to results.

- "Impure" languages, such as ML, are based upon the 
  extension of this notion with a range of possible
  effects, such as exceptions and assignments.

Pure languages are easier to reason about and may benefit
from lazy evaluation, while impure languages may be more
efficient and can lead to shorter programs.

One of the primary developments in the programming language
community in recent years (starting in the early 1990s) has
been an approach to integrating the pure and impure camps,
based upon the notion of a "monad".  This lecture introduces
the use of monads for programming with effects in Haskell.


Abstracting programming patterns
================================

Monads are an example of the idea of abstracting out a common
programming pattern as a definition.  Before considering monads,
let us review this idea, by means of two simple functions:

> inc	     :: Maybe Int -> Maybe Int
> inc Nothing     =  Nothing
> inc (Just n)    =  Just (n+1)

> sqr	     :: Maybe Int -> Maybe Int
> sqr Nothing     =  Nothing
> sqr (Just n)    =  Just (n^2)

How could we define these two functions more compactly?


Both functions are defined using the same programming pattern, namely
mapping Nothing to itself, and Just a value to some function applied
to the value.  Abstracting this pattern gives `fmap`
using which our two examples can now be defined more compactly:

> inc2 x = fmap (+1) x
> sqr2 x = fmap (^2) x







A Simple Evaluator
------------------

Consider the following simple language of expressions that are built
up from integer values using a division operator:

> data Expr = Val Int | Div Expr Expr

Such expressions can be evaluated as follows:

> eval1 ::  Expr -> Int
> eval1 (Val n)   =  n
> eval1 (Div x y) =  eval1 x `div` eval1 y

However, this function doesn't take account of the possibility of 
division by zero, and will produce an error in this case.  In order
to deal with this explicitly, we can use the `Maybe` type
to define a "safe" version of division

> safediv     :: Int -> Int -> Maybe Int
> safediv n m =  if m == 0 then Nothing else Just (n `div` m)

and then modify our evaluator:

> eval2 :: Expr -> Maybe Int
> eval2 (Val n)   = Just n
> eval2 (Div x y) = case eval2 x of  
>     Just m -> case eval2 y of
>                 Just n  -> m `safediv` n
>                 Nothing -> Nothing
>     Nothing -> Nothing









As in the previous section, we can observe a common pattern, namely
performing a case analysis on a value of a `Maybe` type, mapping
`Nothing` to itself, and `Just x` to some result depending upon `x`.
(*Aside*: we could go further and also take account of the fact that
the case analysis is performed on the result of an eval, but this
would lead to the more advanced notion of a monadic fold.)

How should this pattern be abstracted out?  One approach would be
to observe that a key notion in the evaluation of division is the
sequencing of two values of a `Maybe` type, namely the results of
evaluating the two arguments of the division.  Based upon this
observation, we could define a sequencing function

> seqn                    :: Maybe a -> Maybe b -> Maybe (a,b)
> seqn Nothing   _        =  Nothing
> seqn _         Nothing  =  Nothing
> seqn (Just x)  (Just y) =  Just (x,y)

using which our evaluator can now be defined a little more compactly:

> eval3 :: Expr -> Maybe Int
> eval3 (Val x) = Just x
> eval3 (Div x y) = apply f (seqn (eval3 x) (eval3 y)) where
>                      f :: (Int, Int) -> Maybe Int
>                      f (m,n) = m `safediv` n



The auxiliary function apply is an analogue of application for `Maybe`,
and is used to process the results of the two evaluations:

> apply            :: (a -> Maybe b) -> Maybe a -> Maybe b
> apply f Nothing  =  Nothing
> apply f (Just x) =  f x






In practice, however, using `seqn` can lead to programs that manipulate
nested tuples, which can be messy.  For example, the evaluation of
an operator `Op` with three arguments may be defined by:

~~~~~{.haskell}
eval (Op x y z) = apply f (eval x `seqn` (eval y `seqn` eval z))
                    where f (a,(b,c)) = ...
~~~~~






Combining Sequencing and Processing
-----------------------------------

The problem of nested tuples can be avoided by returning of our 
original observation of a common pattern: "performing a case analysis
on a value of a `Maybe` type, mapping `Nothing` to itself, and `Just x` 
to some result depending upon `x`".   Abstract this pattern directly gives
a new sequencing operator that we write as `>>=`, and read as "then":

~~~~~{.haskell}
(>>=)   :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f = case m of
            Nothing -> Nothing
            Just x  -> f x
~~~~~







Replacing the use of case analysis by pattern matching gives a
more compact definition for this operator:

~~~~~{.haskell}
(>>=)         :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>= _ = Nothing
(Just x) >>= f = f x
~~~~~

That is, if the first argument is `Nothing` then the second argument
is ignored and `Nothing` is returned as the result.  Otherwise, if
the first argument is of the form `Just x`, then the second argument
is applied to `x` to give a result of type `Maybe b`.

The `>>=` operator avoids the problem of nested tuples of results
because the result of the first argument is made directly available
for processing by the second, rather than being paired up with the
second result to be processed later on.  In this manner, `>>=` integrates
the sequencing of values of type `Maybe` with the processing of their
result values.  In the literature, `>>=` is often called *bind*, because
the second argument binds the result of the first.  Note also that
`>>=` is just apply with the order of its arguments swapped.

Using `>>=`, our evaluator can now be rewritten as:

> eval4 :: Expr -> Maybe Int
> eval4 (Val n)   = Just n
> eval4 (Div x y) = eval4 x >>= (\n ->
>                   eval4 y >>= (\m ->
>                   safediv n m))






The case for division can be read as follows: evaluate `x` and call
its result value `n`, then evaluate `y` and call its result value `m`,
and finally combine the two results by applying `safediv`.  In
fact, the scoping rules for lambda expressions mean that the
parentheses in the case for division can freely be omitted.

Generalising from this example, a typical expression built using
the `>>=` operator has the following structure:

~~~~~~~~~~~~~~~~~{.haskell}
m1 >>= \x1 ->
m2 >>= \x2 ->
...
mn >>= \xn ->
f x1 x2 ... xn
~~~~~~~~~~~~~~~~~

That is, evaluate each of the expression `m1`, `m2`,...,`mn` in turn, 
and combine their result values `x1`, `x2`,..., `xn` by applying the 
function f. The definition of `>>=` ensures that such an expression
only succeeds (returns a value built using `Just`) if each `mi` in 
the sequence succeeds.
In other words, the programmer does not have to worry about dealing
with the possible failure (returning `Nothing`) of any of the component
expressions, as this is handled automatically by the `>>=` operator. 



Haskell provides a special notation for expressions of the above
structure, allowing them to be written in a more appealing form:

~~~~~{.haskell}
do x1 <- m1
   x2 <- m2
   ...
   xn <- mn
   f x1 x2 ... xn
~~~~~

Hence, for example, our evaluator can be redefined as:

> eval (Val n)   = Just n
> eval (Div x y) = do x1 <- eval x
>                     x2 <- eval y
>                     x1 `safediv` x2


Exercise
--------

- Show that the version of `eval` defined using `>>=` is equivalent to
  our original version, by expanding the definition of `>>=`.

- Redefine `seqn x y` and `eval (Op x y z)` using the `do` notation.





Monads in Haskell
=================

The `do` notation for sequencing is not specific to the `Maybe` type,
but can be used with any type that forms a *monad*.  The general
concept comes from a branch of mathematics called category theory.
In Haskell, however, a monad is simply a parameterised type `m`,
together with two functions of the following types:

~~~~~{.haskell}
return :: a -> m a
(>>=)  :: m a -> (a -> m b) -> m b
~~~~~

(*Aside*: the two functions are also required to satisfy some simple
properties, but we will return to these later.)  For example, if
we take `m` as the parameterised type `Maybe`, `return` as the function
`Just :: a -> Maybe a`, and `>>=` as defined in the previous section,
then we obtain our first example, called the *maybe monad*.

In fact, we can capture the notion of a monad as a new class
declaration.  In Haskell, a class is a collection of types that
support certain overloaded functions.  For example, the class
`Eq` of equality types can be declared as follows:

~~~~~{.haskell}
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x /= y = not (x == y)
~~~~~

The declaration states that for a type `a` to be an instance of
the class `Eq`, it must support equality and inequality operators
of the specified types.  In fact, because a default definition
has already been included for `/=`, declaring an instance of this
class only requires a definition for `==`.  For example, the type
`Bool` can be made into an equality type as follows:

~~~~~{.haskell}
instance Eq Bool where
   False == False = True
   True  == True  = True
   _     == _     = False
~~~~~

The notion of a monad can now be captured as follows:

~~~~~{.haskell}
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
~~~~~

That is, a monad is a parameterised type `m` that supports `return`
and `>>=` functions of the specified types.  The fact that `m` must
be a parameterised type, rather than just a type, is inferred from its
use in the types for the two functions.   Using this declaration,
it is now straightforward to make `Maybe` into a monadic type:

~~~~~{.haskell}
instance Monad Maybe where
   -- return      :: a -> Maybe a
   return x       =  Just x

   -- (>>=)       :: Maybe a -> (a -> Maybe b) -> Maybe b
   Nothing  >>= _ =  Nothing
   (Just x) >>= f =  f x
~~~~~

(*Aside*: types are not permitted in instance declarations, but we
include them as comments for reference.)  It is because of this
declaration that the `do` notation can be used to sequence `Maybe`
values.  More generally, Haskell supports the use of this notation
with any monadic type.  In the next few sections we give some 
further examples of types that are monadic, and the benefits
that result from recognising and exploiting this fact.










The List Monad
==============


The `Maybe` monad provides a simple model of computations that can
fail, in the sense that a value of type `Maybe a` is either `Nothing`,
which we can think of as representing failure, or has the form
`Just x` for some `x` of type `a`, which we can think of as success.

The list monad generalises this notion, by permitting multiple
results in the case of success.  More precisely, a value of
`[a]` is either the empty list `[]`, which we can think of as
failure, or has the form of a non-empty list `[x1,x2,...,xn]`
for some `xi` of type `a`, which we can think of as success.
Making lists into a monadic type is straightforward:

~~~~~{.haskell}
instance Monad [] where
   -- return :: a -> [a]
   return x  =  [x]
                   
   -- (>>=)  :: [a] -> (a -> [b]) -> [b]
   xs >>= f  =  concat (map f xs)
~~~~~






(*Aside*: in this context, `[]` denotes the list type `[a]` without
its parameter.)  That is, return simply converts a value into a
successful result containing that value, while `>>=` provides a
means of sequencing computations that may produce multiple
results: `xs >>= f` applies the function f to each of the results
in the list xs to give a nested list of results, which is then
concatenated to give a single list of results.

As a simple example of the use of the list monad, a function
that returns all possible ways of pairing elements from two 
lists can be defined using the do notation as follows:

> pairs :: [Int] -> [Int] -> [(Int,Int)]
> pairs xs ys =  do x <- xs
>                   y <- ys
>                   return (x, y)

> testPairs = pairs [1,2,3,4] [5,6,7,8]

That is, consider each possible value `x` from the list `xs`, and 
each value `y` from the list `ys`, and return the pair `(x,y)`. 

It is interesting to note the similarity to how this function would be
defined using the list comprehension notation:

> pairs' xs ys = [ (x,y) | x <- xs, y <- ys ]


What are some other examples that can be written using list comprehension?


* Rewrite the `map` function using a list comprehension.

> map' f xs = [  f x | x <- xs  ]

* Create a list of all pairs where the first component is less than the second.

> pairs2 xs ys = [  (x,y) | x <- xs, y <- ys, x < y ]

> pairs2' xs ys = do x <- xs
>                    y <- ys                      
>                    if (x < y) then [ (x,y) ] else []


* Rewrite `filter`, using a guarded list comprehension.

> filter' :: (a -> Bool) -> [a] -> [a]
> filter' f xs = [ x | x <- xs, f x ]


* Write quicksort

> quicksort [] = []
> quicksort (x:xs) = quicksort xs1 ++ [x] ++ quicksort xs2 where
>            xs1 = [ y | y <- xs , y < x ]
>            xs2 = [ y | y <- xs , y >= x ]



In fact, there is a formal connection between the `do` notation
and the comprehension notation.  Both are simply different 
shorthands for repeated use of the `>>=` operator for lists.
Indeed, the language *Gofer* that was one of the precursors
to Haskell permitted the comprehension notation to be used 
with any monad.  For simplicity however, Haskell only allows
the comprehension notation to be used with lists.








The State Monad
===============

Now let us consider the problem of writing functions that manipulate
some kind of state. We're going to start with some examples of state
manipulation, written in an awkward style, and then show how monads
can cleanly abstract the sequencing necessary for such programs.

By way of an example of state manipulation, let us first define a type
of binary trees whose leaves contains values of some type a:

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>   deriving (Eq, Show)

Here is a simple example:

> tree :: Tree Char
> tree =  Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

A functional programmer would count the number of leaves in a tree with this code:

> countF :: Tree a -> Int
> countF (Leaf _) = 1
> countF (Node t1 t2) = count t1 + count t2

Now, consider how a C programmer would count the
number of leaves in a tree. He might create a local reference cell,
and then then walk the tree, incrementing the reference cell at each
leaf.  In pure code, we cannot modify the values of any
variables. However, we can define a *state transformer*, a function that 
takes an initial state as an imput and returns the new state.

In this example, the state is the Int and a state transformer is a function
of type `Int -> Int`.


> count :: Tree a -> Int
> count t = aux t 0 where
>   aux :: Tree a -> Int -> Int
>   aux (Leaf _) n = n + 1
>   aux (Node t1 t2) n = n2 where
>     n1 = aux t1 n
>     n2 = aux t2 n1


In general, a state transformer takes a current state as its argument,
and produces a modified state as its result, in which the modified
state reflects any side effects performed by the function:

> type State = Int

> type ST0 = State -> State


Now consider the problem of defining a function that labels each leaf
in such a tree with a unique or "fresh" integer.  This can be achieved
by taking the next fresh integer as an additional argument to a helper
function, and returning the next fresh integer as an additional
result.

> label1 :: Tree a -> Tree (a, Int)
> label1 = undefined
  

Credit
------

This lecture is a formatted and lightly revised version of the
lecture notes by [Graham Hutton][0], January 2011


[0]: http://www.cs.nott.ac.uk/~gmh/monads

