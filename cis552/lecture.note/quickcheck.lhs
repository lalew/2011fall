QuickCheck: Type-directed Property Testing
---

> {-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances #-}

> import Test.QuickCheck
> import Control.Monad
> import Data.List
> import Data.Map (Map)
> import qualified Data.Map as Map 
> import While
> import Parser hiding (choose)
> import State

To compile this file, you need the auxiliary files used in HWs 3 and 4:
     - [State.hs](../../hw/hw3/State.hs)
     - [Parser.hs](../../hw/hw4/Parser.hs)
     - [ParserCombinators.hs](../../hw/hw4/ParserCombinator.hs)
     - While.hs (hw4 soln, available after 10/28/2011)

In this lecture, we will look at [QuickCheck][1], a technique that
cleverly exploits typeclasses and monads to deliver a powerful 
automatic testing methodology. 

Quickcheck was developed by [Koen Claessen][0] and [John Hughes][11]
more than ten years ago, and has since been ported to other languages
and is currently used, among other things to find subtle [concurrency
bugs][3] in [telecommunications code][4]. In 2010, it received the
[most influential paper award](http://www.sigplan.org/award-icfp.htm)
for the ICFP 2000 conference.

The key idea on which QuickCheck is founded, is *property-based
testing*.  That is, instead of writing individual test cases (eg unit
tests corresponding to input-output pairs for particular functions)
one should write *properties* that are desired of the functions, and
then *automatically* generate *random* tests which can be run to
verify (or rather, falsify) the property.

By emphasizing the importance of specifications, QuickCheck yields 
several benefits:

1. The developer is forced to think about what the code *should do*,

2. The tool finds corner-cases where the specification is violated, 
   which leads to either the code or the specification getting fixed,

3. The specifications live on as rich, machine-checkable documentation
   about how the code should behave.


Properties
==========

A QuickCheck property is essentially a function whose output is a boolean.
The standard "hello-world" QC property is

> prop_revapp :: [Int] -> [Int] -> Bool
> prop_revapp xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys



That is, a property looks a bit like a mathematical theorem that the
programmer believes is true. A QC convention is to use the prefix `"prop_"`
for QC properties. Note that the type signature for the property is not the 
usual polymorphic signature; we have given the concrete type `Int` for the
elements of the list. This is because QC uses the types to generate random
inputs, and hence is restricted to monomorphic properties (those that don't
contain type variables.)

To *check* a property, we simply invoke the function

~~~~~{.haskell}
quickCheck :: (Testable prop) => prop -> IO ()
  	-- Defined in Test.QuickCheck.Test
~~~~~

let's try it on our example property above

~~~~~{.haskell}
*Main> quickCheck prop_revapp 
~~~~~

What's that ?! Well, let's run the *property* function on the two inputs

~~~~~{.haskell}
*Main> prop_revapp [0] [1] 
~~~~~

QC has found a sample input for which the property function *fails* ie,
returns `False`. Of course, those of you who are paying attention will
realize there was a bug in our property, namely it should be

> prop_revapp_ok :: [Int] -> [Int] -> Bool
> prop_revapp_ok xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

because `reverse` will flip the order of the two parts `xs` and `ys` of 
`xs ++ ys`. Now, when we run 

~~~~~{.haskell}
*Main> quickCheck prop_revapp_ok
~~~~~






That is, Haskell generated 100 test inputs and for all of those, the
property held. You can up the stakes a bit by changing the number of tests
you want to run

> quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

and then do

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_revapp_ok
~~~~~




QuickCheck QuickSort
--------------------

Let's look at a slightly more interesting example. Here is the canonical 
implementation of *quicksort* in Haskell.

> qsort []     = []
> qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
>   where lhs  = [y | y <- xs, y < x]
>         rhs  = [z | z <- xs, z > x]

Really doesn't need much explanation! Let's run it "by hand" on a few inputs

~~~~~{.haskell}
*Main> [10,9..1]
[10,9,8,7,6,5,4,3,2,1]
*Main> qsort [10,9..1]


*Main> [2,4..20] ++ [1,3..11]
[2,4,6,8,10,12,14,16,18,20,1,3,5,7,9,11]
*Main> qsort $ [2,4..20] ++ [1,3..11]

~~~~~

Looks good -- let's try to test that the output is in 
fact sorted. We need a function that checks that a 
list is ordered

> isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
> isOrdered _          = True

and then we can use the above to write a property

> prop_qsort_isOrdered :: [Int] -> Bool
> prop_qsort_isOrdered = isOrdered . qsort

Let's test it!

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_qsort_isOrdered 
~~~~~

Conditional Properties
----------------------

Here are several other properties that we 
might want. First, repeated `qsorting` should not
change the list. That is, 

> prop_qsort_idemp ::  [Int] -> Bool 
> prop_qsort_idemp xs = qsort (qsort xs) == qsort xs


Second, the head of the result is the minimum element
of the input

> prop_qsort_min :: [Int] -> Bool
> prop_qsort_min xs = head (qsort xs) == minimum xs

However, when we run this, we run into a glitch



~~~~~{.haskell}
*Main> quickCheck prop_qsort_min 
~~~~~

But of course! The earlier properties held *for all inputs*
while this property makes no sense if the input list is empty! 
This is why thinking about specifications and properties has the 
benefit of clarifying the *preconditions* under which a given 
piece of code is supposed to work. 

In this case we want a *conditional properties* where we only want 
the output to satisfy to satisfy the spec *if* the input meets the
precondition that it is non-empty.

> prop_qsort_nn_min    :: [Int] -> Property
> prop_qsort_nn_min xs = 
>   not (null xs) ==> head (qsort xs) == minimum xs
>
> prop_qsort_nn_max    :: [Int] -> Property
> prop_qsort_nn_max xs = 
>   not (null xs) ==> head (reverse (qsort xs)) == maximum xs

We can write a similar property for the maximum element too. This time
around, both the properties hold

~~~~~{.haskell}
*Main> quickCheckN 100 prop_qsort_nn_min

*Main> quickCheckN 100 prop_qsort_nn_max
~~~~~

Note that now, instead of just being a `Bool` the output
of the function is a `Property` a special type built into 
the QC library. Similarly the *implies* combinator `==>` 
is one of many QC combinators that allow the construction 
of rich properties.


Testing Against a Model Implementation
--------------------------------------

We could keep writing different properties that capture 
various aspects of the desired functionality of `qsort`. 
Another approach for validation is to test that our `qsort` 
is *behaviorally* identical to a trusted *reference 
implementation* which itself may be too inefficient or 
otherwise unsuitable for deployment. In this case, let's 
use the standard library's `sort` function

> prop_qsort_sort    :: [Int] -> Bool
> prop_qsort_sort xs =  qsort xs == sort xs

which we can put to the test

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_qsort_sort
~~~~~






Say, what?!



~~~~~{.haskell}
*Main> qsort [-1,-1]
~~~~~

Ugh! So close, and yet ... Can you spot the bug in our code?

~~~~~{.haskell}
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs  = [y | y <- xs, y < x]
        rhs  = [z | z <- xs, z > x]
~~~~~

We're assuming that the *only* occurrence of (the value) `x` 
is itself! That is, if there are any *copies* of `x` in the 
tail, they will not appear in either `lhs` or `rhs` and hence
they get thrown out of the output. 


Is this a bug in the code? What *is* a bug anyway? Perhaps the
fact that all duplicates are eliminated is a *feature*! At any 
rate there is an inconsistency between our mental model of how 
the code *should* behave as articulated in `prop_qsort_sort` 
and the actual behavior of the code itself.

We can rectify matters by stipulating that the `qsort` produces
lists of distinct elements

> isDistinct (x:xs) = not (x `elem` xs) && isDistinct xs
> isDistinct _      = True
>
> prop_qsort_distinct :: [Int] -> Bool 
> prop_qsort_distinct = isDistinct . qsort  

and then, weakening the equivalence to only hold on inputs that 
are duplicate-free 

> prop_qsort_distinct_sort :: [Int] -> Property
> prop_qsort_distinct_sort xs = 
>   (isDistinct xs) ==> qsort xs == sort xs

QuickCheck happily checks the modified properties

~~~~~{.haskell}
*Main> quickCheck prop_qsort_distinct

*Main> quickCheck prop_qsort_distinct_sort 

~~~~~


The Perils of Conditional Testing
---------------------------------

Well, we managed to *fix* the `qsort` property, but beware! Adding
preconditions leads one down a slippery slope. In fact, if we paid
closer attention to the above runs, we would notice something

~~~~~{.haskell}
*Main> quickCheckN 10000 prop_qsort_distinct_sort 
...
(5012 tests; 248 discarded)
...
+++ OK, passed 10000 tests.
~~~~~

The bit about some tests being *discarded* is ominous. In effect, 
when the property is constructed with the `==>` combinator, QC 
discards the randomly generated tests on which the precondition 
is false. In the above case QC grinds away on the remainder until 
it can meet its target of `10000` valid tests. This is because 
the probability of a randomly generated list meeting the precondition 
(having distinct elements) is high enough. This may not always be the case.

The following code is (a simplified version of) the `insert` function 
from the standard library 

~~~~~{.haskell}
insert x []                 = [x]
insert x (y:ys) | x > y     = x : y : ys
                | otherwise = y : insert x ys
~~~~~

Given an element `x` and a list `xs`, the function walks along `xs` 
till it finds the first element greater than `x` and it places `x` 
to the left of that element. Thus

~~~~~{.haskell}
*Main> insert 8 ([1..3] ++ [10..13])
~~~~~

Indeed, the following is the well known [insertion-sort][5] algorithm

> isort :: Ord a => [a] -> [a]
> isort = foldr insert []

We could write our own tests, but why do something a machine can do better?!

> prop_isort_sort    :: [Int] -> Bool
> prop_isort_sort xs = isort xs == sort xs

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_isort_sort 
~~~~~

Now, the reason that the above works is that the `insert` 
routine *preserves* sorted-ness. That is while of course 
the property 

> prop_insert_ordered'      :: Int -> [Int] -> Bool
> prop_insert_ordered' x xs = isOrdered (insert x xs)

is bogus

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_insert_ordered' 

*Main> insert 0 [0, -1]

~~~~~

the output *is* ordered if the input was ordered to begin with

> prop_insert_ordered      :: Int -> [Int] -> Property 
> prop_insert_ordered x xs = 
>   isOrdered xs ==> isOrdered (insert x xs)

Notice that now, the precondition is more *complex* -- the property 
requires that the input list be ordered. If we QC the property

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_insert_ordered

~~~~~

Ugh! The ordered lists are so *sparsely* distributed 
among random lists, that QC timed out well before it 
found 1000 valid inputs!

*Aside* the above example also illustrates the benefit of 
writing the property as `p ==> q` instead of using the boolean
operator `||` to write `not p || q`. In the latter case, there is 
a flat predicate, and QC doesn't know what the precondition is,
so a property may hold *vacuously*. For example consider the 
variant

> prop_insert_ordered_vacuous :: Int -> [Int] -> Bool
> prop_insert_ordered_vacuous x xs = 
>   not (isOrdered xs) || isOrdered (insert x xs)

QC will happily check it for us

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_insert_ordered_vacuous
~~~~~

Unfortunately, in the above, the tests passed *vacuously* 
only because their inputs were *not* ordered, and one 
should use `==>` to avoid the false sense of security 
delivered by vacuity.

QC provides us with some combinators for guarding against 
vacuity by allowing us to investigate the *distribution* 
of test cases

~~~~~{.haskell}
collect  :: Show a => a -> Property -> Property
classify :: Bool -> String -> Property -> Property
~~~~~

We may use these to write a property that looks like

> prop_insert_ordered_vacuous' :: Int -> [Int] -> Property 
> prop_insert_ordered_vacuous' x xs = 
>   collect (length xs) $
>   classify (isOrdered xs) "ord" $
>   classify (not (isOrdered xs)) "not-ord" $
>   not (isOrdered xs) || isOrdered (insert x xs)

When we run this, as before we get a detailed breakdown
of the 100 passing tests

~~~~~{.haskell}
*Main> quickCheck prop_insert_ordered_vacuous'
~~~~~

where a line `P% N, COND` means that `p` percent of the inputs had length
`N` and satisfied the predicate denoted by the string `COND`. Thus, as we
see from the above, a paltry 13% of the tests were ordered and that was
because they were either empty (`2% 0, ord`) or had one (`9% 1, ord`).
or two elements (`2% 2, ord`). The odds of randomly stumbling upon a 
beefy list that is ordered are rather small indeed!


Generating Data
===============

Before we start discussing how QC generates data (and how we can help it
generate data meeting some pre-conditions), we must ask ourselves a basic
question: how does QC behave *randomly* in the first place?!

~~~~~{.haskell}
*Main> quickCheck prop_insert_ordered'

*Main> quickCheck prop_insert_ordered'

~~~~~

Eh? This seems most *impure* -- same inputs yielding two totally different
outputs! Well, this should give you a clue as to one of the key techniques
underlying QC -- **monads!** 

The Generator Monad
-------------------

A Haskell term that generates a (random value) of type `a` has the type
[`Gen a`][6] which is defined as

~~~~~{.haskell}
newtype Gen a = MkGen{ unGen :: StdGen -> Int -> a }
~~~~~

In effect, the term is a function that takes as input a random number
generator `StdGen` and a seed `Int` and returns an `a` value. One can
easily (and we shall see, profitably!) turn `Gen` into a `Monad` by

~~~~~{.haskell}
instance Monad Gen where
  return x =
    MkGen (\_ _ -> x)
  
  MkGen m >>= k =
    MkGen (\r n ->
      let (r1,r2)  = split r
          MkGen m' = k (m r1 n)
       in m' r2 n
    )
~~~~~

The function `split` simply *forks* the random number generator into two
parts; which are used by the left and right parameters of the bind
operator `>>=`. (*Aside* you should be able to readily spot the 
similarity between random number generators and the `State` monad -- 
in both cases the basic action is to grab some value and transition 
the *state* to the next-value. For more details see [Chapter 14, RWH][7])

The Arbitrary Typeclass
-----------------------

QC uses the above to define a typeclass for types for which
random values can be generated!

~~~~~{.haskell}
class Arbitrary a where
  arbitrary :: Gen a
~~~~~

Thus, to have QC work with (ie generate random tests for) values of type
`a` we need only make `a` an instance of `Arbitrary` by defining an
appropriate `arbitrary` function for it. QC defines instances for base
types like `Int` , `Float`, lists etc and lifts them to compound types.

~~~~~{.haskell}
instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = do x <- arbitrary
	         y <- arbitrary 
	         return (x,y)
~~~~~
or more simply

~~~~~{.haskell}
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a,b,c) where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary
~~~~~



Generator Combinators
---------------------

QC comes loaded with a set of combinators that allow us to create 
custom instances for our own types.

The first of these combinators is `choose`

~~~~~{.haskell}
choose :: (System.Random.Random a) => (a, a) -> Gen a
~~~~~

which takes an *interval* and returns an random element from that interval.
(The typeclass `System.Random.Random` describes types which can be
*sampled*. For example, the following is a randomly chosen set of numbers
between `0` and `3`.

~~~~~{.haskell}
*Main> sample $ choose (0, 3)
~~~~~

A second useful combinator is `elements` 

~~~~~{.haskell}
elements :: [a] -> Gen a
~~~~~

which returns a generator that produces values drawn from the input list

~~~~~{.haskell}
*Main> sample $ elements [10, 20..100]
~~~~~

A third combinator is `oneof` 

~~~~~{.haskell}
oneof :: [Gen a] -> Gen a
~~~~~

which allows us to randomly choose between multiple generators

~~~~~{.haskell}
*Main> sample $ oneof [elements [10,20,30], choose (0,3)]
~~~~~

and finally, the above is generalized into the `frequency` combinator 

~~~~~{.haskell}
frequency :: [(Int, Gen a)] -> Gen a
~~~~~

which allows us to build weighted combinations of individual generators.


Generating Ordered Lists
------------------------

We can use the above combinators to write generators for lists 

> genList1 ::  (Arbitrary a) => Gen [a]
> genList1 = liftM2 (:) arbitrary genList1

~~~~~{.haskell}
*Main> sample (genList1 :: Gen [Int])
~~~~~









Can you spot a problem in the above? It only generates infinite 
lists! Hmm. Let's try again,

> genList2 ::  (Arbitrary a) => Gen [a]
> genList2 = oneof [ return []
>                  , liftM2 (:) arbitrary genList2]

~~~~~{.haskell}
*Main> sample (genList2 :: Gen [Int])
~~~~~







This is not bad, but we may want to give the generator a higher 
chance of not finishing off with the empty list, so let's use

> genList3 ::  (Arbitrary a) => Gen [a]
> genList3 = frequency [ (1, return [])
>                      , (7, liftM2 (:) arbitrary genList3) ]

~~~~~{.haskell}
*Main> sample (genList3 :: Gen [Int])
~~~~~





We can use the above to build a custom generator that always returns
*ordered lists* by piping the generate list into the `sort` function

> genOrdList :: (Arbitrary a, Ord a) => Gen [a]
> genOrdList = genList3 >>= return . sort

To *check* the output of a custom generator we can use the `forAll` combinator

~~~~~{.haskell}
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
~~~~~

For example, we can check that in fact, the combinator only produces
ordered lists

~~~~~
*Main> quickCheck $ forAll genOrdList isOrdered 
~~~~~

and now, we can properly test the `insert` property

> prop_insert :: Int -> Property 
> prop_insert x = forAll genOrdList $ \xs -> isOrdered xs && isOrdered (insert x xs)

~~~~~
*Main> quickCheck prop_insert 
~~~~~

Case Study : Checking Compiler Optimizations
============================================

Next, let's look at how QC can be used to generate structured data, 
by doing a small case-study on checking a compiler optimization. 

Recall the small *While* language that you wrote an evaluator for 
in [HW3](../hw/hw3.html). 

While: Syntax
-------------

The languages had arithmetic expressions 

~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
data Expression =
    Var  Variable
  | Val  Value
  | Op   Bop Expression Expression
  deriving (Eq, Ord)
~~~~~~~~~~~~~~~~~~~~~~~~

where the atomic expressions were either variables or values.  Note
that instead of using strings directly as variables, this time we are
creating a newtype.

~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
newtype Variable = V String deriving (Eq, Ord, Show)

data Value =
     IntVal Int
   | BoolVal Bool
   deriving (Eq, Ord)
~~~~~~~~~~~~~~~~~~~~~~~~

We used the expressions to define imperative *statements* which are either
assignments, if-then-else, a sequence of two statements, or a while-loop.

~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
data Statement =
    Assign Variable Expression
  | If Expression Statement Statement
  | While Expression Statement
  | Sequence Statement Statement
  | Skip
  deriving (Eq, Ord)
~~~~~~~~~~~~~~~~~~~~~~~~

While: Semantics
----------------

The behavior of *While* programs was given using a *state* which is simply
a map from variables to values. Intuitively, a statement will *update* the
state by modifying the values of the variables appropriately.

~~~~~~~{.haskell}
type Store = Map Variable Value
~~~~~~~

Your assignment was to (use the `State` monad to) write an evaluator 
(aka interpreter) for the language that took as input a program and 
a starting state and returned the final state.

> execute ::  Store -> Statement -> Store
> execute env = flip execState env . evalS

Since you wrote the code for the HW (you **DID** didn't you?) we won't go
into the details now -- check out the HW solution if you had trouble.

Generating While Programs
-------------------------

We could painstakingly write manual test cases, but instead let's write some
simple generators for *While* programs, so that we can then check
interesting properties of the programs and the evaluator.

First, let's write a generator for variables.

> instance Arbitrary Variable where 
>   arbitrary = do x <- elements ['A' .. 'Z']
>                  return (V [x])







thus, we assume that the programs are over variables drawn from the
uppercase alphabet characters. That is, our test programs range over 26
variables (you can change the above if you like.)

Second, we can write a generator for constant values (that can appear in 
expressions). Our generator simply chooses between randomly generated 
`Bool` and `Int` values.

> instance Arbitrary Value where 
>   arbitrary = oneof [ liftM IntVal arbitrary,
>                       liftM BoolVal arbitrary]



Third, we define a generator for `Expression` and `Bop` which 
selects from the different cases.

> instance Arbitrary Bop where
>   arbitrary = elements [ Plus, Times, Minus, Gt, Ge, Lt, Le]

> -- instance Arbitrary Expression where
> --  arbitrary = arbE
>
> arbE :: Gen Expression
> arbE = frequency [ (1, liftM Var arbitrary), 
>                    (1, liftM Val arbitrary),
>                    (5, liftM3 Op arbitrary arbitrary arbitrary) ]                     




Finally, we need to write a generator for `Store` so that we can run 
the *While* program from some arbitrary input configuration. 

> instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Map a b) where
>   arbitrary = do xvs <- arbitrary 
>                  return $ Map.fromList xvs
>  

In the above, `xvs` is a randomly generated list of key-value tuples,
which is turned into a `Map` by the `fromList` function.

Program Equivalence
-------------------

Let `p1` and `p2` be two (*While*) programs. We say that `p1` 
is *equivalent to* `p2` if for all input configurations `st` the
configuration resulting from executing `p1` from `st` is the same 
as that obtained by executing `p2` from `st`. Formally,

> (===) ::  Statement -> Statement -> Property
> p1 === p2 = forAll arbitrary $ \st -> execute st p1 == execute st p2 

Checking An Optimization: Zero-Add-Elimination
----------------------------------------------

Excellent! Let's take our generators our for a spin, by checking some
*compiler optimizations*. Intuitively, a compiler optimization (or 
transformation) can be viewed as a *pair* of programs -- the input 
program `p_in` and a transformed program `p_out`. A transformation 
`(p_in, p_out)`is *correct* iff `p_in` is equivalent to `p_out`.

Here's are some simple *sanity* check properties that correspond to
optimizations. 

> prop_add_zero_elim :: Variable -> Expression -> Property
> prop_add_zero_elim x e = 
>   (x `Assign` (Op Plus e $ Val (IntVal 0))) === (x `Assign` e) 
>
> prop_sub_zero_elim :: Variable -> Expression -> Property
> prop_sub_zero_elim x e =
>   (x `Assign` (Op Minus e $ Val (IntVal 0))) === (x `Assign` e) 

Let's check the properties!

~~~~~{.haskell}
*Main> quickCheck prop_add_zero_elim 
~~~~~








Uh? whats going on? Well, let's look at the generator for expressions.

~~~~~{.haskell}
arbE = frequency [ (1, liftM Var arbitrary)
                 , (1, liftM Val arbitrary)
                 , (5, liftM3 Op arbitrary arbitrary arbitrary) ]
~~~~~

in effect, its will generate infinite expressions with high probability!
(do the math!) So we need some way to control the size, either by biasing 
the `Var` and `Val` constructors (which terminate the generation) or by
looking at the *size* of the structure during generation. We can do this
with the combinator

~~~~~{.haskell}
sized :: (Int -> Gen a) -> Gen a
~~~~~

which lets us write functions that parameterize the generator with an
integer (and then turn that into a flat generator.)

> arbnE :: Int -> Gen Expression
> arbnE n = frequency [ (1, liftM Var arbitrary), 
>                       (1, liftM Val arbitrary),
>                       (n, liftM3 Op arbitrary (arbnE (n `div` 2)) (arbnE (n `div` 2))) ]
                         
                         








In the above, we keep *halving* the number of allowed nodes, and when that
number goes to `0` we just return an atomic expression (either a variable
or a constant.) We can now update the generator for expressions to

> -- instance Arbitrary Expression where
> --   arbitrary = sized arbnE

And now, let's check the property again

~~~~~{.haskell}
*Main> quickCheck prop_add_zero_elim 
~~~~~







whoops! Forgot about those pesky boolean expressions! If you think about it,

~~~~~{.haskell}
X := True + 0
~~~~~{.haskell}

will assign `0` to the variable while

~~~~~{.haskell}
X := True 
~~~~~{.haskell}

will assign `True` to the variable! Urgh. Ok, let's limit ourselves to *Integer* 
expressions

> intE :: Gen Expression
> intE = sized arbnEI 
>   where arbnEI 0 = oneof [ liftM Var arbitrary
>                          , liftM (Val . IntVal) arbitrary ]
>         arbnEI n = oneof [ liftM Var arbitrary
>                          , liftM (Val . IntVal) arbitrary
>                          , liftM2 (Op Plus)   (arbnEI n_by_2) (arbnEI n_by_2) 
>                          , liftM2 (Op Times)  (arbnEI n_by_2) (arbnEI n_by_2) 
>                          , liftM2 (Op Minus)  (arbnEI n_by_2) (arbnEI n_by_2) 
>                          ]
>                    where n_by_2 = n `div` 2

using which, we can tweak the property to limit ourselves to integer
expressions

> prop_add_zero_elim' :: Variable -> Property
> prop_add_zero_elim' x = 
>   forAll intE $ \e -> (x `Assign` (Op Plus e $ Val (IntVal 0))) === (x `Assign` e)


O, Quickcheck, what say you now?

~~~~~{.haskell}
*Main> quickCheck prop_add_zero_elim'
~~~~~



Of course! in the input state where `N` has the value `True`, the result of
executing `V := N` is quite different from executing `V := N + 0`. Oh well,
so much for that optimization, I guess we need some type information before
we can eliminate the additions-to-zero!


Checking An Optimization: Constant Folding (sort of) 
----------------------------------------------------

Well, that first one ran aground because *While* was untyped (tsk tsk.)
and so adding a zero can cause problems if the expression is a boolean. 
Let's look at another optimization that is not plagued by the
int-v-bool conflict. Suppose you have two back-to-back 
assignments

~~~~~{.haskell}
X := E
Y := E
~~~~~

It is silly to recompute `E` twice, since the result is already stored in
`X`. So, we should be able to optimize the above code to 

~~~~~{.haskell}
X := E
Y := X
~~~~~

Let's see how we might express the correctness of this transformation 
as a QC property

> prop_const_prop :: Variable -> Variable -> Expression -> Property
> prop_const_prop x y e = 
>   ((x `Assign` e) `Sequence` (y `Assign` e))
>   ===
>   ((x `Assign` e) `Sequence` (y `Assign` Var x))

Mighty QC, do you agree ?

~~~~~{.haskell}
*Main> quickCheck prop_const_prop 
~~~~~







Shrinking 
---------

Holy transfer function!! It fails?!! And what is that bizarre test? It
seems rather difficult to follow. Turns out, QC comes with a *test
shrinking* mechanism; all we need do is add to the `Arbitrary` instance
a function of type

~~~~~{.haskell}
shrink :: a -> [a]
~~~~~

which will take a candidate and generate a list of *smaller* candidates
that QC will systematically crunch through till it finds a minimally
failing test!

> instance Arbitrary Expression where
>   arbitrary = sized arbnE
>
>   shrink (Op _ e1 e2) = [e1, e2]
>   shrink _            = []
> 

Let's try it again to see if we can figure it out!

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_const_prop 
*** Failed! Falsifiable (after 26 tests and 4 shrinks):    
D
U
A + D
fromList [(D,-638),(G,256),(H,False),(K,False),(O,True),(R,True),(S,-81),(T,926)]
~~~~~

Aha! Consider the two programs

~~~~~{.haskell}
D := A + D; 
U := A + D
~~~~~

and 

~~~~~{.haskell}
D := A + D; 
U := D
~~~~~

are they equivalent? Pretty subtle, eh. 


One last challenge, can you use QuickCheck to test homework #4? What is 
is the property that we care about for the parser and pretty printer?

> prop_RTS :: Statement -> Bool
> prop_RTS s = case doParse statementP ((show . pp) s) of
>                    [ (s', []) ] -> s == s'
>                    _  -> False

~~~~~{.haskell}
*Main> quickCheck prop_RTS
~~~~~

Well, I hope I've convinced you that QuickCheck is pretty awesome. 
The astonishing thing about it is its sheer simplicity -- a few 
fistfuls of typeclasses and a tiny pinch of monads and lo! a 
shocking useful testing technique that can find a bunch of 
subtle bugs or inconsistencies in your code. 

Moral of the story -- types can go a long way towards making your code
*obviously correct*, but not the whole distance. Make up the difference 
by writing properties, and have the machine crank out tests for you!

There is a lot of literature on QuickCheck on the web. It is used
for a variety of commercial applications, both in Haskell and in 
pretty much every modern language, including [Perl][10]. 
Even if you don't implement a system in Haskell, you can use
QuickCheck to test it, by just using the nifty [data generation][9] 
facilities. 

Credit: lecture is adapted from UCSD [12].

[0]: http://www.cse.chalmers.se/~koen/
[1]: http://www.cse.chalmers.se/~rjmh/QuickCheck/
[2]: http://www.cs.york.ac.uk/fp/smallcheck/
[3]: http://video.google.com/videoplay?docid=4655369445141008672#
[4]: http://www.erlang-factory.com/upload/presentations/55/TestingErlangProgrammesforMulticore.pdf
[5]: http://en.wikipedia.org/wiki/Insertion_sort
[6]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/src/Test-QuickCheck-Gen.html#Gen
[7]: http://book.realworldhaskell.org/read/monads.html
[8]: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
[9]: http://www.haskell.org/haskellwiki/QuickCheck_as_a_test_set_generator
[10]: http://community.moertel.com/~thor/talks/pgh-pm-talk-lectrotest.pdf
[11]: http://www.cse.chalmers.se/~rjmh
[12]: http://cseweb.ucsd.edu/classes/wi11/cse230/lectures/quickcheck.lhs


> -- Extra code for arbitrarily generating statements. 
>
> instance Arbitrary Statement where
>   arbitrary = sized arbnS where
>     arbnS 0 = oneof 
>                [ liftM2 Assign arbitrary arbitrary
>                , return Skip ]
>     arbnS n = frequency [ (1, liftM2 Assign arbitrary arbitrary)
>                         , (1, return Skip)
>                         , (2, liftM3 If arbitrary (arbnS n_by_2) (arbnS n_by_2))
>                         , (2, liftM2 While arbitrary (arbnS n_by_2))
>	                  , (2, liftM2 Sequence  (arbnS n_by_2) (arbnS n_by_2)) 
>                         ]
>       where n_by_2 = n `div` 2
>
>   shrink (Assign v e) = []
>   shrink (If e s1 s2) = [s1, s2]
>   shrink (While e s)  = [s]
>   shrink (Sequence s1 s2) = [s1 , s2]
>   shrink Skip = []