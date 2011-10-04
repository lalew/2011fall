Programming With Effects II
===========================

We're continuing to study monads by looking at examples of *specific*
monads to try to understand how they work. At this point, don't panic
if you don't understand the big picture, each of the specific
instances are useful in their own right. For right now, think of
monads as
[burritos](http://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/),
especially since they really
[are](http://blog.plover.com/prog/burritos.html).


> {-# LANGUAGE NoImplicitPrelude #-}
> import Prelude hiding (getLine,sequence,(>>))
> import Data.Map (Map)
> import qualified Data.Map as Map
> import State

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

A functional programmer would count the number of leaves in a tree
with this code:

> countF :: Tree a -> Int
> countF (Leaf _) = 1
> countF (Node t1 t2) = countF t1 + countF t2

Now, consider how a C programmer would count the number of leaves in a
tree. He might create a local reference cell, and then then walk the
tree, incrementing the reference cell at each leaf.  In pure code, we
cannot modify the values of any variables. However, we can define a
*state transformer*, a function that takes an initial state as an
input and returns the new state.

In this example, the state, also called the "store", is an Int (the
current count) and a state transformer is a function of type `Int ->
Int`.

> type Store = Int

> type ST0 = Store -> Store

> countI :: Tree a -> Int
> countI t = aux t 0 where
>   aux :: Tree a -> Int -> Int
>   aux (Leaf _) n = n + 1         -- found a leaf, increment state
>   aux (Node t1 t2) n = n2 where  -- thread the state through the 
>     n1 = aux t1 n                -- computation on the subtrees
>     n2 = aux t2 n1


In general, a state transformer takes a current state as its argument,
and produces a modified state as its result, in which the modified
state reflects any side effects performed by the function.

Now consider the problem of defining a function that labels each leaf
in such a tree with a unique or "fresh" integer.  This can be achieved
by taking the next fresh integer as an additional argument to a helper
function, and returning the next fresh integer as an additional
result.

> label1 :: Tree a -> Tree (a, Int)
> label1 t = fst (aux t 0) where
>    aux :: Tree a -> Int -> ( Tree(a,Int), Int )
>    aux (Leaf x) n     = ( Leaf (x,n), n+1 )
>    aux (Node t1 t2) n = ( Node t1' t2', n2 ) where
>       (t1', n1) = aux t1 n
>       (t2', n2) = aux t2 n1










This example demonstrates that in general, we may wish to return a
result value in addition to updating the store. For this reason, we
generalize our type of state transformers to also return a result
value, with the type of such values being a parameter of the `ST1`
type:

> type ST1 a = Store -> (a, Store)







The state transformer may also wish to take argument values.  However,
there is no need to further generalize the `ST1` type to take account
of this, because this behavior can already be achieved by currying.
For example, the state transformer for the tree above takes a tree and
returns a labeled tree and has type `Tree a -> ST1 (Tree (a,Int))`, which
abbreviates the curried function type

~~~~~{.haskell}
Tree a -> Store -> (Tree (a, Int), Store)
~~~~~



The reason we are talking about state transformers is that
parameterized type `ST1` is a monad.  What are its definitions of
return and bind?

~~~~~{.haskell}
type ST1 a = Store -> (a, Store)

instance Monad ST1 where
   -- return :: a -> Store -> (a, Store)
   return x  =  \ st -> (x,st)

   -- (>>=)  :: (Store -> (a, Store)) -> (a -> (Store -> (b, Store))) -> (Store -> (b, Store))
   st >>= f  =  \ s -> let (x, s') = st s in f x s'
               
~~~~~










That is, `return` converts a value into a state transformer that
simply returns that value without modifying the state.

In turn, `>>=` provides a means of sequencing state transformers:
`st >>= f` applies the state transformer `st` to an initial state
`s`, then applies the function `f` to the resulting value `x` to
give a second state transformer `(f x)`, which is then applied
to the modified state `s'` to give the final result.

Note that `return` could also be defined by `return x s = (x,s)`.  
However, we prefer the above definition in which the second 
argument `s` is shunted to the body of the definition using a
lambda abstraction, because it makes explicit that `return` is
a function that takes a single argument and returns a state
transformer, as expressed by the type `a -> ST a`:  A similar
comment applies to the above definition for `>>=`.

We conclude this section with a technicality.  In Haskell,
types defined using the `type` mechanism cannot be made into
instances of classes.  Hence, in order to make ST into an
instance of the class of monadic types, in reality it needs
to be redefined using the "data" mechanism, which requires
introducing a dummy constructor (called `S` for brevity):

> data ST2 a = S (Store -> (a, Store))

It is convenient to define our own application function for
this type, which simply removes the dummy constructor:

> apply        :: ST2 a -> Store -> (a, Store)
> apply (S f) x = f x









In turn, ST2 is now defined as a monadic type as follows:

> instance Monad ST2 where
>   -- return :: a -> ST2 a
>   return x   = S (\s -> (x,s))
>
>   -- (>>=)  :: ST2 a -> (a -> ST2 b) -> ST2 b
>   st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

(*Aside*: the runtime overhead of manipulating the dummy constructor
S can be eliminated by defining ST using the `newtype` mechanism
of Haskell, rather than the `data` mechanism.)


Now, let's rewrite the tree labeling function with the State 
monad.


In order to generate a fresh integer, we define a special
state transformer that simply returns the current state as
its result, and the next integer as the new state:

> fresh :: ST2 Int
> fresh =  S (\n -> (n, n+1))


Using this, together with the `return` and `>>=` primitives that
are provided by virtue of `ST` being a monadic type, it is now
straightforward to define a function that takes a tree as its
argument, and returns a state transformer that produces the
same tree with each leaf labelled by a fresh integer:

> mlabel            :: Tree a -> ST2 (Tree (a,Int))
> mlabel (Leaf x)   =  do 
>    n <- fresh
>    return (Leaf (x,n))
> mlabel (Node l r) =  do 
>    l' <- mlabel l
>    r' <- mlabel r
>    return (Node l' r')



Note that the programmer does not have to worry about the tedious
and error-prone task of dealing with the plumbing of fresh labels,
as this is handled automatically by the state monad.

Finally, we can now define a function that labels a tree by
simply applying the resulting state transformer with zero as
the initial state, and then discarding the final state:

> label  :: Tree a -> (Tree (a, Int), Int)
> label t =  (apply (mlabel t) 0)

For example, `label tree` gives the following result:

~~~~~{.haskell}
ghci> label tree
Node (Node (Leaf ('a', 0)) (Leaf ('b',1))) (Leaf ('c', 2))
~~~~~

Exercise
--------

- Define a function `app :: (Store -> Store) -> ST2 Store`, such 
  that fresh can be redefined by `fresh = app (+1)`.


- Define a function `run :: ST2 a -> Store -> a`, such that label
  can be redefined by `label t = run (mlabel t) 0`.



A Generic State Transformer
===========================

Often, the *store* that we want to have will have multiple 
components, eg multiple variables whose values we might want 
to *update*. This is easily accomplished by using a different
type for `Store` above, for example, if we want two integers, 
we might use the definition

~~~~~{.haskell}
type Store = (Int, Int)
~~~~~

and so on. 

However, it would be good to write reusable code, which would work 
with any store. 

The file [State](State.html)  ( [lhs version](State.lhs) ) contains
a generic library for that purpose.




Using a Generic State Transformer
=================================

Let us use our generic state monad to rewrite the tree labeling function 
from above. Note that the actual type definition of the generic transformer
is *hidden* from us, so we must use only the publicly exported functions:
`get`, `put` and `runState` (in addition to the monadic functions we get for
free.)


First, we write an action that returns the next fresh integer 
(Note, the first type argument is the store, the second is the 
result type of the monadic action.)

> freshS :: State Int Int
> -- freshS = do { n <- get ; _ <- put (n+1) ; return n }
> freshS = get >>= (\ n -> put (n+1) >>= (\ _ -> return n))

Now, the labeling function is straightforward

> mlabelS (Leaf x)   =  do n <- freshS
>                          return (Leaf (x,n))
> mlabelS (Node l r) =  do l' <- mlabelS l
>                          r' <- mlabelS r
>                          return (Node l' r')


Easy enough!

~~~~~{.haskell}
ghci> runState (mlabelS tree) 0
~~~~~






We can *execute* the action from any initial state of our choice

~~~~~{.haskell}
ghci> runState (mlabelS tree) 1000
~~~~~







Now, what's the point of a generic state transformer if we can't have richer
states? Next, let us extend our `fresh` and `label` functions so that 

- each node gets a new label (as before), and

- the state also contains a map of the *frequency* with which each 
  leaf value appears in the tree.

Thus, our state will now have two elements, an integer denoting the *next*
fresh integer, and a `Map a Int` denoting the number of times each leaf
value appears in the tree. (Documentation for the [Data.Map module](http://lambda.haskell.org/hp-tmp/docs/2011.2.0.0/ghc-doc/libraries/containers-0.4.0.0/Data-Map.html). )

> data MySt a = M { index :: Int
>                 , freq  :: Map a Int }
>               deriving (Eq, Show)

We write an *action* that returns the next fresh integer as

> freshM :: State (MySt a) Int
> freshM = do 
>   s     <- get              
>   let n  = index s           -- access the index component of record s
>   put ( s { index = n + 1 } ) -- create new record, like x but with
>                              -- updated index component
>   return n 

Similarly, we want an action that updates the frequency of a given
element `k`

> updFreqM :: Ord a => a -> State (MySt a) ()
> updFreqM k = do 
>   s    <- get               
>   let f = freq s 
>   let n = Map.findWithDefault 0 k f
>   put $ s { freq = Map.insert k (n + 1) f }

And with these two, we are done

> mlabelM :: Ord a => Tree a -> State (MySt a) (Tree (a, Int))
> mlabelM (Leaf x)   =  do n  <- freshM
>                          _  <- updFreqM x                           
>                          return (Leaf (x,n))                           
> mlabelM (Node l r) =  do l' <- mlabelM l
>                          r' <- mlabelM r
>                          return $ Node l' r'

Now, our *initial* state will be something like

> initM :: MySt a
> initM = M 0 Map.empty

and so we can label the tree

~~~~~{.haskell}
ghci> let tree2   = Node tree tree 
ghci> let (lt, s) = runState (mlabelM tree) initM 

ghci> lt
Node (Node (Node (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2))) (Node (Node (Leaf ('a',3)) (Leaf ('b',4))) (Leaf ('c',5)))

ghci> s
M {index = 6, freq = fromList [('a',2),('b',2),('c',2)]}
~~~~~





The IO Monad
============

Recall that interactive programs in Haskell are written using the
type `IO a` of "actions" that return a result of type `a`, but may
also perform some input/output.  A number of primitives are
provided for building values of this type, including:

~~~~~{.haskell}
return  :: a -> IO a
(>>=)   :: IO a -> (a -> IO b) -> IO b
getChar :: IO Char
putChar :: Char -> IO ()
~~~~~

The use of return and `>>=` means that `IO` is monadic, and hence
that the do notation can be used to write interactive programs.
For example, the action that reads a string of characters from
the keyboard can be defined as follows:


> getLine :: IO String
> getLine = undefined


It is interesting to note that the `IO` monad can be viewed as a
special case of the state monad, in which the internal state is
a suitable representation of the "state of the world":

~~~~~{.haskell}
   type World = ...

   type IO a  = World -> (a,World)
~~~~~

That is, an action can be viewed as a function that takes the
current state of the world as its argument, and produces a value
and a modified world as its result, in which the modified world
reflects any input/output performed by the action.  In reality,
Haskell systems such as Hugs and GHC implement actions in a more
efficient manner, but for the purposes of understanding the
behavior of actions, the above interpretation can be useful.



Credit
------

This lecture is a formatted and lightly revised version of the
lecture notes by [Graham Hutton][0], January 2011

[0]: http://www.cs.nott.ac.uk/~gmh/monads
