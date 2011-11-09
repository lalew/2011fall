Monad Transformers
==================

> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, OverlappingInstances, FlexibleInstances #-}
>
> import Control.Monad.Error
> import Control.Monad.State
> import Control.Monad.Writer

> import qualified State as S


Monads Can Do Many Things
=========================

Let's recall the simple language of divisions. 

> data Expr = Val Int
>           | Div Expr Expr
>           deriving (Show)


Today, we will see how monads can be used to write (and compose) 
*evaluators* for such languages.

Remember the vanilla *unsafe* evaluator 

> eval            ::  Expr -> Int
> eval (Val n)   =  n
> eval (Div x y) =  eval x `div` eval y

Here are two terms that we will use as running examples. 

> ok  = Div (Div (Val 1972) (Val 2)) (Val 23)
> err = Div (Val 2) (Div (Val 1) (Div (Val 2) (Val 3)))

The first evaluates properly and returns a valid answer, 
and the second fails with a divide-by-zero exception.

~~~~~{.haskell}
*Main> eval ok 

*Main> eval err
~~~~~

We didn't like this `eval` because it can just blow up 
with a divide by zero error without telling us how it 
happened. Worse, the error is a *radioactive* value
that, spread unchecked through the entire computation.

We used the `Maybe` type to capture the failure case: a
`Nothing` result meant that an error happened somewhere,
while a `Just n` result meant that evaluation succeeded
yielding `n`. Morever, we saw how the `Maybe` monad could
be used to avoid ugly case-split-staircase-hell.

> evalMaybe ::  Expr -> Maybe Int
> evalMaybe (Val n)   = return n
> evalMaybe (Div x y) = do n <- evalMaybe x
>                          m <- evalMaybe y
>                          if m == 0 
>                            then Nothing
>                            else return (n `div` m)

which behaves thus

~~~~~{.haskell}
*Main> evalMaybe ok 

*Main> evalMaybe err
~~~~~


Error Handling Via Exception Monads
-----------------------------------

The trouble with the above is that it doesn't let us know
*where* the divide by zero occurred. It would be nice to 
have an *exception* mechanism where, when the error occurred,
we could just saw `Throw x` for some value `x` which would, 
like an exception go rocketing back to the top and tell us
what the problem was.

If you think for a moment, you'll realize this is but a small
tweak on the `Maybe` type; all we need is to jazz up the 
`Nothing` constructor so that it carries the exception value.

> data Exc a = Raise  String
>            | Result a
>            deriving (Show)

Here the `Raise` is like `Nothing` but it carries a string 
denoting what the exception was. We can make the above a 
`Monad` much like the `Maybe` monad.

> instance Monad Exc where
> -- Exc a -> (a -> Exc b) -> Exc b
>   (Raise str) >>= f = Raise str
>   (Result x)  >>= f = f x
>   return           = Result

and now, we can use our newly minted monad to write 
a better exception throwing evaluator

> -- evalExc ::  Expr -> Exc Int
> evalExc (Val n)   = return n
> evalExc (Div x y) = do n <- evalExc x
>                        m <- evalExc y
>                        if m == 0 
>                          then throwErrorExc $ errorS y m
>                          else return $ n `div` m

where the sidekick `errorS` generates the error string. 

> errorS y m = "Error dividing by " ++ show y ++ " = " ++ show m

Note that this is essentially like the first evaluator; 
instead of bailing with `Nothing` we return some (hopefully)
helpful message, but the monad takes care of ensuring that 
the exception is shot back up.

~~~~~{.haskell}
*Main> evalExc ok 

*Main> evalExc err
~~~~~

Counting Operations Via State Monads
------------------------------------

Next, lets stop being so paranoid about errors and instead 
try to do some *profiling*. Lets imagine that the `div` 
operator is very expensive, and that we would like to 
*count* the number of divisions that are performed while
evaluating a particular expression.

As you might imagine, our old friend the state monad is likely to be
of service here! The type of store that we'd like to use is just 
the count of number of division operations, we can store that in 
an Int.

> type Store = Int

We imported the library `State.hs` above, and qualified it so that 
all of the operations begin with `S.`

~~~~~~~~~~{.haskell}
data State a    = S (Store -> (a, Store))
instance Monad State where
   return x     = S $ \s -> (x, s)
   (S st) >>= f = S $ \s -> let (x, s') = st s 
                                S st'   = f x
                            in st' s'

runState :: State s a -> s -> (a, s)
runState (S f) x = f x

get :: State s s
get = S (\s -> (s, s))

put :: s -> State s ()
put s' = S (\_ -> ((), s'))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For simplicity, we will abbreviate the State monad, using this
particular Store type, as 'ST'.

> type ST = S.State Store

Armed with the above, we can write a function

> tickStore :: ST ()
> tickStore = do n <- S.get
>                S.put (n+1)

Now, we can write a profiling evaluator

> -- evalST           :: Expr -> ST Int
> evalST (Val n)   = return n
> evalST (Div x y) = do n <- evalST x
>                       m <- evalST y
>                       tickST
>                       return (n `div` m)

and observe our profiling evaluator at work

> goST :: Expr -> IO ()
> goST e = putStrLn $ "value: " ++ show x ++ ", count: " ++ show s
>            where (x,s) = undefined :: (Int, Int) -- S.runState (evalST e) 0

~~~~~{.haskell}
*Main> goST ok
~~~~~

But, alas, to get the profiling we threw out the nifty 
error handling that we had put in earlier

~~~~~{.haskell}
*Main> goST err 
~~~~~


Transformers: Making Monads Multitask
=====================================

So it looks like Monads can do many thigs, but only 
*one thing at a time* -- you can either use a monad 
to do the error management plumbing *OR* to do the 
state manipulation plumbing, but not at the same time. 
Is it too much ask for both? I guess we could write a 
*mega-state-and-exception* monad that supports the 
operations of both, but that doesn't sound like any 
fun at all! Worse, if later we decide to add yet 
another feature, then we would have to make up yet 
another mega-monad. 

We shall take a different approach, where we will keep
*wrapping* or decorating monads with extra features, so 
that we can take a simple monad, and then add the 
Exception monad's features to it, and then add the 
State monad's features and so on. 

The key to doing this is to not define exception 
handling, state passing etc as monads, but as
**functions from monads to monads.** 
This will require a little more work up-front 
(most of which is done already in well-designed libraries)
but after that we can add new features in a modular manner.
For example, to get a mega state- and exception- monad,
we will start with a dummy `Identity` monad, apply it to 
the `StateT` monad transformer (which yields state-passing monad)
and pass the result to the `ExcT` monad transformer which yields
the desired mega monad. Incidentally, the above should remind 
some of you of the [Decorator Design Pattern][2] and others 
of [Python's Decorators][3].


Step 1: Describing Monads With Special Features
-----------------------------------------------

The first step to being able to compose monads is to 
define typeclasses that describe monads armed with 
the special features. For example, the notion of an 
*exception monad* is captured by the typeclass

> class Monad m => MonadExc m where
>   throwErrorExc :: String -> m a 

which corresponds to monads that are also equipped with 
an appropriate `throwErrorExc` function (you can add a `catch` 
function too, if you like!) Indeed, we can make `Exc` an
instance of the above by

> instance MonadExc Exc where 
>   throwErrorExc = Raise

See what happens if you change 'Raise' to 'throwErrorExc' in 
the evaluator 'evalExc' above. What is the new type of the 
evaluator?

Similarly, we can bottle the notion of a *state monad* in the
typeclass

> class Monad m => MonadST m where
>   runStateST :: m a -> Store -> m (a, Store)
>   getST      :: m Store 
>   putST      :: Store -> m ()

which corresponds to monads that are kitted out with the
appropriate execution, extraction and modification functions.
Using this monad, we can redefine the ticking operation to 
work for any state monad.

> tickST :: MonadST m => m ()
> tickST = do store <- getST 
>             putST (store+1)              

Needless to say, we can make `ST` an instance of the above by

> instance MonadST ST where
>   -- runStateST :: ST a -> Store -> ST (a, Store)
>   runStateST m s = return (S.runState m s)
>   getST      = S.get
>   putST      = S.put

Now go back and see what happens when you replace `tickStore`
with `tickST` above.

Step 2: Using Monads With Special Features
------------------------------------------

Armed with these two typeclasses, we can write our evaluator
quite easily  

> evalMega (Val n)   = return n
> evalMega (Div x y) = do n <- evalMega x
>                         m <- evalMega y
>                         tickST
>                         if m == 0 
>                           then throwErrorExc $ errorS y m 
>                           else return $ n `div` m

Note that it is simply the combination of the two evaluators
from before -- we use the `throwErrorExc` from `evalExc` and the 
`tickST` from `evalST`. Meditate for a moment on the type of 
above evaluator; note that it works with *any monad* that 
is both a exception- and a state- monad! Indeed, if, as I 
exhorted you to, you had gone back and studied the types of
`evalST` and `evalExc` you would find that each of those 
functions required the underlying monad to be a 
state-manipulating and exception-handling monad respectively.
In contrast, the above evaluator simply demands both features.

But where do we get monads that are both state-manipulating and
exception-handling?


Step 3: Injecting Special Features into Monads
----------------------------------------------

To *add* special features to existing monads, we will use
*monad transformers*, which are type operators `t` that 
map a monad `m` to a monad `t m`. The key ingredient of 
a transformer is that it must have a function `promote`
that can take an `m` value (ie action) and turn it into a 
`t m` value (ie action):

> class Transformer t where
>   promote :: Monad m => m a -> (t m) a

Now, that just defines the *type* of a transformer, let's see
some real transformers!

**A Transformer For Exceptions**

Consider the following type

> newtype ExcT m a = MkExc (m (Exc a))

it is simply a type with two parameters -- the first is a monad `m`
inside which we will put the exception monad `Exc a`. In other words,
the `ExcT m a` simply *injects* the `Exc a` monad *into* the value
slot of the `m` monad. By convention, the names of monad transformers
end with 'T'.

It is easy to formally state that the above is a 
bonafide transformer

> instance Transformer ExcT where
>   promote = MkExc . promote_ 

where the generic `promote_` function simply injects
the value from the outer monad `m`  into the inner 
monad `m1` :

> promote_ ::  (Monad m1, Monad m2) => m1 t -> m1 (m2 t)
> promote_ m = do t <- m
>                 return (return t)

Consequently, any operation on the input monad `m` can be 
directly promoted into an action on the transformed monad, 
and so the transformation *preserves* all the operations
on the original monad.

Now, the real trick is twofold, we ensure that if `m` 
is a monad, then transformed `ExcT m` is an 
*exception monad*, that is an `MonadExc`.

First, we show the transformer output is a monad:

> instance Monad m => Monad (ExcT m) where
>   -- return :: a -> ExcT m a
>   --           a -> m (Exn a)
>   return x = MkExc (return (Result x))
>   -- (>>=) :: ExcT m a -> (a -> ExcT m b) -> ExcT m b
>   -- p :: m (Exc a)
>   (MkExc p) >>= f  = MkExc $ do x <- p
>                                 case x of                           
>                                   Raise s -> return (Raise s)
>                                   Result a -> strip (f a)
>
> strip :: ExcT m a -> m (Exc a) 
> strip (MkExc e) = e

and next we ensure that the transformer is an 
exception monad by equipping it with `throwErrorExc`

> instance Monad m => MonadExc (ExcT m) where
>   -- String -> ExcT m a
>   throwErrorExc s = MkExc (return (Raise s))

**A Transformer For State**

Next, we will build a transformer for the state monad,
following, more or less, the recipe for exceptions. Here 
is the type for the transformer

> newtype STT m a =  MkSTT (Store -> m (a, Store))
>   -- NOT : m (Store -> (a, Store))
                     
Thus, in effect, the enhanced monad is a state-update where 
the output is the original monad as we do the state-update
and return as output the new state wrapped inside the 
parameter monad.

> instance Transformer STT where
>   -- f :: m a 
>          
>   promote f = MkSTT (\s -> do a <- f  
>                               return (a, s))

Next, we ensure that the transformer output is a monad:

> instance Monad m => Monad (STT m) where
>   return  = promote . return
>    -- f :: a -> STT m b
>            -- Store -> m (b, Store)
>   m >>= f = MkSTT $ \ s -> do (x, s') <- (strip m) s
>                               strip (f x) s'                                
>      where strip (MkSTT a) = a                                 

and next we ensure that the transformer is a state 
monad by equipping it with the operations from `MonadST` 

> instance Monad m => MonadST (STT m) where
> --runStateST :: STT m a -> Store -> STT m (a, Store)
> -- (Store -> m (a, Store))
>    -- Store -> m ((a, Store), Store))
>   runStateST (MkSTT f) s = MkSTT (\ s0 -> do (a, s') <- f s
>                                              return ((a,s'), s0))            

> --getST :: STT m Store
> --getST :: MkSTT (Store -> m (Store, Store))
>   getST = MkSTT $ \s -> return (s,s)
>
> --getST :: Store -> STT m () 
> --getST :: Store -> MkSTT (Store -> m ((), Store))
>   putST s = MkSTT $ \_ -> return ((),s) 

Step 4: Preserving Old Features of Monads
-----------------------------------------

Of course, we must make sure that the original features
of the monads are not lost in the transformed  monads. 
For this purpose, we will just use the `promote` 
operation to directly transfer operations from 
the old monad into the transformed monad. 

Thus, we can ensure that if a monad was already 
a state-manipulating monad, then the result of 
the exception-transformer is *also* a 
state-manipulating monad.

> instance MonadExc m => MonadExc (STT m) where
>   throwErrorExc s = promote (throwErrorExc s)

> instance MonadST m => MonadST (ExcT m) where
>   getST = promote getST
>   putST = promote . putST
>   runStateST (MkExc m) s = MkExc $ do (ex, s') <- runStateST m s
>                                       case ex of
>                                         Result x  -> return $ Result (x, s')
>                                         Raise err -> return $ Raise err 

Step 5: Whew! Put together and Run
----------------------------------

Finally, we can put all the pieces together and run the transformers.
We could *order* the transformations differently (and that can have
different consequences on the output as we will see.)

> evalExSt :: Expr -> STT Exc Int
> evalExSt = evalMega
>
> evalStEx :: Expr -> ExcT ST Int
> evalStEx = evalMega

which we can run as

> goExSt :: Expr -> IO ()
> goExSt e = putStr $ pr (evalExSt e) where
>     pr :: STT Exc Int -> String  
>     pr (MkSTT f) = case (f 0) of 
>                      Raise s         -> "Raise: " ++ s ++ "\n"
>                      Result (v, cnt) -> "Count: " ++ show cnt ++ "\n" ++
>                                         "Result: " ++ show v ++ "\n"

> goStEx :: Expr -> IO ()
> goStEx e = putStr $ pr (evalStEx e) where
>    pr :: ExcT ST Int -> String
>    pr (MkExc f) = "Count: " ++ show cnt ++ "\n" ++ show r ++ "\n" where
>        (r, cnt) = S.runState f 0
            
~~~~~{.haskell}
*Main> goStEx ok

*Main> goStEx err

*Main> goExSt ok

*Main> goExSt err
~~~~~


The Monad Transformer Library
=============================

While it is often *instructive* to roll your own versions 
of code, as we did above, in practice you should reuse as 
much as you can from standard libraries. 


Error Monads and Transformers 
-----------------------------

The above sauced-up exception-tracking version of `Maybe` 
already exists in the standard type [Either][1].

~~~~~{.haskell}
*Main> :info Either 
data Either a b = Left a | Right b 	-- Defined in Data.Either
~~~~~

The `Either` type is a generalization of our `Exc` type, 
where the exception is polymorphic, rather than just being
a `String`. In other words the hand-rolled `Exc a` corresponds 
to the standard `Either String a` type.

The standard [MonadError][6] typeclass corresponds directly with
`MonadExc` developed above.

~~~~~{.haskell}
*Main> :info MonadError
class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a
  	-- Defined in Control.Monad.Error.Class
instance (Monad m, Error e) => MonadError e (ErrorT e m)
  -- Defined in Control.Monad.Error
instance (Error e) => MonadError e (Either e)
  -- Defined in Control.Monad.Error
instance MonadError IOError IO -- Defined in Control.Monad.Error
~~~~~

Note that `Either String` is an instance of `MonadError` much 
like `Exc` is an instance of `MonadExc`. Finally, the `ErrorT`
transformer corresponds to the `ExcT` transformer developed above
and its output is guaranteed to be an instance of `MonadError`.

State Monads and Transformers
-----------------------------

Similarly, the `ST` monad that we wrote above is but a pale reflection 
of the more general [State][4] monad.  

~~~~~{.haskell}
*Main> :info State
type State s = StateT s Data.Functor.Identity.Identity
  	-- Defined in Control.Monad.Trans.State.Lazy
~~~~~

The `MonadExc` typeclass corresponds directly with the 
standard [MonadState][5] typeclass is the proper version  
of our `MonadST` rendered above. 

~~~~~{.haskell}
*Main> :info MonadState
class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
  	-- Defined in Control.Monad.State.Class

instance (Monad m) => MonadState s (StateT s m)
  -- Defined in Control.Monad.State.Lazy

instance MonadState s (State s)
  -- Defined in Control.Monad.State.Lazy
~~~~~

Note that `State s` is already an instance of `MonadState` much 
like `ST` is an instance of `MonadST`. Finally, the `StateT`
transformer corresponds to the `STT` transformer developed above
and its output is guaranteed to be an instance of `MonadState`.

Thus, if we stick with the standard libraries, we can simply write

> tick :: (MonadState Int m) => m ()
> tick = do { n <- get; put (n+1) }

> eval1 :: (MonadError String m, MonadState Int m) => 
>           Expr -> m Int            
> eval1 (Val n)   = return n
> eval1 (Div x y) = do n   <- eval1 x
>                      m   <- eval1 y
>                      if m == 0 
>                        then throwError $ errorS y m
>                        else do tick
>                                return  $ n `div` m



> evalSE :: Expr -> StateT Int (Either String) Int
> evalSE = eval1

~~~~~{.haskell}
*Main> runStateT (evalSE ok) 0

*Main> runStateT (evalSE err) 0
~~~~~

You can stack them in the other order if you prefer

> evalES :: Expr -> ErrorT String (State Int) Int
> evalES = eval1

which will yield a different result

~~~~~{.haskell}
*Main> runState (runErrorT (evalES ok)) 0

*Main> runState (runErrorT (evalES err)) 0
~~~~~

see that we actually get the division-count (upto the point of
failure) even when the computation bails.


Tracing Operations Via Logger Monads
------------------------------------

Next, we will spice up our computations to also *log* messages (a *pure* 
variant of the usual method where we just *print* the messages to the
screen.) This can be done with the standard [Writer][7] monad, which
supports a `tell` action that logs the string you want (and allows you to
later view the entire log of the computation).

To accomodate logging, we juice up our evaluator directly as

> -- eval2 :: (MonadError String m, MonadState Int m, 
> --          MonadWriter String m) => 
> --          Expr -> m Int            
> eval2 v = 
>   case v of 
>     Val n   -> do tell $ msg (Val n) n
>                   return n
>     Div x y -> do n <- eval2 x
>                   m <- eval2 y
>                   if m == 0 
>                     then throwError $ errorS y m 
>                     else do tick 
>                             tell $ msg (Div x y) (n `div` m)
>                             return  $ n `div` m

where the `msg` function is simply

> msg :: (Show a, Show b) => a -> b -> String
> msg t r = "term: " ++ show t ++ ", yields " ++ show r ++ "\n"

Note that the only addition to the previous evaluator is the `tell`
operations! We can run the above using

> evalWSE :: Expr -> WSE Int
> evalWSE = eval2

where `WSE` is a type abbreviation

> type WSE a = WriterT String (StateT Int (Either String)) a 

That is, we simply use the `WriterT` transformer to decorate the underlying 
monad that carries the state and exception information.

~~~~~{.haskell}
*Main> runStateT (runWriterT (evalWSE ok)) 0

*Main> runStateT (runWriterT (evalWSE err)) 0
~~~~~

That looks a bit ugly, so we can write our own pretty-printer

> instance Show a => Show (WSE a) where
>   show m = case runStateT (runWriterT m) 0 of 
>              Left s            -> "Error: " ++ s
>              Right ((v, w), s) -> "Log:\n"  ++ w       ++ "\n" ++
>                                   "Count: " ++ show s  ++ "\n" ++
>                                   "Value: " ++ show v  ++ "\n"

after which we get

~~~~~{.haskell}
*Main> print $ evalWSE ok

*Main> print $ evalWSE err
~~~~~

*How come we didn't get any log in the error case?* 

The answer lies in the *order* in which we compose the transformers; 
since the error wraps the log, if the computation fails, the log gets 
thrown away. Instead, we can just wrap the other way around

> type ESW a = ErrorT String (WriterT String (State Int)) a          
>
> evalESW :: Expr -> ESW Int
> evalESW = eval2

after which, everything works just fine!

~~~~~{.haskell}
*Main> evalESW err
~~~~~

> instance Show a => Show (ESW a) where 
>   show m = "Log:\n"  ++ log ++ "\n" ++ 
>            "Count: " ++ show cnt ++ "\n" ++
>            result
>     where ((res, log), cnt) = runState (runWriterT (runErrorT m)) 0
>           result   = case res of 
>                        Left s -> "Error: " ++ s
>                        Right v -> "Value: " ++ show v

[1]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Either
[2]: http://oreilly.com/catalog/hfdesignpat/chapter/ch03.pdf
[3]: http://en.wikipedia.org/wiki/Python_syntax_and_semantics#Decorators
[4]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#v:state
[5]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Class.html#t:MonadState
[6]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Error-Class.html#t:MonadError
[7]: http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Writer-Lazy.html#t:Writer
