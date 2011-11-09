A Poor Man's Concurrency Monad
==============================

> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}

> import Data.Monoid
> import Control.Monad
> import Control.Monad.Writer
> import Control.Monad.Trans
> import System.IO
> import Data.IORef

This lecture talks about embedding concurrency in a single-threaded
language. It isn't the same as "real" concurrency, where
multiprocessors run in parallel. Instead it is "time-sharing"
concurrency, where multiple 'threads' of execution share the same
single, uniprocessor.  These threads are not pre-emptive, they will
run as long as they wish, only relinquishing control at certain
points. 

The basis for this lecture is a paper published in 1999, a time when
laptops and personal computers were uniformly uniprocessors.

    Koen Claessen, A Poor Man's Concurrency Monad, JFP 1999.

Why would you want such concurrency, even today? 
------------------------------------------------

 - To hide latency, i.e. non-blocking File IO.  Disk access is slow,
and the processor could be doing useful stuff while waiting for the
disk to return.

 - To express concurrent applications. The world is naturally
concurrent. A processor has many forms of interaction, from the
network, to the disk, to the display, to the keyboard/mouse, etc, and
a single application may wish to support these interactions, even
while doing perhaps long-running computations.

 - To avoid locks/etc. Since this is "time-sharing" concurrency, which
is going to be run on a single processor, we know that each thread
could be interrupted *only* at specific points. So we don't need
locking to guarantee about atomicity, significantly simplifying 
concurrent programming.

- Furthermore, this library is all user code, so it allows *you*
access to concurrency "internals" such as the thread-scheduling
algorithm. If your application requires specific control over how
thread scheduling works, then you can modify the implementation of the
monad.


How do we simulate concurrency on a uniprocessor?
--------------------------------------------------

The standard technique is *interleaving*, i.e. running first part of
one thread, then suspending it, and then allowing another thread to
run.

To suspend a thread we need access to its "future computation", in
programming language terminology, this is often referred to as a
*continutation*.

The concurrency monad includes all computations that have access to
their continuations. A computation in this monad is a function whose
first argument is its continuation, i.e. a function that the
computation will call when it wants to "return" its value, i.e. pass
it to the next step in the computation. 

We're going to divide up computations into slices called actions, so
all computations in this monad should themselves be `Action`s, and
their continuations should likewise produce new `Action`s.  An action
can be an atomic action, a concurrency operation (such as forking a
new action), or the terminal action that halts the thread. We'll get
time sharing by running actions from multiple threads in a round-robin
fashion.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type Action = 
    Atom Action         -- do an atomic computation, returning a new action
  | Fork Action Action  -- fork computation into two different actions
  | Stop                -- halt the computation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}

Putting this all together, we get the following definition for the 
concurrency monad:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type C a = (a -> Action) -> Action
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can build up larger computations using bind and return.

To sequence computations, we first abstract the current continuation
k, then run the first computation m, giving it a contination that next
runs f with the value produced by m and the original continuation.

When we are done, we 'return' the final answer by giving it to the
continuation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type C a = (a -> Action) -> Action

instance Monad C where

   (>>=) ::  C a -> (a -> C b) -> C b
   m >>= f  = \ ( k :: b -> Action) -> m ( \ a -> f a k ) 
  
   return :: a -> C a 
   return x = \ k -> k x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

However, there is one more wrinkle. We want to parameterize the Action
type by a monad so that the atomic computation can be monadic. This is
necessary, for example, if we want to do IO operations.

> data Action m = 
>        Atom (m (Action m))           -- do some work
>      | Fork (Action m) (Action m)    -- create a new thread
>      | Stop                          -- finish the thread


Therefore, our definition of the monad takes is also parameterized by
'm', this underlying monad of atomic computations.

> newtype C m a = C { apply :: (a -> Action m) -> Action m }

We can show that `C m` is a monad using essentially the same 
code as above, after taking account of the newtype constructor.

> instance Monad (C m) where
>
>   m >>= f  = C $ \ k -> apply m ( \ a -> apply (f a) k ) 
>
>   return x = C $ \ k -> k x

The `atom` function turns an arbitrary computation in the monad m into
an atomic action in `C m`. An atomic action is one that that runs the
monadic computation and then passes its result to the
continuation. (We know that the monadic computation m will not be
interrupted, that is why this is called "atomic".)

> atom :: Monad m => m a -> C m a 
> atom m = C $ \ k -> Atom $ do { a <- m; return (k a) }

The atom operation is what makes the Concurrency monad a monad
transformer. It is how we lift operations from one monad into the
concurrency monad.
                                        
> instance MonadTrans C where
>   -- lift :: Monad m => m a -> C m a
>   lift = atom

We next give wrappers for the other concurrency operations.           
           
The `stop` function discards any continuation, ending a computation.   

> stop :: Monad m => C m a 
> stop = C $ \k -> Stop

For concurrency, we have two variants. The first operation is
symmetric, and combines two concurrent computations into one by
forking and passing the same continuation to each part.

> par :: Monad m => C m a -> C m a -> C m a
> par m1 m2 = C $ \k -> Fork (apply m1 k) (apply m2 k)

The second operation is more similar to the standard fork. It turns
its argument into an action and then passes unit to the continuation.

> fork :: Monad m => C m () -> C m ()
> fork m = C $ \k -> Fork (action m) (k ())

Above, the operation `action` transforms computations (of type `C m
a`) into actions. All it does is give the computation the 'Stop'
continuation.

> action :: Monad m => C m a -> Action m
> action m = apply m (\k -> Stop)

Semantics
=========

At any moment, the status of the computation is modelled by a
collection of (concurrently running) threads. Each thread state is
represented by its current Action.

For simplicity, we represent that collection as a list and, for
round-robin scheduling, treat that list as a queue. (In a
high-performance system we would use a faster data structure.)

> sched :: Monad m => [Action m] -> m ()
> sched [] = return ()
> sched (a : as) = case a of 
>   Atom am    -> do { a' <- am ; sched (as ++ [a']) }
>   Fork a1 a2 -> sched (as ++ [a1,a2])
>   Stop       -> sched as

Running a computation is merely turning it into an action and then
giving it to the thread scheduler.

> run :: Monad m => C m a -> m ()
> run m = sched  [ action m ]

Example - Concurrent Output
===========================

Now let's see some examples of concurrency! The first example involves
concurrent output---two threads writing to the screen at the same
time. To make sure that we get the full effect of concurrency, we'll
first turn off buffering on the standard input and output sources:

Main*> hSetBuffering stdout NoBuffering
Main*> hSetBuffering stdin NoBuffering

Next, we'll define a class of monads that support output. These are
the ones that have a 'write' operation.

> class Monad m => Output m where  
>    write :: String -> m ()

For example, we can make the 'IO' monad a member of this class.

> instance Output IO where
>    write = putStr

Now, here is an infinite loop that just writes its argument. 

> loop :: Output m => String -> m ()
> loop s = do write s 
>             loop s

If we run this loop from the toplevel (in the IO monad) we don't get
to do anything else.

~~~~~~{.haskell}
*Main> loop "CIS 552"
~~~~~~

But with concurrency, we make this loop run in parallel with other
computations. To do that, we'll need to run the loop in the
concurrency monad. Therefore, we need to make the concurrency monad a
member of the output class. Because 'C' is a monad transformer, that 
is not too difficult to do.

> instance Output m => Output (C m) where
>    write s = lift (write s)

> example :: Output m => C m ()
> example = do write "start!"
>              fork (loop "dog")
>              loop "cat"

We run this computation by giving it to the scheduler.

~~~~~~{.haskell}
*Main> run example
~~~~~~

Note that our implementation of write for the concurrency monad
determines how much interleaving is possible between two different
simultaneous writes.

> -- instance Output m => Output (C m) where
> --    write [] = lift (write [])
> --    write (x:xs) = lift (write [x]) >> write xs

Concurrent Input and Output
===========================

Now suppose we would like threads to read as well as write.  To do that 
we need a class for *asyncronous* input. The method in this class 
reads some input as long as there is some ready. If there is no input 
available, it immediately returns with 'Nothing'.

> class Monad m => Input m where  
>    input :: m (Maybe String)

To implement nonblocking input in the IO monad, we first test to see
if the standard input is ready before we use the standard blocking
operation.

> instance Input IO where
>    input = do x <- hReady stdin
>               if x then liftM Just getLine else return Nothing

For example, we can write a loop that prints out a string until a 
line is entered in the keyboard.

> ioloop :: (Input m, Output m) => String -> m String
> ioloop s = do i <- input
>               case i of 
>                 Just x  -> return ("Thread " ++ s ++ ":" ++ x)
>                 Nothing -> do write s
>                               ioloop s

~~~~~~{.haskell}
*Main> ioloop "CIS 552"
~~~~~~

We can run this thread concurrently with other threads by 
inserting the concurrency monad into the Input class.

> instance Input m => Input (C m) where
>    input = lift input


> example2 :: (Input m, Output m) => C m ()
> example2 = do 
>            fork (ioloop "a" >>= write)
>            fork (ioloop "b" >>= write)


> example3 :: (Input m, Output m) => C m ()
> example3 = do 
>            x <- par (ioloop "a") (ioloop "b")
>            -- write "done!"
>            write x

Shared State 
============

Sometimes threads may wish to communicate with eachother by passing
messages through some shared state. An abstraction designed for that
purpose is a 'MVar'. A MVar is a potentially empty memory
location. Initially the memory location is empty, but it can be
updated to contain information. If the memory location is read, then
the data is removed.

> type MVar a = IORef (Maybe a)

> class Monad m => MVarMonad m where
>   newMVar   :: m (MVar a)
>   writeMVar :: MVar a -> a -> m ()
>   takeMVar  :: MVar a -> m (Maybe a)

> instance MVarMonad IO where
>   newMVar       = newIORef Nothing
>   writeMVar v a = writeIORef v (Just a)
>   takeMVar  v   = do a <- readIORef v
>                      writeIORef v Nothing                      
>                      return a

> instance MVarMonad m => MVarMonad (C m) where
>   newMVar       = undefined
>   writeMVar v a = undefined
>   takeMVar  v   = undefined

Blocking message receiving. The function loops until 
the value of the reference changes.  This operation 
*requires* concurrency to do anything interesting...

> readMVar :: (MVarMonad m) => MVar a -> m a
> readMVar v = undefined

Now here is an example using message passing. We have two threads 
that communicate via messages.

> data Msg = 
>    Add | Reset | Print | Quit

> simulation :: MVar Msg -> Integer -> C IO ()
> simulation mv i = do 
>   x <- takeMVar mv 
>   case x of 
>     Just Add   -> do write "Adding...\n"
>                      simulation mv (i+1)
>     Just Reset -> do write "Resetting...\n"
>                      simulation mv 0
>     Just Print -> do write ("Current value is " ++ show i ++ "\n")
>                      simulation mv i
>     Just Quit  -> do write ("Done\n") 
>     Nothing    -> simulation mv i

> interface :: MVar Msg -> C IO ()
> interface mv = do
>    maybeKey <- input  
>    case maybeKey of 
>      Just "a" -> writeMVar mv Add   >> interface mv
>      Just "r" -> writeMVar mv Reset >> interface mv
>      Just "p" -> writeMVar mv Print >> interface mv
>      Just "q" -> writeMVar mv Quit  >> interface mv
>      Just s   -> write ("Unknown command: " ++ s) >> interface mv
>      Nothing  -> interface mv

> example5 = do 
>    mv <- newMVar
>    par (simulation mv 0) (interface mv)

How do you merge an infinite lists of infinite lists?
-----------------------------------------------------

There is more that can be done with the concurrency, monad! We finish
with a simple, academic question. How do you merge an infinite list of
infinite lists?

For example, where are some infinite lists:

> ones   = '1' : ones
> twos   = '2' : twos
> threes = '3' : threes 

*Main> take 10 ones

*Main> take 20 twos


And here is an infinite list of infinite lists.

> allnums :: [String]
> allnums = undefined 

*Main> take 20 (map (take 20) allnums)

We can use the concurrency monad to merge them!

First, we use make the Writer monad an instance of the Output monad.

> instance Output (Writer String) where
>    write = tell
 
The trick is to write all of the strings concurrently, letter by letter.

> merge :: [String] -> String
> merge = undefined

*Main> take 20 (merge allnums)
