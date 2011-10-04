Programming with Effects III
============================

> {-# LANGUAGE NoImplicitPrelude #-}
> import Prelude hiding (getLine, putStrLn, (>>))
> import System.IO hiding (putStrLn)
> import Data.Char

Arbitrary monads
================

An important benefit of abstracting out the notion of a monad into 
a single typeclass, is that it then becomes possible to define a 
number of useful functions that work in an arbitrary monad.  


We've already seen this in the `pairs` function


> pairs xs ys = do
>   x <- xs
>   y <- ys
>   return (x, y)


What do you think the type of the above is ? (I left out an annotation
deliberately!)

~~~~~{.haskell}
ghci> :type pairs
~~~~~

It takes two monadic values and returns a single *paired* monadic value.
Be careful though! The function above will behave differently depending
on what specific monad instance it is used with! If you use the `Maybe`
monad

~~~~~{.haskell}
ghci> pairs (Nothing) (Just 'a')


ghci> pairs (Just 42) (Nothing)


ghci> pairs (Just 2) (Just 'a')

~~~~~

this generalizes to the list monad 

~~~~~{.haskell}
ghci> pairs [] ['a'] 


ghci> pairs [42] []


ghci> pairs [2] ['a'] 


ghci> pairs [1,2] "ab" 

~~~~~

However, the behavior is quite different with the `IO` monad

~~~~~{.haskell}
ghci> pairs getChar getChar

~~~~~

Other common generic operations can be adapted for monadic
programming.  For example, the `map` function on lists can be
generalized as follows:

> liftM     :: Monad m => (a -> b) -> m a -> m b
> liftM f mx = undefined







Similarly, `concat` on lists generalizes to:

> join    :: Monad m => m (m a) -> m a
> join mmx = undefined







As a final example, we can define a function that transforms
a list of monadic expressions into a single such expression that
returns a list of results, by performing each of the argument
expressions in sequence and collecting their results:


> sequence          :: Monad m => [m a] -> m [a]
> sequence          = undefined









Monads As Programmable Semicolon
--------------------------------

It is sometimes useful to sequence two monadic expressions,
but discard the result value produced by the first:


> (>>)     :: Monad m => m a -> m b -> m b
> mx >> my =  undefined



For example, in the state monad the `>>` operator is just normal
sequential composition, written as `;` in most languages. Without
using layout for the `do` notation, sequencing is a semicolon too.

> hello :: IO ()
> hello = do { putChar 'H'; 
>              putChar 'e'; 
>              putChar 'l'; 
>              putChar 'l'; 
>              putChar 'o' }


Indeed, in Haskell the entire `do` notation with or without `;` is 
just [syntactic sugar][4] for `>>=` and `>>`. For this reason, we can 
legitimately say that Haskell has a [*programmable semicolon*][5].


Exercise
--------

- Define `liftM` and `join` more compactly by using `>>=`.

- Explain the behavior of sequence for the maybe monad.


The monad laws
==============

Earlier we mentioned that the notion of a monad requires that the
return and `>>=` functions satisfy some simple properties.    The
first two properties concern the link between return and `>>=`:

~~~~~{.haskell}
return x >>= f  =  f x	   --	(1)

mx >>= return   =  mx	   --	(2)
~~~~~

Intuitively, equation (1) states that if we return a value `x` and
then feed this value into a function `f`, this should give the same
result as simply applying `f` to `x`.  Dually, equation (2) states
that if we feed the results of a computation `mx` into the function
return, this should give the same result as simply performing `mx`.
Together, these equations express --- modulo the fact that the
second argument to `>>=` involves a binding operation --- that
return is the left and right identity for `>>=`.

The third property concerns the link between `>>=` and itself, and
expresses (again modulo binding) that `>>=` is associative:

~~~~~{.haskell} 
(mx >>= f) >>= g  =  mx >>= (\x -> (f x >>= g)) 	-- (3)
~~~~~

Note that we cannot simply write `mx >>= (f >>= g)` on the right 
hand side of this equation, as this would not be type correct.

As an example of the utility of the monad laws, let us see how they
can be used to prove a useful property of the `liftM` function above,
namely that it distributes over the composition operator for
functions, in the sense that:

~~~~~{.haskell}
liftM (f . g)  =  liftM f . liftM g
~~~~~

This equation generalizes the familiar distribution property of
map from lists to an arbitrary monad.  In order to verify this
equation, we first rewrite the definition of `liftM` using `>>=`:

~~~~~{.haskell}
liftM f mx  =  mx >>= \x -> return (f x)
~~~~~

Now the distribution property can be verified as follows:

~~~~~{.haskell}
(liftM f . liftM g) mx 

     = ...

     = liftM (f . g) mx
~~~~~

Exercise
--------

Show that the maybe monad satisfies equations (1), (2) and (3).


More on the IO monad  
====================

Associated reading: RWH ch. 7

Last time, we briefly talked about IO a --- the type of interactive
programs in Haskell. The type `IO a` is the type of "actions" that
return a result of type `a`, but may also perform some input/output.
A number of primitives are provided for building values of this type,
including:

~~~~~{.haskell}
return  :: a -> IO a
(>>=)   :: IO a -> (a -> IO b) -> IO b
getChar :: IO Char
putChar :: Char -> IO ()
~~~~~

For example, the action that reads a string of characters from
the keyboard can be defined as follows:

> getLine :: IO String
> getLine = undefined


Likewise, we can put a string of characters, with a newline at the
end.

> putStrLn :: String -> IO ()
> putStrLn = undefined





(If you have been warned away from the use of recursion in imperative
languages, don't worry here. In Haskell, recursion is common, and GHC
is smart enough to optimize tail-recursive functions so that they can
run in constant stack space.)


The mind-bending thing about IO in Haskell is that IO actions are
first-class values. They do not produce an effect when they are
evaluated. For example, the following definition

> putThreeTimes :: IO ()
> putThreeTimes = do putChar 'a' >> putChar 'b' >> putChar 'c'

does not immediately print out "abc", it is an IO action that will
print to the screen only when it is called by something in the IO
context.

Likewise, actions that are defined in functions, but never called 
never get performed. 

> f :: Char -> Char
> f x = let _ = putStrLn "Here" in toUpper x 


Lazy IO
-------

What if we want to read and write from files? That is also in the IO monad.
The following types and functions are defined in the module 'System.IO'.

~~~~~{.haskell}
type Handle   = ... 
type FilePath = String
data IOMode   = ReadMode | WriteMode | ReadWriteMode | AppendMode


openFile       :: FilePath -> IOMode -> IO Handle
openBinaryFile :: FilePath -> IOMode -> IO Handle

hGetChar       :: Handle -> IO Char
hGetLine       :: Handle -> IO String
hPutStrLn      :: Handle -> String -> IO ()

hClose         :: Handle -> IO ()
~~~~~

As in other languages, there are builtin file handles for `stdin`, 
`stdout` and `stderr`. 

However, as well as reading a file line by line, Haskell provides access
to the *whole file at once*.  The function 

~~~~~~~~{.haskell}
hGetContents :: Handle -> IO String
~~~~~~~~

will return, the entire contents of the file in a string. How could
this possibly be a good idea?

What happens when you run this file?

> toUpperLazy :: IO ()
> toUpperLazy = do
>   inh <- openFile "pg2.txt" ReadMode
>   outh <- openFile "output.txt" WriteMode
>   inpStr <- hGetContents inh
>   hPutStr outh (map toUpper inpStr)
>   hClose inh
>   hClose outh


Lazy IO means that you can isolate the effectful parts from your
program from the pure parts. You can think about processing the input,
as a whole, with a pure function (such as map toUpper). This separates
the reasoning about IO (which is order dependent) from the reasoning
about String processing.







A word of warning
-----------------

Note that you shouldn't keep hold of the input string, otherwise
Haskell will *have* to load the entire program into memory.

> toUpperLazyBad :: IO ()
> toUpperLazyBad = do
>   inh <- openFile "pg2.txt" ReadMode
>   outh <- openFile "output.txt" WriteMode
>   outh2 <- openFile "output2.txt" WriteMode
>   inpStr <- hGetContents inh
>   let result = map toUpper inpStr
>   hPutStr outh (map toLower inpStr)
>   hPutStr outh (map toLower inpStr)
>   hClose inh
>   hClose outh




Other topics
------------

The subject of monads is a large one, and we have only scratched
the surface here.  If you are interested in finding out more, 
two suggestions for further reading would be to look at "monads
with a zero and plus" (which extend the basic notion with two 
extra primitives that are supported by some monads), and "monad
transformers" (which provide a means to combine monads.)  For
example, see sections 3 and 7 of the following article, which
concerns the monadic nature of [functional parsers][3]
For a more in-depth exploration of the IO monad, see Simon Peyton
Jones' excellent article on the ["awkward squad"][2]

[1]: http://en.wikipedia.org/wiki/Gofer_(software) "Gofer Language"
[2]: http://research.microsoft.com/Users/simonpj/papers/marktoberdorf/ "Awkward Squad"
[3]: http://www.cs.nott.ac.uk/~gmh/monparsing.pdf "Functional Parsers"
[4]: http://book.realworldhaskell.org/read/monads.html#monads.do
[5]: http://donsbot.wordpress.com/2007/03/10/practical-haskell-shell-scripting-with-error-handling-and-privilege-separation/
