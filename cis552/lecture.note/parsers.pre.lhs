Monadic Parsing 
===============

Associated Reading: RWH chapters 10 and 16

> import Data.Char
> import Control.Monad


What is a Parser?
-----------------

A parser is a piece of software that takes a raw `String` (or sequence
of bytes) and returns some structured object, for example, a list of
options, an XML tree or JSON object, a program's Abstract Syntax Tree
and so on.  Parsing is one of the most basic computational
tasks. *Every* serious software system has a parser tucked away
somewhere inside, for example

- Shell Scripts (command-line options)
- Web Browsers (duh!)
- Games (level descriptors)
- Routers (packets)
- etc

(Indeed I defy you to find any serious system that *does not* do some
parsing somewhere!)

The simplest and most accurate way to think of a parser is as a
function

~~~~~{.haskell}
type Parser = String -> StructuredObject
~~~~~

Composing Parsers
-----------------

The usual way to build a parser is by specifying a grammar and using a
parser generator (eg yacc, bison, antlr) to create the actual parsing
function. While elegant, one major limitation of the grammar based 
approach is its lack of modularity. For example, suppose I have two 
kinds of primitive values `Thingy` and `Whatsit`. 


       Thingy : rule 	{ action } 
       ;

       Whatsit : rule  { action }
       ;


If you want a parser for *sequences of* `Thingy` and `Whatsit` we have
to painstakingly duplicate the rules as


      Thingies : Thingy Thingies  { ... } 
                 EmptyThingy      { ... }
      ;
      
      Whatsits : Whatsit Whatsits { ... }
                 EmptyWhatsit     { ... }
      ;



This makes sub-parsers hard to reuse. Next, we will see how to
*compose* mini-parsers for sub-values to get bigger parsers for
complex values.

To do so, we will generalize the above parser type a little bit, by
noting that a (sub-)parser need not (indeed, will not) consume consume
*all* of its input, and so we can simply have the parser return the
unconsumed input

~~~~~{.haskell}
type Parser = String -> (StructuredObject, String) 
~~~~~

Of course, it would be silly to have different types for parsers for
different kinds of objects, and so we can make it a parameterized type.

~~~~~{.haskell}
type Parser a = String -> (a, String) 
~~~~~

One last generalization: the parser could return multiple results, for
example, we may want to parse the string

~~~~~{.haskell}
"2 + 3 + 4"
~~~~~

either as

~~~~~{.haskell}
Plus (Plus 2 3) 4
~~~~~

or as 

~~~~~{.haskell}
Plus 2 (Plus 3 4)
~~~~~

So, we can have our parsers return a *list* of possible results, where
the empty list corresponds to a failure to parse. 

~~~~~{.haskell}
type Parser a = String -> [(a, String)]
~~~~~

As the last step, we willwrap this type definition up in a `newtype`,
which is like a datatype that has only one constructor. This will make 
sure that we keep parsers distinct from other values of this type
(and will allow us to make class instances).

> newtype Parser a = P (String -> [(a, String)])

The above is simply the parser, which we will build up
compositionally.  The actual parsing is done when we use the parser
and apply it to the input stream.


> doParse :: Parser a -> String -> [(a,String)]
> doParse (P p) s = p s


Lets build some parsers!


Parse A Single character
-------------------------

Here's a *very* simple character parser, that returns the first `Char`
from a list if one exists

> oneChar :: Parser Char
> oneChar = P undefined

Lets run the parser

~~~~~{.haskell}
ghci> doParse oneChar "hey!"

ghci> doParse oneChar ""

~~~~~

Parser Composition
------------------



Now we can write another parser that grabs a pair of `Char` values


> twoChar0 :: Parser (Char, Char)
> twoChar0  = undefined










Or, it is more general to write a *combinator* that takes two parsers
and returns a new parser that returns a pair of values

> pairP0 ::  Parser a -> Parser b -> Parser (a,b)
> pairP0 p1 p2 = undefined


and use that to rewrite 'twoChar' more elegantly as

> twoChar = pairP0 oneChar oneChar 

which would run like this

~~~~~{.haskell}
ghci> doParse twoChar "hey!"


ghci> doParse twoChar ""

~~~~~

Now we could keep doing this, but often to go forward, it is helpful to
step back and take a look at the bigger picture.
Here's the the *type* of a parser

~~~~~{.haskell}
newtype Parser a = P (String -> [(a, String)])
~~~~~

it should remind you of something else, remember this?

~~~~~{.haskell}
newtype State s a = S (s -> (a, s))
~~~~~


Parser is A Monad
=================

Indeed, a parser, like a state transformer, [is a monad!][2] 
if you squint just the right way. We need to define the `return`
and `>>=` functions. 

The first is very simple, we can let the types guide us. We
must ignore the input string and just return the input element.

> returnP :: a -> Parser a
> returnP x = P undefined

The bind is a bit more tricky, but again, lets lean 
on the types:

> bindP :: Parser a -> (a -> Parser b) -> Parser b
> p1 `bindP` fp2 = P undefined

So, we need to suck the `a` values out of the first 
parser and invoke the second parser with them on the 
remaining part of the string.





Armed with those, we can officially brand parsers 
as monads

> instance Monad Parser where
>   (>>=)  = bindP
>   return = returnP


Parser Combinators
==================

Since parsers are monads, we can write a bunch of high-level
combinators for composing smaller parsers into bigger ones.

For example, we can use our beloved `do` notation to rewrite 
the `pairP` as

> pairP :: Parser a -> Parser b -> Parser (a,b)
> pairP p1 p2 = do x <- p1
>                  y <- p2
>                  return (x, y)

shockingly, exactly like the `pairs` function we have seen before.

Next, lets flex our monadic parsing muscles and write some new 
parsers. It will be helpful to have a *failure* parser that 
always goes down in flames (returns `[]`)

> failP :: Parser a
> failP = P undefined

Seems a little silly to write the above, but its helpful to 
build up richer parsers like the following which parses a 
`Char` *if* it satisfies a predicate `p`

> satP ::  (Char -> Bool) -> Parser Char
> satP p = undefined

Note that we are working abstractly here, we've defined satP
without using the "P" data constructor or "doParse". We are 
building up parsers programmatically, from smaller components.

With this, we can write some simple parsers for particular characters.
The following parse alphabet and numeric characters respectively

> alphaChar, digitChar :: Parser Char
> alphaChar = satP isAlpha
> digitChar = satP isDigit

~~~~~~~~~~~~~~~~~~.{haskell}
ghci> doParse alphaChar "123" 

ghci> doParse digitChar "123"
~~~~~~~~~~~~~~~~~~~





and this little fellow returns the first digit in a string as an `Int`

> digitInt :: Parser Int
> digitInt  = do 
>   c <- digitChar
>   return $ ord c - ord '0'


which works like so

~~~~~{.haskell}
ghci> doParse digitInt "92"


ghci> doParse digitInt "cat"

~~~~~

Finally, this parser will parse only a particular `Char` passed in as
input

> char :: Char -> Parser Char
> char c = undefined

~~~~~~~~~~~{.haskell}
ghci> doParse (char 'a') "ab" 

~~~~~~~~~~~~~~~~~~~~~






A Nondeterministic Choice Combinator
------------------------------------

Next, lets write a combinator that takes two sub-parsers and 
nondeterministically chooses between them. 

> chooseP :: Parser a -> Parser a -> Parser a

How would we go about encoding *choice* in our parsers? 
Well, we want to return a succesful parse if *either* 
parser succeeds. Since our parsers return multiple values, 
we can simply return the *union* of all the results!

> p1 `chooseP` p2 = P undefined

We can use the above combinator to build a parser that 
returns either an alphabet or a numeric character

> alphaNumChar = alphaChar `chooseP` digitChar

When we run the above we get some rather interesting results

~~~~~{.haskell}
ghci> doParse alphaNumChar "cat"

ghci> doParse alphaNumChar "2cat"

ghci> doParse alphaNumChar "2at"

~~~~~

What is even nicer is that if *both* parsers succeed, 
you end up with all the results. For example, heres a 
parser that grabs `n` characters from the input 

> grabn :: Int -> Parser String 
> grabn n | n <= 0    = return ""
>         | otherwise = do c  <- oneChar  
>                          cs <- grabn (n-1)
>                          return (c:cs)

(Challenge: can you remove the recursion from that?)

Now, we can use our choice combinator 

> grab2or4 = grabn 2 `chooseP` grabn 4

and now, we will get back *both* results if possible

~~~~~{.haskell}
ghci> doParse grab2or4 "mickeymouse"

~~~~~

and only one result if that's possible

~~~~~{.haskell}
ghci> doParse grab2or4 "mic"


ghci> doParse grab2or4 "m"

~~~~~

Even with the rudimentary parsers we have at our disposal, we can start
doing some rather interesting things. For example, here is a little
calculator. First, we parse the operation


> intOp = plus `chooseP` minus `chooseP` times `chooseP` divide 
>   where plus   = char '+' >> return (+)
>         minus  = char '-' >> return (-)
>         times  = char '*' >> return (*)
>         divide = char '/' >> return div



(can you guess the type of the above parser?) And then we parse the
expression

> calc :: Parser Int
> calc = do x  <- digitInt
>           op <- intOp
>           y  <- digitInt 
>           return $ x `op` y

which, when run, will both parse and calculate

~~~~~{.haskell}
ghci> doParse calc "8/2"


ghci> doParse calc "8+2cat"


ghci> doParse calc "8/2cat"


ghci> doParse calc "8-2cat"


ghci> doParse calc "8*2cat"

~~~~~


Recursive Parsing
-----------------

To start parsing interesting things, we need to add recursion 
to our combinators. For example, its all very well to parse 
individual characters (as in `char` above) but it would a lot 
more swell if we could grab particular `String` tokens. 

Lets try to write it! 

> string :: String -> Parser String


~~~~~{.haskell}
string ""     = return ""
string (c:cs) = do char c
                   string cs
                   return $ c:cs
~~~~~

Ewww! Is that explicit recursion ?! Lets try again (can you spot the
pattern)

> string = undefined

Much better!

~~~~~{.haskell}
ghci> doParse (string "mic") "mickeyMouse"


ghci> doParse (string "mic") "donald duck"

~~~~~

Ok, I guess that wasn't really recursive then after all! Lets try
again.  Lets write a combinator that takes a parser `p` that returns
an `a` and returns a parser that returns *many* `a` values. That is,
it keeps grabbing as many `a` values as it can and returns them as 
`[a]`.

i.e. we want a parser that parses one thing, then calls itself
recursively or always succeeds (consuming no input).

> manyP   :: Parser a -> Parser [a]
> manyP p = undefined


Beware, the above can yield *many* results

~~~~~{.haskell}
ghci> doParse (manyP digitInt) "123a" 

~~~~~

which is simply all the possible ways to extract sequences 
of integers from the input string.




Deterministic Maximal Parsing
-----------------------------

Often we want a single result, not a set of results. For example,
the more intuitive behavior of `many` would be to return the maximal
sequence of elements and not *all* the prefixes.

To do so, we need a *deterministic* choice combinator. This combinator
returns no more than one result (i.e. it runs the choice parser 
but discards extra results).

> (<|>) :: Parser a -> Parser a -> Parser a
> p1 <|> p2 = P undefined
>               


Now, we can revisit the `manyP` combinator and ensure that it 
returns a single, maximal sequence. 

> mmanyP   :: Parser a -> Parser [a]
> mmanyP p = undefined

~~~~~{.haskell}
ghci> doParse (mmanyP digitInt) "123a" 

~~~~~

Let's use the above to write a parser that will return an entire
natural number (not just a single digit.)


> oneNat0 :: Parser Integer
> oneNat0 = do xs <- mmanyP digitChar 
>              return $ ((read xs) :: Integer)


*Aside*, can you spot the pattern above? We took the 
parser `mmanyP digitChar` and simply converted its output
using the `read` function. This is a recurring theme, and 
the type of what we did gives us a clue

~~~~~{.haskell}
(a -> b) -> Parser a -> Parser b
~~~~~

Aha! a lot like `map`. Indeed, there is a generalized version
of `map` that we have seen before (`liftM`) and we bottle up 
the pattern by declaring `Parser` to be an instance of the 
`Functor` typeclass

> instance Functor Parser where
>   fmap f p = do x <- p
>                 return (f x)

after which we can rewrite

> oneNat :: Parser Int
> oneNat = read `fmap` mmanyP digitChar 

Lets take it for a spin

~~~~~{.haskell}
ghci> doParse oneNat "123a"

~~~~~


Parsing Arithmetic Expressions
==============================

Lets use the above to build a small calculator, that
parses and evaluates arithmetic expressions. In essence, 
an expression is either binary operand applied to two 
sub-expressions or an integer. We can state this as


> calc1 ::  Parser Int
> calc1 = binExp <|> oneNat 
>   where binExp = do x <- calc1 
>                     o <- intOp 
>                     y <- oneNat
>                     return $ x `o` y


This works pretty well!

~~~~~{.haskell}
ghci> doParse calc1 "1+2+33"


ghci> doParse calc1 "11+22-33"

~~~~~

but things get a bit strange with minus

~~~~~{.haskell}
ghci> doParse calc1 "11+22-33+45"

~~~~~

Huh? Well, if you look back at the code, you'll realize the 
above was parsed as

~~~~~{.haskell}
11 + ( 22 - (33 + 45))
~~~~~

because in each `binExp` we require the left operand to be 
an integer. In other words, we are assuming that each 
operator is *right associative* hence the above result. 

Even worse, we have no precedence, and so

~~~~~{.haskell}
ghci> doParse calc1 "10*2+100"

~~~~~

as the string is parsed as

~~~~~{.haskell}
10 * (2 + 100)
~~~~~

Precedence
----------

We can add both associativity and precedence in the usual way, by
stratifying the parser into different levels. Here, lets split our 
operations into addition- and multiplication-precedence.

> addOp :: Parser (Int -> Int -> Int)
> addOp = plus `chooseP` minus 
>   where plus   = char '+' >> return (+)
>         minus  = char '-' >> return (-)
>
> mulOp :: Parser (Int -> Int -> Int)
> mulOp = times `chooseP` divide 
>   where times  = char '*' >> return (*)
>         divide = char '/' >> return div

Now, we can stratify our language into (mutually recursive) 
sub-languages, where each top-level expression is parsed as 
a *sum-of-products* 

> sumE :: Parser Int
> sumE = addE <|> prodE 
>   where addE = do x <- prodE 
>                   o <- addOp
>                   y <- sumE 
>                   return $ x `o` y
>
> prodE :: Parser Int
> prodE = mulE <|> factorE
>   where mulE = do x <- factorE
>                   o <- mulOp
>                   y <- prodE 
>                   return $ x `o` y
>
> factorE :: Parser Int
> factorE = parenE <|> oneNat
>   where parenE = do char '('
>                     n <- sumE 
>                     char ')'
>                     return n

We can run this 

~~~~~{.haskell}
ghci> doParse sumE "10*2+100"


ghci> doParse sumE "10*(2+100)"

~~~~~

Do you understand why the first parse returned `120` ?
What would happen if we *swapped* the order of `prodE`
and `sumE` in the body of `addE` (or `factorE` and `prodE` 
in the body of `prodE`) ? Why?

Parsing Pattern: Chaining
-------------------------

There is not much point gloating about combinators if we are 
going to write code like the above -- the bodies  of `sumE` 
and `prodE` are almost identical!

Lets take a closer look at them. In essence, an `sumE` is 
of the form

~~~~~{.haskell}
prodE + < prodE + < prodE + ... < prodE >>>
~~~~~

that is, we keep chaining together `prodE` values and 
adding them for as long as we can. Similarly a `prodE` 
is of the form

~~~~~{.haskell}
factorE * < factorE * < factorE * ... < factorE >>>
~~~~~

where we keep chaining `factorE` values and multiplying 
them for as long as we can. There is something unpleasant 
about the above: the addition operators are right-associative

~~~~~{.haskell}
ghci> doParse sumE "10-1-1"

~~~~~

Ugh! I hope you understand why: its because the above was 
parsed as `10 - (1 - 1)` (right associative) and not
`(10 - 1) - 1` (left associative). You might be tempted 
to fix that simply by flipping the order of `prodE` and 
`sumE`

~~~~~{.haskell}
sumE = addE <|> prodE 
  where addE = do x <- sumE 
                  o <- addOp
                  y <- prodE 
                  return $ x `o` y
~~~~~

but this would prove disastrous. Can you see why?  The parser for
`sumE` directly (recursively) calls itself *without consuming any
input!* Thus, it goes off the deep end and never comes back. Instead,
we want to make sure we keep consuming `prodE` values and adding them
up (rather like fold) and so we could do


> sumE1 :: Parser Int
> sumE1 = prodE1 >>= rest 
>   where rest x = next x <|> return x
>         next x = do o <- addOp
>                     y <- prodE1 
>                     rest $ x `o` y
>
> prodE1 :: Parser Int
> prodE1 = factorE1 >>= rest
>   where rest x = next x <|> return x
>         next x = do o <- mulOp
>                     y <- factorE1 
>                     rest $ x `o` y
>
> factorE1 :: Parser Int
> factorE1 = parenE <|> oneNat
>   where parenE = do char '('
>                     n <- sumE1 
>                     char ')'
>                     return n

It is easy to check that the above is indeed left associative.

~~~~~{.haskell}
ghci> doParse sumE1 "10-1-1"

~~~~~

and it is also very easy to spot and bottle the chaining computation
pattern: the only differences are the *base* parser 
(`prodE1` vs `factorE1`) and the binary operation (`addOp` vs `mulOp`).
We simply make those parameters to our *chain-left* combinator

> chainl :: Parser b -> Parser (b -> b -> b) -> Parser b
> p `chainl` pop = p >>= rest
>    where rest x = next x <|> return x 
>          next x = do o <- pop
>                      y <- p
>                      rest $ x `o` y 

Similarly, we often want to parse bracketed expressions, so we can
write a combinator

> parenP :: Char -> Parser b -> Char -> Parser b
> parenP l p r = do char l
>                   x <- p
>                   char r
>                   return x

after which we can rewrite the grammar in three lines

> sumE2    = prodE2   `chainl` addOp
> prodE2   = factorE2 `chainl` mulOp
> factorE2 = parenP '(' sumE2 ')' <|> oneNat 

~~~~~{.haskell}
ghci> doParse sumE2 "10-1-1"


ghci> doParse sumE2 "10*2+1"


ghci> doParse sumE2 "10+2*1"


~~~~~

That concludes our (in-class) exploration of monadic parsing. This is
merely the tip of the iceberg. Though parsing is a very old problem,
and has been studied since the dawn of computing, we saw how monads
bring a fresh perspective which have recently been transferred from
Haskell to [many other languages][3]. There have been several exciting
[recent][4] [papers][5] on the subject, that you can explore on your
own.  Finally, Haskell comes with several parser combinator libraries
including [Parsec][3] which you will play around with in your
next homework assignment.

[2]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
[3]: http://www.haskell.org/haskellwiki/Parsec
[4]: http://www.cse.chalmers.se/~nad/publications/danielsson-parser-combinators.html
[5]: http://portal.acm.org/citation.cfm?doid=1706299.1706347



