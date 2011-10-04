
OK, we're going to write a Haskell program to encode and decode text
files using a secret code.  We'll call it the Brown Fox code.  Here's
how it works:

    - Replace each letter according to the following correspondence:

            "abcdefghijklmnopqrstuvwxyz"
        to  "thequickbrownfxjmpsvlazydg"

      But leave any non-letter characters alone.

    - Then reverse the order of the lines in the file.

> {-# OPTIONS -Wall #-}
> module Main where

> import Data.Char
> import Data.Maybe


> type Code = [(Char,Char)]

> code1 :: Code
> code1 =    (zip ['a' .. 'z'] cypher)
>         ++ (zip ['A' .. 'Z'] (map toUpper cypher))
>   where 
>     cypher :: String
>     cypher = "thequickbrownfxjmpsvlazydg"

> code2 :: Code
> code2 = map (\(a,b) -> (b,a)) code1


> encodeChar :: Code -> Char -> Char
> encodeChar code c = fromMaybe c (lookup c code)

> encodeLine :: Code -> String -> String
> encodeLine code = map (encodeChar code)

> encodeContent :: Code -> String -> String
> encodeContent code = unlines . (map $ encodeLine code) . reverse . lines

> encodeFile :: Code -> FilePath -> IO ()
> encodeFile code f = 
>    do fcontents <- readFile f
>       writeFile (f ++ ".code") (encodeContent code fcontents)


> main :: IO ()
> main = do putStrLn "Shall I encode or decode?"
>           encdec <- getLine
>           code <- case encdec of
>                     "encode" -> return code1
>                     "decode" -> return code2
>                     _ -> error $ "I don't know how to " ++ encdec ++ "!"
>           putStrLn $ "What file shall I " ++ encdec ++ "?"
>           fn <- getLine
>           encodeFile code fn
>           putStrLn "All done!"
