
-- Advanced Programming, HW 4
-- by <Zi Yan> <yanzi>, <Adrian Benton> <adrianb>

import Control.Monad

import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Parser
import ParserCombinators

import Test.HUnit
import Test.QuickCheck


import Data.Char (toLower)

type Variable = String
 
--generate
upperAbcGen :: Gen Char
upperAbcGen = Test.QuickCheck.choose ('A', 'Z')

varGen :: Gen Variable
varGen = liftM2 (:) upperAbcGen $ listOf upperAbcGen

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)

instance Arbitrary Value where
  arbitrary = oneof [fmap IntVal (arbitrary :: Gen Int), 
                     fmap BoolVal (arbitrary :: Gen Bool)]

 
data Expression =
    Var Variable
  | Val Value  
  | Op  Bop Expression Expression
  deriving (Show, Eq)
 
-- Inspired by RealWorld Haskell Ch. 11
-- Seems like there is an elegant way to do some of this using liftM.
-- ...have yet to figure it out.
instance Arbitrary Expression where
  arbitrary = do n <- (Test.QuickCheck.choose (0, 2) :: Gen Int)
                 case n of
                   0 -> do s   <- varGen
                           return $ Var s
                   1 -> do val <- (arbitrary :: Gen Value)
                           return $ Val val
                   2 -> do op  <- (arbitrary :: Gen Bop)
                           e1  <- (arbitrary :: Gen Expression)
                           e2  <- (arbitrary :: Gen Expression)
                           return $ Op op e1 e2
  shrink e = case e of
    Var _       -> []
    Val _       -> []
    Op  o e1 e2 -> map (\e1' -> Op o e1' e2) (shrink e1) ++ 
                   map (\e2' -> Op o e1 e2') (shrink e2) ++ 
                   (shrink e1) ++ (shrink e2) ++ [e1, e2]


data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt        
  | Ge       
  | Lt       
  | Le       
  deriving (Show, Eq)


instance Arbitrary Bop where 
  arbitrary = elements [Plus, Minus, Times, Divide, Gt, Ge, Lt, Le]

{- for precedence use, but not working
   with test.imp, so I commented it.
instance Ord Bop where
   compare Gt Plus      = LT
   compare Gt Minus     = LT
   compare Gt Times     = LT
   compare Gt Divide    = LT
   compare Ge Plus      = LT
   compare Ge Minus     = LT
   compare Ge Times     = LT
   compare Ge Divide    = LT
   compare Lt Plus      = LT
   compare Lt Minus     = LT
   compare Lt Times     = LT
   compare Lt Divide    = LT
   compare Le Plus      = LT
   compare Le Minus     = LT
   compare Le Times     = LT
   compare Le Divide    = LT
   compare Plus Times   = LT
   compare Plus Divide  = LT
   compare Minus Times  = LT
   compare Minus Divide = LT
   compare Plus Gt      = GT
   compare Minus Gt     = GT
   compare Times Gt     = GT
   compare Divide Gt    = GT
   compare Plus Ge      = GT
   compare Minus Ge     = GT
   compare Times Ge     = GT
   compare Divide Ge    = GT
   compare Plus Lt      = GT
   compare Minus Lt     = GT
   compare Times Lt     = GT
   compare Divide Lt    = GT
   compare Plus Le      = GT
   compare Minus Le     = GT
   compare Times Le     = GT
   compare Divide Le    = GT
   compare Times Plus   = GT
   compare Divide Plus  = GT
   compare Times Minus  = GT
   compare Divide Minus = GT
   compare Gt Ge        = EQ
   compare Gt Lt        = EQ
   compare Gt Le        = EQ
   compare Ge Gt        = EQ
   compare Ge Lt        = EQ
   compare Ge Le        = EQ
   compare Lt Ge        = EQ
   compare Lt Gt        = EQ
   compare Lt Le        = EQ
   compare Le Ge        = EQ
   compare Le Gt        = EQ
   compare Le Lt        = EQ
   compare Plus Plus    = EQ
   compare Plus Minus   = EQ
   compare Minus Minus  = EQ
   compare Minus Plus   = EQ
   compare Times Divide = EQ
   compare Times Times  = EQ
   compare Divide Times = EQ
   compare Divide Divide= EQ
-}

data Statement =
    Assign Variable Expression          
  | If Expression Statement Statement
  | While Expression Statement       
  | Sequence Statement Statement        
  | Skip
  deriving (Show, Eq)

nonSeqGen :: Gen Statement
nonSeqGen = do n <- (Test.QuickCheck.choose (0, 10)) :: Gen Int
               case n of
                   n | n < 7 -> do v <- varGen
                                   e <- (arbitrary :: Gen Expression)
                                   return $ Assign v e
                   7 -> do g  <- (arbitrary :: Gen Expression)
                           s1 <- (arbitrary :: Gen Statement)
                           s2 <- (arbitrary :: Gen Statement)
                           return $ If g s1 s2
                   8 -> do g  <- (arbitrary :: Gen Expression)
                           s  <- (arbitrary :: Gen Statement)
                           return $ While g s
                   _ -> return Skip

instance Arbitrary Statement where
  arbitrary = do n <- (Test.QuickCheck.choose (0, 10)) :: Gen Int
                 case n of
                   n | n < 7 -> do v <- varGen
                                   e <- (arbitrary :: Gen Expression)
                                   return $ Assign v e
                   7 -> do g  <- (arbitrary :: Gen Expression)
                           s1 <- (arbitrary :: Gen Statement)
                           s2 <- (arbitrary :: Gen Statement)
                           return $ If g s1 s2
                   8 -> do g  <- (arbitrary :: Gen Expression)
                           s  <- (arbitrary :: Gen Statement)
                           return $ While g s
                   9 -> do s1 <- nonSeqGen
                           s2 <- (arbitrary :: Gen Statement)
                           return $ Sequence s1 s2
                   _ -> return Skip
  shrink s = case s of
    Skip           -> []
    Assign v e     -> map (Assign v) (shrink e)
    If g s1 s2     -> map (\s1' -> If g s1' s2) (shrink s1) ++ 
                      map (\s2' -> If g s1 s2') (shrink s2) ++
                      map (\g' -> If g' s1 s2) (shrink g) ++ 
                      shrink s1 ++ shrink s2 ++ [s1, s2]
    While g s      -> map (\s' -> While g s') (shrink s) ++
                      map (\g' -> While g' s) (shrink g) ++
                      (shrink s) ++ [s]
    Sequence s1 s2 -> map (\s1' -> Sequence s1' s2) (shrink s1) ++
                      map (\s2' -> Sequence s1 s2') (shrink s2) ++
                      (shrink s1) ++ (shrink s2) ++ [s1, s2]

main :: IO () 
main = do _ <- runTestTT (TestList [ t0, 
                                     t11, t11', t12, t13, 
                                     t2, t21 ])
          quickCheck qcRoundTripExp
          quickCheck qcRoundTripState
          return ()

-- Problem 0
---------------------------------------------

class PP a where
  pp :: a -> Doc

instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Divide = PP.char '/'
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="

instance PP Value where
  pp (IntVal a) = PP.text (show a)
  pp (BoolVal True) = PP.text "true"
  pp (BoolVal False) = PP.text "false"

-- I add parenthesis to all expressions but not a single variable
-- or a value
instance PP Expression where
  pp (Var var) = PP.text var
  pp (Val val) = pp val
  pp (Op bop exp1@(Op _ _ _) exp2@(Op _ _ _)) = 
     (PP.text "(") <> (pp exp1) <> (PP.text ")") <+> 
     (pp bop) <+> 
     (PP.text "(") <> (pp exp2) <> (PP.text ")")
  pp (Op bop exp1@(Op _ _ _) exp2) = 
     (PP.text "(") <> (pp exp1) <> (PP.text ")") <+> 
     (pp bop) <+> 
     (pp exp2)
  pp (Op bop exp1 exp2@(Op _ _ _)) = 
     (pp exp1) <+> 
     (pp bop) <+> 
     (PP.text "(") <> (pp exp2) <> (PP.text ")")
  pp (Op bop exp1 exp2) = (pp exp1) <+> (pp bop) <+> (pp exp2)

{- the following will make precedences work, but not working
   with test.imp file, so I commented it.
  pp  (Op bop exp1@(Op bop1 _ _) exp2@(Op bop2 _ _)) 
    | compare bop bop1 == GT && compare bop bop2 == GT =
     (PP.text "(") <> (pp exp1) <> (PP.text ")") <+> 
     (pp bop) <+> 
     (PP.text "(") <> (pp exp2) <> (PP.text ")") 
    | compare bop bop1 == GT = 
     (PP.text "(") <> (pp exp1) <> (PP.text ")") <+> 
     (pp bop) <+> 
     (pp exp2)
    | compare bop bop2 == GT = 
     (pp exp1) <+> 
     (pp bop) <+> 
     (PP.text "(") <> (pp exp2) <> (PP.text ")")
  pp (Op bop exp1@(Op bop1 _ _) exp2) 
    | compare bop bop1 == GT = 
     (PP.text "(") <> (pp exp1) <> (PP.text ")") <+> 
     (pp bop) <+> 
     (pp exp2)
    | compare bop bop1 == LT || compare bop bop1 == EQ =
     (pp exp1) <+> (pp bop) <+> (pp exp2)
  pp (Op bop exp1 exp2@(Op bop2 _ _))
    | compare bop bop2 == GT = 
     (pp exp1) <+> 
     (pp bop) <+> 
     (PP.text "(") <> (pp exp2) <> (PP.text ")")
    | compare bop bop2 == LT || compare bop bop2 == EQ =
     (pp exp1) <+> (pp bop) <+> (pp exp2)
  pp (Op bop exp1 exp2) = (pp exp1) <+> (pp bop) <+> (pp exp2)-}

instance PP Statement where
  pp (Assign var exp) = (pp (Var var)) <+> (PP.text ":=") <+> (pp exp)
  pp (If exp stmt1 stmt2) = (PP.text "if") <+> (pp exp) <+> (PP.text "then\n ") <+>
                            (pp stmt1) <> (PP.text "\nelse\n ") <+> (pp stmt2) <>
                            (PP.text "\nendif\n")
  pp (While exp stmt) = (PP.text "while") <+> (pp exp) <+> (PP.text "do\n ") <+>
                        (pp stmt) <> (PP.text "\nendwhile\n")
  pp (Sequence stmt1 stmt2) = (pp stmt1) <> (PP.text ";\n") <> (pp stmt2)
  pp (Skip) = (PP.text "skip")

display :: PP a => a -> String
display = show . pp

-- Simple tests 

oneV,twoV,threeV :: Expression
oneV   = Val (IntVal 1)
twoV   = Val (IntVal 2)
threeV = Val (IntVal 3)

t0 :: Test
t0 = TestList [display oneV ~?= "1",
      display (BoolVal True) ~?= "true",        
      display (Var "X") ~?= "X",
      display (Op Plus oneV twoV) ~?= "1 + 2",
      display (Op Plus oneV (Op Plus twoV threeV)) ~?= "1 + (2 + 3)", 
      display (Op Plus (Op Plus oneV twoV) threeV) ~?= "(1 + 2) + 3",
      display (Op Times (Op Plus oneV twoV) threeV) ~?= "(1 + 2) * 3",
      display (Op Divide (Op Plus oneV twoV) threeV) ~?= "(1 + 2) / 3",
      display (Op Ge (Op Plus oneV twoV) threeV) ~?= "(1 + 2) >= 3",
      display (Op Lt (Op Plus oneV twoV) (Op Times oneV twoV)) ~?= 
              "(1 + 2) < (1 * 2)",
      display (Assign "X" threeV) ~?= "X := 3",
      display Skip ~?= "skip", 
      display (Sequence Skip Skip) ~?= "skip;\nskip",
      t0b'',
      display (While (Op Lt (Var "x") (Val (IntVal 12))) Skip) ~?= 
              "while x < 12 do\n  skip\nendwhile\n" ]

--- Your own test cases

t0b :: Test
t0b  = display (If (Val (BoolVal True)) Skip Skip) ~?=
       "if true then skip else skip endif"

t0b' :: Test
t0b' = display (If (Val (BoolVal True)) Skip Skip) ~?=
      "if true then\n  skip\nelse  skip\nendif"

t0b'' :: Test
t0b'' = display (If (Val (BoolVal True)) Skip Skip) ~?=
      "if true then\n  skip\nelse\n  skip\nendif\n"


-- Problem 1
---------------------------------------------

valueP :: Parser Char Value
valueP = intP <|> boolP

intP :: Parser Char Value
intP = do val <- int
          return (IntVal val)

constP :: String -> a -> Parser Char a
constP s t = do xs <- string s
                return t

boolP :: Parser Char Value
boolP = do res <- constP "true" True <|> (constP "false" False)
           return (BoolVal res)

opP :: Parser Char Bop 
opP = ineqP <|> addP <|> mulP

opP' :: Parser Token Bop
opP' = ineqP' <|> addP' <|> mulP'

ineqP :: Parser Char Bop
ineqP = ge <|> gt <|> le <|> lt
    where gt = char '>'    >> return Gt
          ge = string ">=" >> return Ge
          lt = char '<'    >> return Lt
          le = string "<=" >> return Le

ineqP' :: Parser Token Bop
ineqP' = gt' <|> ge' <|> lt' <|> le'
    where gt' = bopP Gt >> return Gt
          ge' = bopP Ge >> return Ge
          lt' = bopP Lt >> return Lt
          le' = bopP Le >> return Le

addP :: Parser Char Bop
addP = plus <|> minus
 where plus  = char '+'    >> return Plus
       minus = char '-'    >> return Minus

addP' :: Parser Token Bop
addP' = plus' <|> minus'
  where plus'  = bopP Plus >> return Plus
        minus' = bopP Minus >> return Minus

mulP :: Parser Char Bop
mulP = times <|> divide
 where times  = char '*' >> return Times
       divide = char '/' >> return Divide
            
mulP' :: Parser Token Bop
mulP' = times' <|> divide'
  where times'  = bopP Times  >> return Times
        divide' = bopP Divide >> return Divide

varP :: Parser Char Variable
varP = many1 upper

wsP :: Parser Char a -> Parser Char a
wsP p = do xs <- p
           _  <- many space 
           return xs

---todo: left associativity finished
---todo: add generic solution for this, change Char to b, finished
charParsers :: [Parser Char Expression]
charParsers = [ineqExp, sumExp]

tokParsers :: [Parser Token Expression]
tokParsers = [ineqExp', sumExp']

exprP :: [Parser b Expression] -> Parser b Expression
exprP parsers = choice parsers

ineqExp :: Parser Char Expression
ineqExp = do l <- sumExp
             o <- wsP ineqP
             r <- exprP charParsers
             return (Op o l r)

ineqExp' :: Parser Token Expression
ineqExp' = do l <- sumExp'
              o <- ineqP'
              r <- exprP tokParsers
              return (Op o l r)

sumExp :: Parser Char Expression
sumExp = do l <- prodExp
            f <- addExp
            return (f l)

sumExp' :: Parser Token Expression
sumExp' = do l <- prodExp'
             f <- addExp'
             return (f l)

addExp :: Parser Char (Expression -> Expression)
addExp = (do o <- wsP addP
             r <- prodExp
             f <- addExp
             return (\l -> f (Op o l r)) ) <|> epsilonP

addExp' :: Parser Token (Expression -> Expression)
addExp' = (do o <- addP'
              r <- prodExp'
              f <- addExp'
              return (\l -> f (Op o l r)) ) <|> epsilonP

epsilonP :: Parser b (Expression -> Expression)
epsilonP = return id


prodExp :: Parser Char Expression
prodExp = do l <- factorExp
             f <- mulExp
             return (f l)

prodExp' :: Parser Token Expression
prodExp' = do l <- factorExp'
              f <- mulExp'
              return (f l)


mulExp :: Parser Char (Expression -> Expression)
mulExp = (do o <- wsP mulP
             r <- factorExp
             f <- mulExp
             return (\l -> f (Op o l r)) ) <|> epsilonP
        
mulExp' :: Parser Token (Expression -> Expression)
mulExp' = (do o <- mulP'
              r <- factorExp'
              f <- mulExp'
              return (\l -> f (Op o l r)) ) <|> epsilonP

factorExp :: Parser Char Expression
factorExp = parExp <|> valExp <|> varExp
  where valExp = do x <- wsP valueP
                    return (Val x)
        varExp = do y <- wsP varP
                    return (Var y)
        parExp = do exp <- between (wsP (char '(')) 
                                   (exprP charParsers) 
                                   (wsP (char ')'))
                    return exp

factorExp' :: Parser Token Expression
factorExp' = parExp' <|> valExp' <|> varExp'
       where valExp' = do (TokVal a)<- valP'  
                          return (Val a)
             varExp' = do (TokVar a)<- varP'
                          return (Var a)
             parExp' = do exp <-between (keyP "(") 
                                        (exprP tokParsers)
                                        (keyP ")")         
                          return exp
                           

t11 :: Test
t11 = TestList ["s1" ~: succeed (parse (exprP charParsers)"1 "),
                "s2" ~: succeed (parse (exprP charParsers) "1  + 2"),
                "s3" ~: succeed (parse (exprP charParsers) "Z+F+1-T*4"),
                "s4" ~: succeed (parse (exprP charParsers) "(Z+F)*(1-T)"),
                "s5" ~: succeed (parse (exprP charParsers) "X>4+R") ] where
  succeed (Left _)  = assert False
  succeed (Right _) = assert True


zT :: Token
zT = TokVar "Z"

fT :: Token
fT = TokVar "F"

tT :: Token
tT = TokVar "T"

oneT :: Token
oneT = TokVal (IntVal 1)

twoT :: Token
twoT = TokVal (IntVal 2)

lP :: Token
lP = Keyword "("

rP :: Token
rP = Keyword ")"

t11' :: Test
t11' = TestList [ "s1" ~: succeed (parse (exprP tokParsers) [oneT]),
                  "s2" ~: succeed (parse (exprP tokParsers) 
                                   [oneT, TokBop Plus, twoT]),
                  "s3" ~: succeed (parse (exprP tokParsers) 
                                   [zT, TokBop Plus, fT, TokBop Plus, oneT, 
                                    TokBop Minus, tT, TokBop Times, twoT]),
                  "s4" ~: succeed (parse (exprP tokParsers) 
                                   [lP, zT, TokBop Plus, fT, rP, TokBop Times, lP,
                                    oneT, TokBop Minus, tT, rP]),
                  "s5" ~: succeed (parse (exprP tokParsers) 
                                   [zT, TokBop Gt, oneT, TokBop Plus, fT]) 
                  ] where
  succeed (Left _)  = assert False
  succeed (Right _) = assert True


charParsers' :: [Parser Char Statement]
charParsers' = [seqP, singleP]

tokParsers' :: [Parser Token Statement]
tokParsers' = [seqP', singleP']

statementP :: [Parser b Statement] -> Parser b Statement
statementP l = choice l

assignP, ifP, whileP, seqP, skipP, singleP :: Parser Char Statement
assignP = do var <- wsP varP
             _   <- wsP (string ":=")
             exp <- wsP (exprP charParsers)
             return (Assign var exp)
ifP     = do _     <- wsP (string "if")
             exp   <- wsP (exprP charParsers)
             _     <- wsP (string "then")
             stmt1 <- statementP charParsers'
             _     <- wsP (string "else")
             stmt2 <- statementP charParsers'
             _     <- wsP (string "endif")
             return (If exp stmt1 stmt2)
whileP  = do _     <- wsP (string "while")
             exp   <- wsP (exprP charParsers)
             _     <- wsP (string "do")
             stmt  <- statementP charParsers'
             _     <- wsP (string "endwhile")
             return (While exp stmt)
skipP   = do _     <- wsP (string "skip")
             return Skip
singleP = skipP <|> assignP <|> ifP <|> whileP
seqP    = do stmt1 <- singleP
             _     <- wsP (string ";")
             stmt2 <- statementP charParsers'
             return (Sequence stmt1 stmt2)


assignP', ifP', whileP', seqP', skipP', singleP' :: Parser Token Statement
assignP' = do (TokVar var) <- varP'
              _   <- keyP ":="
              exp <- exprP tokParsers
              return (Assign var exp)
ifP'     = do _     <- keyP "if"
              exp   <- exprP tokParsers
              _     <- keyP "then"
              stmt1 <- statementP tokParsers'
              _     <- keyP "else"
              stmt2 <- statementP tokParsers'
              _     <- keyP "endif"
              return (If exp stmt1 stmt2)
whileP'  = do _     <- keyP "while"
              exp   <- exprP tokParsers
              _     <- keyP "do"
              stmt  <- statementP tokParsers'
              _     <- keyP "endwhile"
              return (While exp stmt)
skipP'   = do _     <- keyP "skip"
              return Skip
singleP' = do skipP' <|> assignP' <|> ifP' <|> whileP'
seqP'    = do stmt1 <- singleP'
              _     <- keyP ";"
              stmt2 <- statementP tokParsers'
              return (Sequence stmt1 stmt2)


t12 :: Test
t12 = TestList ["s1" ~: p "fact.imp",
                "s2" ~: p "test.imp", 
                "s3" ~: p "abs.imp" ,
                "s4" ~: p "times.imp" ] where
  p s = do { y <- parseFromFile (statementP charParsers') s ; succeed y }
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

testRT :: String -> Assertion
testRT filename = do 
   x <- parseFromFile (statementP charParsers') filename 
   case x of 
     Right ast -> case parse (statementP charParsers') (display ast) of
       Right ast' -> assert (ast == ast')
       Left _ -> assert False
     Left _ -> assert False                             

--quick check
qcRoundTripExp :: Expression -> Bool
qcRoundTripExp exp = case (parse (exprP charParsers) $ display exp) of
  Right exp' -> exp == exp'
  Left  _    -> False

qcRoundTripState :: Statement -> Bool
qcRoundTripState s = case (parse (statementP charParsers') $ display s) of
  Right s' -> s == s'
  Left  _  -> False

t13 :: Test
t13 = TestList ["s1" ~: testRT "fact.imp",
                "s2" ~: testRT "test.imp", 
                "s3" ~: testRT "abs.imp" ,
                "s4" ~: testRT "times.imp" ]

-- Problem 2
---------------------------------------------

data Token = 
     TokVar String       -- variables
   | TokVal Value        -- primitive values
   | TokBop Bop          -- binary operators
   | Keyword String    -- keywords        
      deriving (Eq, Show)

keywords :: [ Parser Char Token ]
keywords = map (\x -> constP x (Keyword x)) 
             [ "(", ")", ":=", ";", "if", "then", "else",
             "endif", "while", "do", "endwhile", "skip" ]

type Lexer = Parser Char [Token]

lexer :: Lexer 
lexer = sepBy1
        (liftM TokVal valueP <|>
         liftM TokVar varP   <|>
         liftM TokBop opP    <|>
         choice keywords)
        (many space)

t2 :: Test
t2 = parse lexer "X := 3" ~?= 
        Right [TokVar "X", Keyword ":=", TokVal (IntVal 3)]


varP', valP' :: Parser Token Token
varP' = satisfy id (\a -> case a of 
                             TokVar _ -> True
                             _        -> False)
valP' = satisfy id (\a -> case a of 
                             TokVal _ -> True
                             _        -> False)

bopP :: Bop -> Parser Token Token
bopP op = satisfy id (\a -> case a of
                                TokBop op' -> (op' == op)
                                _          -> False)

keyP :: String -> Parser Token Token
keyP str = satisfy id (\a -> case a of
                                 Keyword b -> (b == str)
                                 _         -> False)


-- here I use lexer to parse the file, then parse the result,
-- and at last do the same comparison as above
testRT' :: String -> Assertion
testRT' filename = do 
   x' <- parseFromFile lexer filename 
   case x' of 
     Right tokens -> 
           case (parse (statementP tokParsers') tokens) of
             Right ast -> case parse (statementP charParsers') (display ast) of
               Right ast' -> assert (ast == ast')
               Left _ -> assert False
             Left _ -> assert False                             
     Left _ -> assert False

t21 :: Test
t21 = TestList ["s1" ~: testRT' "fact.imp",
                "s2" ~: testRT' "test.imp", 
                "s3" ~: testRT' "abs.imp" ,
                "s4" ~: testRT' "times.imp" ]


