
-- Advanced Programming, HW 4
-- by <YOUR NAME HERE> <pennkey>, <YOUR PARTNER'S NAME> <pennkey>

import Control.Monad

import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Parser
import ParserCombinators

import Test.HUnit

type Variable = String
 
data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)
 
data Expression =
    Var Variable
  | Val Value  
  | Op  Bop Expression Expression
  deriving (Show, Eq)
 
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

data Statement =
    Assign Variable Expression          
  | If Expression Statement Statement
  | While Expression Statement       
  | Sequence Statement Statement        
  | Skip
  deriving (Show, Eq)

main :: IO () 
main = do _ <- runTestTT (TestList [ t0, 
                                     t11, t12, t13, 
                                     t2 ])
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
  pp _ = error "TBD"

instance PP Expression where
  pp _ = error "TBD"

instance PP Statement where
  pp _ = error "TBD"

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
      display (Op Plus (Op Plus oneV twoV) threeV) ~?= "1 + 2 + 3",
      display (Assign "X" threeV) ~?= "X := 3",
      display Skip ~?= "skip"  ]

--- Your own test cases

t0b :: Test
t0b  = display (If (Val (BoolVal True)) Skip Skip) ~?=
       "if true then skip else skip endif"

t0b' :: Test
t0b' = display (If (Val (BoolVal True)) Skip Skip) ~?=
      "if true then\n  skip\nelse  skip\nendif"



-- Problem 1
---------------------------------------------

valueP :: Parser Value
valueP = intP <|> boolP

intP :: Parser Value
intP = error "TBD"

constP :: String -> a -> Parser a
constP _ _ = error "TBD"

boolP :: Parser Value
boolP = error "TBD"

opP :: Parser Bop 
opP = error "TBD"

varP :: Parser Variable
varP = many1 upper

wsP :: Parser a -> Parser a
wsP p = error "TBD"

exprP :: Parser Expression
exprP = error "TBD"

t11 :: Test
t11 = TestList ["s1" ~: succeed (parse exprP "1 "),
                "s2" ~: succeed (parse exprP "1  + 2") ] where
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

statementP :: Parser Statement
statementP = error "TBD"

t12 :: Test
t12 = TestList ["s1" ~: p "fact.imp",
                "s2" ~: p "test.imp", 
                "s3" ~: p "abs.imp" ,
                "s4" ~: p "times.imp" ] where
  p s = do { y <- parseFromFile statementP s ; succeed y }
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

testRT :: String -> Assertion
testRT filename = do 
   x <- parseFromFile statementP filename 
   case x of 
     Right ast -> case parse statementP (display ast) of
       Right ast' -> assert (ast == ast')
       Left _ -> assert False
     Left _ -> assert False                             

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

keywords :: [ Parser Token ]
keywords = map (\x -> constP x (Keyword x)) 
             [ "(", ")", ":=", ";", "if", "then", "else",
             "endif", "while", "do", "endwhile", "skip" ]

type Lexer = Parser [Token]

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



