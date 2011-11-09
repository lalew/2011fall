{-# LANGUAGE StandaloneDeriving #-}

module While where

import Test.HUnit hiding (State)

import State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Parser hiding (get)
import ParserCombinators


-- Syntax of WHILE programming language

newtype Variable = V String deriving (Eq, Ord)

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Eq)

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v 
  | Op  Bop Expression Expression
  deriving (Eq)

data Bop = 
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool 
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Eq)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Eq)
           
-- Semantics of WHILE programming language           

type Store = Map Variable Value

evalE :: Expression -> State Store Value
evalE (Var x)      = do  
  m <- get 
  case (Map.lookup x m) of
    Just v ->  return v
    Nothing -> return (IntVal 0)
evalE (Val v)      = return v
evalE (Op o e1 e2) = liftM2 (evalB o) (evalE e1) (evalE e2) 

evalB :: Bop -> Value -> Value -> Value
evalB Plus   (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalB Minus  (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalB Times  (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalB Gt     (IntVal i1) (IntVal i2) = BoolVal (i1 > i2)
evalB Ge     (IntVal i1) (IntVal i2) = BoolVal (i1 >= i2)
evalB Lt     (IntVal i1) (IntVal i2) = BoolVal (i1 < i2)
evalB Le     (IntVal i1) (IntVal i2) = BoolVal (i1 <= i2)
evalB _ _ _ = IntVal 0

evalS :: Statement -> State Store ()
evalS w@(While e s)    = do 
  v <- evalE e
  case v of    
    BoolVal True  -> evalS (Sequence s w)
    BoolVal False -> return ()
    IntVal  _     -> return ()
evalS Skip             = return ()
evalS (Sequence s1 s2) = do evalS s1
                            evalS s2
                            
evalS (Assign x e)     = do 
    v <- evalE e
    m <- get
    put (Map.insert x v m)
evalS (If e s1 s2)      = do
    v <- evalE e 
    case v of 
      BoolVal True  -> evalS s1
      BoolVal False -> evalS s2
      IntVal _ -> return ()

execS :: Statement -> Store -> Store
execS =  execState . evalS  

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:" 
              putStrLn $ show $ execS stmt Map.empty

varX, varY, varZ, varF, varN :: Variable
varX = V "X"
varY = V "Y"
varF = V "F"
varZ = V "Z"
varN = V "N"

w_test :: Statement
w_test = (Sequence (Assign varX (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign varY (Val (IntVal 0))) (While (Op Gt (Var varX) (Val (IntVal 0))) (Sequence (Assign varY (Op Plus (Var varY) (Var varX))) (Assign varX (Op Minus (Var varX) (Val (IntVal 1))))))))

w_fact :: Statement
w_fact = (Sequence (Assign varN (Val (IntVal 2))) (Sequence (Assign varF (Val (IntVal 1))) (While (Op Gt (Var varN) (Val (IntVal 0))) (Sequence (Assign varX (Var varN)) (Sequence (Assign varZ (Var varF)) (Sequence (While (Op Gt (Var varX) (Val (IntVal 1))) (Sequence (Assign varF (Op Plus (Var varZ) (Var varF))) (Assign varX (Op Minus (Var varX) (Val (IntVal 1)))))) (Assign varN (Op Minus (Var varN) (Val (IntVal 1))))))))))

t4a :: Test 
t4a = execS w_test Map.empty ~?= 
        Map.fromList [(varX,IntVal 0),(varY,IntVal 10)]

t4b :: Test
t4b = execS w_fact Map.empty ~?=
        Map.fromList [(varF,IntVal 2),(varN,IntVal 0),(varX,IntVal 1),(varZ,IntVal 2)]

----------------------------

-- Pretty printing and parsing for WHILE programming language


class PP a where
  pp :: a -> Doc

instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="

instance PP Value where
  pp (IntVal i)  = PP.int i 
  pp (BoolVal b) = if b then PP.text "true" else PP.text "false"


instance PP Expression where
  pp (Var x) = pp x
  pp (Val x) = pp x
  pp e@(Op _ _ _) = ppPrec 0 e  where
     ppPrec n (Op bop e1 e2) =
        parens (level bop < n) $
           ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2 
     ppPrec _ e' = pp e'
     parens b = if b then PP.parens else id

-- use the C++ precendence level table
level :: Bop -> Int
level Plus   = 3
level Minus  = 3 
level Times  = 5
level _      = 8

instance PP Variable where
  pp (V x) = PP.text x

instance PP Statement where
  pp (Assign x e) = pp x <+> PP.text ":=" <+> pp e
  pp (If e s1 s2) = 
    PP.vcat [PP.text "if" <+> pp e <+> PP.text "then",
         PP.nest 2 (pp s1), 
         PP.text "else",
         PP.nest 2 (pp s2),
         PP.text "endif"]
  pp (While e s)  = 
     PP.vcat [PP.text "while" <+> pp e <+> PP.text "do",
              PP.nest 2 (pp s),
              PP.text "endwhile"]            
  pp (Sequence s1@(Sequence _ _) s2) = PP.parens (pp s1) <> PP.semi $$ pp s2     
  pp (Sequence s1 s2) = pp s1 <> PP.semi $$ pp s2
  pp Skip = PP.text "skip"


display :: PP a => a -> String
display = show . pp

-- Use pretty printer for showing


instance Show Variable where
  show = display
instance Show Value where
  show = display
instance Show Bop where  
  show = display
instance Show Expression where  
  show = display
instance Show Statement where
  show = display


{-
-- if you would like to see the AST instead of pretty printing, 
-- replace the above with the following code.
deriving instance Show Variable
deriving instance Show Value
deriving instance Show Bop
deriving instance Show Expression
deriving instance Show Statement
-}

--- Parsing

valueP :: Parser Value
valueP = intP <|> boolP

intP :: Parser Value

intP = liftM IntVal int

constP :: String -> a -> Parser a

constP s x = string s >> return x 

boolP :: Parser Value

boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)


opP :: Parser Bop 

opP =   constP "+" Plus 
    <|> constP "-" Minus
    <|> constP "*" Times
    <|> constP ">=" Ge
    <|> constP "<=" Le
    <|> constP ">" Gt
    <|> constP "<" Lt


varP :: Parser Variable
varP = liftM V $ many1 upper

wsP :: Parser a -> Parser a
wsP p = do x <- p 
           _ <- many space             
           return x


exprP :: Parser Expression
exprP = sumP where
   sumP    = prodP   `chainl1` opLevel (level Plus)
   prodP   = compP   `chainl1` opLevel (level Times)
   compP   = factorP `chainl1` opLevel (level Gt)
   factorP = between (wsP (char '(')) exprP (wsP (char ')')) <|> baseP
   baseP   = liftM Val (wsP valueP) <|> liftM Var (wsP varP)

opLevel :: Int -> Parser (Expression -> Expression -> Expression)
opLevel l = do x <- wsP opP 
               if level x == l then (return $ Op x) else fail ""

statementP :: Parser Statement
statementP = do s1 <- baseP 
                seqP s1 <|> return s1  where
  seqP s1  = do             
               (sstring ";") 
               s2 <- statementP 
               return (Sequence s1 s2)

  baseP   = assignP <|> ifP <|> whileP <|> skipP <|> factorP
  factorP = between (wsP (char '(')) statementP (wsP (char ')')) <|> baseP       
  
  ifP     = do  sstring "if"
                e <- exprP
                sstring "then"
                s1 <- statementP
                sstring "else"
                s2 <- statementP
                sstring "endif"
                return (If e s1 s2)
  whileP  = do  sstring "while"
                e <- exprP
                sstring "do"
                s <- statementP
                sstring "endwhile"
                return (While e s)
  skipP   = do  sstring "skip"
                return Skip                  
  assignP = do  x <- (wsP varP)
                sstring ":="
                e <- exprP
                _ <- many space
                return (Assign x e)

sstring :: String -> Parser ()
sstring s = (string s >> many space >> return ())

cmdP :: Parser (Either Expression Statement)
cmdP = (sstring "eval" >> liftM Left exprP) <|> liftM Right statementP

repl :: Store -> IO ()
repl store = do
  putStr "%> "
  line <- getLine
  case doParse cmdP line of 
    [ (Right stmt,_) ] -> do 
      putStr "\n"
      repl (execState (evalS stmt) store)
    [ (Left exp, []) ] -> do
      putStrLn $ display (evalState (evalE exp) store)
      repl store
    [] -> putStrLn "???" >> repl store
            
  