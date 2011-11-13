-- Advanced Programming, HW 6
-- Christian DeLozier <delozier>, Zi Yan <yanzi>

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Main where

import WhilePP

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Test.HUnit hiding (State)

type Store    = Map Variable Value

evalE :: (MonadState Store m, MonadError Value m, MonadWriter String m) 
         => Expression -> m Value
evalE (Var x) = do m <- get
                   case (Map.lookup x m) of
                     Just v ->  return v
                     Nothing -> throwError $ IntVal 0
evalE (Val v) = return v
evalE (Op o e1 e2) = do v1 <- evalE e1
                        v2 <- evalE e2
                        evalB o v1 v2

evalB ::  (MonadState Store m, MonadError Value m, MonadWriter String m) 
          => Bop -> Value -> Value -> m Value
evalB Plus   (IntVal i1) (IntVal i2) = return $ IntVal (i1 + i2)
evalB Minus  (IntVal i1) (IntVal i2) = return $ IntVal (i1 - i2)
evalB Times  (IntVal i1) (IntVal i2) = return $ IntVal (i1 * i2)
evalB Gt     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 > i2)
evalB Ge     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 >= i2)
evalB Lt     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 < i2)
evalB Le     (IntVal i1) (IntVal i2) = return $ BoolVal (i1 <= i2)
evalB Divide _ (IntVal 0)            = throwError $ IntVal 1
evalB Divide (IntVal i1) (IntVal i2) = return $ IntVal (i1 `div` i2)
evalB _      _          _            = throwError $ IntVal 2

evalS :: (MonadState Store m, MonadError Value m, MonadWriter String m) 
         => Statement -> m ()
evalS stmt = case stmt of 
                  Assign x e -> do v <- evalE e
                                   m <- get
                                   put (Map.insert x v m)
                  If e stmt1 stmt2 -> do v <- evalE e
                                         case v of 
                                           BoolVal True  -> 
                                             evalS stmt1
                                           BoolVal False -> 
                                             evalS stmt2
                                           IntVal _      -> 
                                             throwError $ IntVal 2 
                  w@(While e s) -> do v <- evalE e
                                      case v of
                                           BoolVal True  -> 
                                             evalS (Sequence s w)
                                           BoolVal False -> 
                                             return ()
                                           IntVal _      -> 
                                             throwError $ IntVal 2
                  Sequence stmt1 stmt2 -> do evalS stmt1
                                             evalS stmt2
                  Skip -> return ()
                  Print str e -> do v <- evalE e
                                    tell $ str ++ (case v of
                                                    IntVal i  -> show i
                                                    BoolVal b -> show b)
                  Throw e -> do v <- evalE e
                                throwError v
                  Try stmt1 v stmt2 -> catchError (evalS stmt1) 
                                                  (\x -> 
                                                    do m <- get
                                                       put (Map.insert v x m)
                                                       evalS stmt2)
                                           
 

instance Error Value

execute :: Store -> Statement -> (Store, Maybe Value, String)
execute st p = (cnt, result, lg)
  where ((res, lg), cnt) = runState (runWriterT (runErrorT (evalS p))) st
        result = case res of 
                      Left a  -> Just a
                      Right _ -> Nothing

raises :: Statement -> Value -> Test
s `raises` v = case (execute Map.empty s) of
    (_, Just v', _) -> v ~?= v'
    _  -> undefined

t1 :: Test
t1 = (Assign "X"  (Var "Y")) `raises` IntVal 0

t2 :: Test
t2 = (Assign "X" (Op Divide (Val (IntVal 1)) (Val (IntVal 0)))) 
     `raises` IntVal 1

t3 :: Test       
t3 = TestList [ Assign "X" (Op Plus (Val (IntVal 1)) (Val (BoolVal True))) 
                `raises` IntVal 2,
                If (Val (IntVal 1)) Skip Skip `raises` IntVal 2,
                While (Val (IntVal 1)) Skip `raises` IntVal 2]

mksequence :: [Statement] -> Statement
mksequence = foldr Sequence Skip

testprog1 :: Statement
testprog1 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Print "hello world: " $ Var "X",
                        If (Op Lt (Var "X") (Var "Y")) 
                        (Throw (Op Plus (Var "X") (Var "Y")))
                        Skip,
                        Assign "Z" $ Val $ IntVal 3]

t4 :: Test
t4 = execute Map.empty testprog1 ~?=
  (Map.fromList [("X", IntVal 0), ("Y",  IntVal 1)], Just (IntVal 1), 
   "hello world: 0")

testprog2 :: Statement
testprog2 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Try (If (Op Lt (Var "X") (Var "Y"))
                                (mksequence [Assign "A" $ Val $ IntVal 100,
                                             Throw (Op Plus 
                                                    (Var "X") (Var "Y")),
                                             Assign "B" $ Val $ IntVal 200])
                                Skip)
                            "E"
                            (Assign "Z" $ Op Plus (Var "E") (Var "A"))]

t5 :: Test
t5 = execute Map.empty testprog2 ~?=
   ( Map.fromList [("A", IntVal 100), ("E", IntVal 1)
          ,("X", IntVal 0), ("Y", IntVal 1)
          ,("Z", IntVal 101)]
          , Nothing 
   , "")
   
testprog3 :: Statement
testprog3 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Try (Assign "X" $ Val $ IntVal 1)
                        "E"
                        (Assign "Y" $ Val $ IntVal 2) ]
            
t6 :: Test
t6 = execute Map.empty testprog3 ~?=
  ( Map.fromList [("X",IntVal 1)],Nothing,"")
  
testprog4 :: Statement
testprog4 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Print "Starting execution: " $ (Var "X"),
                        If (Op Divide (Var "X") (Var "X")) 
                        (Assign "X" $ Val $ IntVal 1)
                        (Assign "X" $ Val $ IntVal 2)]
            
t7 :: Test
t7 = execute Map.empty testprog4 ~?=
  ( Map.fromList [("X",IntVal 0)],Just (IntVal 1), "Starting execution: 0")

testprog5 :: Statement
testprog5 = mksequence[Try (mksequence [Assign "X" $ Val $ BoolVal True,
                                        If (Var "X") 
                                        (Assign "X" $ Op Plus 
                                         (Var "X") (Var "Y"))
                                        (Assign "X" $ Val $ IntVal 1)])
                       "E"
                       (If (Op Le (Var "E") (Val (IntVal 1))) 
                        (Assign "X" $ Val $ BoolVal False)
                        (Throw (Var "X")))]
            
t8 :: Test
t8 = execute Map.empty testprog5 ~?=
  ( Map.fromList [("E",IntVal 0),("X",BoolVal False)],Nothing,"")
  
t9 :: Test
t9 = execute (Map.insert "Y" (IntVal 5) Map.empty) testprog5 ~?=
  ( Map.fromList [("E",IntVal 2),("X",BoolVal True),("Y",IntVal 5)],
    Just (BoolVal True),"" )

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ t1, t2, t3, t4, t5, t6, t7, t8, t9 ]
   return ()

