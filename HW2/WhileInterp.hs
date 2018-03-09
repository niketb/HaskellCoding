{-
  Name: Niket Bhodia
  Class: CS 252
  Assigment: HW2
  Date: 09/28/2017
  Description: Implements the While language as per the small-step semantics defined in the accompanying LaTeX and PDF documents
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Unop(..),
  Value(..),
  testProgram,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe


-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | Op1 Unop Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  | And      -- && :: Bool -> Bool -> Bool
  | Or       -- || :: Bool -> Bool -> Bool 
  deriving (Show)

data Unop =  -- This is for unary operators, such as not
    Not      -- not :: Bool -> Bool
  deriving (Show)  

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.
applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal 0) = error "Sorry! Humanity hasn't yet developed the math to handle divide-by-zero operations."
applyOp Divide (IntVal i) (IntVal j) = IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = BoolVal $ i <= j
applyOp And (BoolVal i) (BoolVal j) = BoolVal $ i && j 
applyOp Or (BoolVal i) (BoolVal j) = BoolVal $ i || j

-- This function is used for defining unary operations
applyUnOp :: Unop -> Value -> Value
applyUnOp Not (BoolVal i) = BoolVal $ not i 

-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)                                     -- Corresponding small-step semantic rules:
evaluate (Val v) s = (v, s)                                                           -- SS-Val?
evaluate (Var x) s =                                                                  -- SS-Var
  case (Map.lookup x s) of 
    Just v -> (v, s)
    Nothing -> error "Uh-oh, you're looking up a variable not currently in the Store."  
evaluate (If cond eTrue eFalse) s 
  | v == (BoolVal True) = evaluate eTrue s                                            -- SS-IfTrueRed
  | v == (BoolVal False) = evaluate eFalse s                                          -- SS-IfFalseRed
  | otherwise = error "Invalid operation: a number cannot be used as a condition"
  where (v, s') = evaluate cond s                                                     -- SS-IfCtxt
evaluate (Op o e1 e2) s =
  let (v1, s1) = evaluate e1 s                                                        -- SS-BinCtxt1
      (v2, s') = evaluate e2 s1                                                       -- SS-BinCtxt2
  in (applyOp o v1 v2, s')                                                            -- SS-BinRed
evaluate (Op1 o e) s =                                                                 
  let (v, s') = evaluate e s                                                           -- SS-UnCtxt
  in (applyUnOp o v, s')                                                              -- SS-UnRed  
evaluate (Assign x e) s =                                                             
  let (v, s') = evaluate e s                                                          -- SS-AssignCtxt
  in (v, Map.insert x v s')                                                           -- SS-AssignRed                                   
evaluate (Sequence e1 e2) s = 
  let (v1, s1) = evaluate e1 s                                                        -- SS-SeqCtxt
  in evaluate e2 s1                                                                   -- SS-SeqRed   
evaluate (While e1 e2) s =                                                            -- SS-While
  let (v1, s1) = evaluate e1 s
  in if v1 == (BoolVal True)
     then (evaluate (Sequence e2 (While e1 e2)) s)  
     else (BoolVal False, s)

-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog