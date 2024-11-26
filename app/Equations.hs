module Equations (cfgToEquations) where

import Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Ebpf.Asm


-- Add a statement to an equation list
addElementToList :: Int -> Int -> Stmt -> Equations -> Equations
addElementToList key prev stmt eqs =
  let currentList = Map.findWithDefault [] key eqs
      newList = (prev, stmt) : currentList
  in Map.insert key newList eqs

-- Translate CFG edges into equations
cfgToEquations :: CFG -> Equations -> Equations
cfgToEquations c eq = foldr edgeToEquation eq (Set.toList c)

edgeToEquation :: (Label, Trans, Label) -> Equations -> Equations
edgeToEquation (from, NonCF i, to) = addElementToList to from (opToStmt i)
edgeToEquation (from, Assert cmp r ir, to) = addElementToList to from (If (assertToTest cmp r ir) to)
edgeToEquation (from, Unconditional, to) = addElementToList to from (Goto to)

-- Helpers to translate instructions and conditions
assertToTest :: Jcmp -> Reg -> RegImm -> Test
assertToTest cmp r ri = case cmp of
  Jeq -> EQUAL exp1 exp2
  Jne -> NOTEQUAL exp1 exp2
  Jgt -> GREATTHAN exp1 exp2
  Jge -> GREATEQUAL exp1 exp2
  Jlt -> LESSTHAN exp1 exp2
  Jle -> LESSEQUAL exp1 exp2
  Jsgt -> GREATTHAN exp1 exp2
  Jsge -> GREATEQUAL exp1 exp2
  Jslt -> LESSTHAN exp1 exp2
  Jsle -> LESSEQUAL exp1 exp2
  Jset -> error "JSET not supported"
  where
    exp1 = Register r
    exp2 = case ri of
      R reg -> Register reg
      Imm n -> Const (fromIntegral n)

opToStmt :: Instruction -> Stmt
opToStmt (Binary _ op r ri) = AssignReg r $ case op of
  Add -> AddOp exp1 exp2
  Sub -> SubOp exp1 exp2
  Mul -> MulOp exp1 exp2
  Div -> DivOp exp1 exp2
  Mov -> exp2
  _   -> error "Unsupported binary operation"
  where
    exp1 = Register r
    exp2 = case ri of
      R reg -> Register reg
      Imm n -> Const (fromIntegral n)
opToStmt (Store _ r _ ri) = AssignMem r exp2
  where
    exp2 = case ri of
      R reg -> Register reg
      Imm n -> Const (fromIntegral n)
opToStmt _ = SKIP
