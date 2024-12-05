module Equations (cfgToEquations) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Types
import Ebpf.Asm

-- Converts the edges of a Control Flow Graph (CFG) into a set of equations.
-- Each edge in the CFG is mapped to an equation representing the flow of control.
cfgToEquations :: CFG -> Equations -> Equations
cfgToEquations cfg eq = foldr edgeToEquation eq (Set.toList cfg)

-- Converts a single edge from the CFG into an equation.
edgeToEquation :: (Label, Trans, Label) -> Equations -> Equations
edgeToEquation (from, NonCF i, to) = addEquation from to (opToStmt i)
edgeToEquation (from, Assert cmp r ir, to) = addEquation from to (If (assertToCond cmp r ir) to)
edgeToEquation (from, Unconditional, to) = addEquation from to (Goto to)

-- Converts a conditional jump (Jcmp) into an appropriate condition (Cond).
assertToCond :: Jcmp -> Reg -> RegImm -> Cond
assertToCond cmp r ri = case cmp of
  Jeq -> Equal exp1 exp2
  Jne -> NotEqual exp1 exp2
  Jgt -> GreaterThan exp1 exp2
  Jge -> GreaterEqual exp1 exp2
  Jlt -> LessThan exp1 exp2
  Jle -> LessEqual exp1 exp2
  Jsgt -> GreaterThan exp1 exp2
  Jsge -> GreaterEqual exp1 exp2
  Jslt -> LessThan exp1 exp2
  Jsle -> LessEqual exp1 exp2
  Jset -> Equal exp1 exp2
  where
    exp1 = Register r
    exp2 = regOrConstant ri

-- Converts a given instruction (e.g., binary operation, store) into an equivalent statement (Stmt).
opToStmt :: Instruction -> Stmt
opToStmt (Binary _ op r ri) = AssignReg r $ case op of
  Add -> AddOp exp1 exp2
  Sub -> SubOp exp1 exp2
  Mul -> MulOp exp1 exp2
  Div -> DivOp exp1 exp2
  Mod -> ModOp exp1 exp2
  And -> AndOp exp1 exp2
  Or -> OrOp exp1 exp2
  Mov -> exp2
  _   -> error "Unsupported binary operation"
  where
    exp1 = Register r
    exp2 = regOrConstant ri
opToStmt (Store _ r _ ri) = AssignMem r (regOrConstant ri)
opToStmt _ = SKIP


-- Adds a new equation (statement) to the equation list for the given node.
addEquation :: Label -> Label -> Stmt -> Equations -> Equations
addEquation prev node stmt eqs =
  let currentList = Map.findWithDefault [] node eqs
      newList = (prev, stmt) : currentList
  in Map.insert node newList eqs

-- Converts a RegImm value into either a Register or a Constant expression.
regOrConstant :: RegImm -> Exp
regOrConstant r = case r of
  R reg -> Register reg
  Imm n -> Const (fromIntegral n)
