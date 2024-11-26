module Types where

import Data.Set (Set)
import Data.Map (Map)
import Ebpf.Asm

-- Shared Types
data Trans =
    NonCF Instruction  -- No jumps, or exit
  | Unconditional
  | Assert Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int
type LabeledProgram = [(Int, Instruction)]
type CFG = Set (Label, Trans, Label)

-- Equation Types
data Exp =
    Register Reg
  | Const Int
  | AddOp Exp Exp
  | SubOp Exp Exp
  | MulOp Exp Exp  -- Not implemented for Intervals
  | DivOp Exp Exp  -- Not implemented for Intervals
    deriving (Show)

data Test =
    EQUAL Exp Exp
  | NOTEQUAL Exp Exp
  | LESSTHAN Exp Exp
  | LESSEQUAL Exp Exp
  | GREATTHAN Exp Exp
  | GREATEQUAL Exp Exp
    deriving (Show)

data Stmt =
    AssignReg Reg Exp
  | AssignMem Reg Exp
  | If Test Label
  | Goto Label
  | SKIP
    deriving (Show)

type Equations = Map Int [(Int, Stmt)]

-- Security Types
data SecurityLevel = High | Low deriving (Eq, Show)

type State = [(Reg, SecurityLevel)]
type Memory = [(Label, Reg, SecurityLevel)]
type Context = [Int]
type SystemState = ([State], Context, Memory)