module Types where

import Data.Set (Set)
import Data.Map (Map)
import Ebpf.Asm

-- CFG Types
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
  | MulOp Exp Exp  
  | DivOp Exp Exp  
    deriving (Show)

data Cond = 
      Equal Exp Exp
    | NotEqual Exp Exp
    | LessThan Exp Exp
    | LessEqual Exp Exp
    | GreaterThan Exp Exp
    | GreaterEqual Exp Exp
    deriving (Show)

data Stmt =
    AssignReg Reg Exp
  | AssignMem Reg Exp
  | If Cond Label
  | Goto Label
  | SKIP -- ! For Undefined statements
    deriving (Show)

type Equations = Map Label [(Label, Stmt)]

-- Security Types
data SecurityLevel = High | Low 
  deriving (Eq, Show)

type State = [(Reg, SecurityLevel)]
type Memory = [(Label, Reg, SecurityLevel)]
type Context = [Int]
type SystemState = ([State], Context, Memory)