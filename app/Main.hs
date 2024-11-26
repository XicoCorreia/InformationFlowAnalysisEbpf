{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.Environment as Sys
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.List (intercalate)

import Data.Text.Display

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()

data Trans =
    NonCF Instruction -- no jumps, or exit
  | Unconditional
  | Assert Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int
type LabeledProgram = [(Int, Instruction)]
type CFG = Set (Label, Trans, Label)

label :: Program -> LabeledProgram
label = zip [0..]

r0 :: Reg
r0 = Reg 0

neg :: Jcmp -> Jcmp
neg cmp =
  case cmp of
    Jeq -> Jne ; Jne -> Jeq
    Jgt -> Jle; Jge -> Jlt; Jlt -> Jge; Jle -> Jgt
    Jsgt -> Jsle; Jsge -> Jslt; Jslt -> Jsge; Jsle -> Jsgt
    Jset -> error "Don't know how to negate JSET"

cfg :: Program -> CFG
cfg prog = Set.unions $ map transfer $ label prog
  where
    transfer (i, instr) =
      case instr of
        JCond cmp r ir off ->
          Set.singleton (i, Assert cmp r ir, i+1+fromIntegral off)
          `Set.union`
          Set.singleton (i, Assert (neg cmp) r ir, i+1)
        Jmp off ->
          Set.singleton (i, Unconditional, i+1+fromIntegral off)
        Exit ->
          Set.empty
        _ ->
          Set.singleton (i, NonCF instr, i+1)

----------------- The following is for creating the equations ----------------------

-------------- The following is to create the equations from CFG -------------------
-- Registers use type Reg from asm

data Exp =
    Register Reg
  | Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp -- Not implemented for Intervals
  | Div Exp Exp -- Not implemented for Intervals
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
  | SKIP -- Used for undefined stmts
    deriving (Show)

type Equations = Map.Map Int [(Int, Stmt)]

addElementToList :: Int -> Int -> Stmt -> Equations -> Equations
addElementToList key prev stmt eqs =
  let currentList = Map.findWithDefault [] key eqs
      newList = (prev, stmt) : currentList
  in Map.insert key newList eqs

cfgToEquations :: CFG -> Equations -> Equations
cfgToEquations c eq = foldr edgeToEquation eq (Set.toList c)

edgeToEquation :: (Label, Trans, Label) -> Equations -> Equations
edgeToEquation (from, (NonCF i), to) = addElementToList to from (opToStmt i)
edgeToEquation (from, (Assert cmp r ir), to) =  addElementToList to from (If (assertToTest cmp r ir) to) 
edgeToEquation (from, Unconditional, to) = addElementToList to from (Goto to)

assertToTest :: Jcmp -> Reg -> RegImm -> Test
assertToTest cmp r ri =
    case cmp of
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
        Jset -> undefined 
  where
    exp1 = Register r
    exp2 = case ri of
      R (Reg x) -> Register (Reg x)
      Imm n -> Const $ fromIntegral n

opToStmt :: Instruction -> Stmt
opToStmt (Binary _ op r ri)  =
  case op of
    Ebpf.Asm.Add -> AssignReg r (Main.Add exp1 exp2)
    Ebpf.Asm.Sub -> AssignReg r (Main.Sub exp1 exp2)
    Ebpf.Asm.Mul -> AssignReg r (Main.Mul exp1 exp2)
    Ebpf.Asm.Div -> AssignReg r (Main.Div exp1 exp2)
    Ebpf.Asm.Mov -> AssignReg r exp2
    _ -> SKIP -- ! undefined operations
    where
      exp1 = Register r
      exp2 = case ri of
        R (Reg x) -> Register (Reg x)
        Imm n -> Const $ fromIntegral n
opToStmt (Store _ r _ ri)  = AssignMem r exp2
    where
      exp2 = case ri of
        R (Reg x) -> Register (Reg x)
        Imm n -> Const $ fromIntegral n
opToStmt _ = SKIP -- ! undefined operations

--------------- The following is to cycle through the Equations--------------------
data SecurityLevel = High | Low deriving (Eq, Show)

type State = [(Reg, SecurityLevel)]

-- Mem[r1] := rj || const
type Memory = [(Label, Reg, SecurityLevel)]

type SystemState = ([State], Context, Memory)


type Context = [Int]

initialState :: State
initialState = [
    (Reg 0, Low), (Reg 1, High), (Reg 2, Low), 
    (Reg 3, Low), (Reg 4, Low), (Reg 5, Low), 
    (Reg 6, Low), (Reg 7, Low), (Reg 8, Low), 
    (Reg 9, Low), (Reg 10, Low)]

unionStt :: State -> State -> State
unionStt [] [] = []
unionStt [] a = a
unionStt a [] = a
unionStt ((reg1, sec1):s1r) ((_, sec2):s2r) =
  if (sec1 == High) || (sec2 == High)
    then (reg1, High) : unionStt s1r s2r
  else
    (reg1, Low) : unionStt s1r s2r

-- Implement Cycle Eq

informationFlowAnalysis :: Equations -> [State]
informationFlowAnalysis e =
  informationFlowAnalysisHelper (Map.toList e) (replicate ((length e) + 1) initialState, [], [])

informationFlowAnalysisHelper :: [(Int, [(Int, Stmt)])] -> SystemState -> [State]
informationFlowAnalysisHelper eq ss = do
  let (state', context', memory') = iterateEquations eq ss
  if ss == (state', context', memory') 
    then state'      -- Return the final state
    else informationFlowAnalysisHelper eq (state', context', memory')



iterateEquations ::  [(Int, [(Int, Stmt)])] -> SystemState -> SystemState
iterateEquations [] ss = ss
iterateEquations (v:vs) ss = 
    iterateEquations vs (processElement v ss)

processElement :: (Int, [(Int, Stmt)]) -> SystemState -> SystemState
processElement (eqIdx, ops) (states, context, mem) = 
    (take eqIdx states ++ [states'] ++ drop (eqIdx + 1) states, context', mem')
  where
    startState = states !! eqIdx
    (states', context', mem') = processElementHelper startState (eqIdx,ops) (states, context, mem)
    

processElementHelper :: State -> (Int, [(Int, Stmt)]) -> SystemState -> (State, Context, Memory)
processElementHelper startState (_,[]) (_, c, m) = (startState, c, m)
processElementHelper startState (currentNode, ((prevNode,sttm):es)) (states, context, mem) =
  let
    -- takes the starting state 
    prevState = states !! prevNode
    -- uses the expression to create a new state starting from the list of states
    (state', context', memory') = updateUsingStmt prevState context mem (prevNode, currentNode) sttm 
    -- does the union between the original and the newly created (if one is high is high, otherwise low)
    newState = unionStt startState state'
    -- recurs on all elements
    otherState = processElementHelper newState (prevNode, es) (states, context', memory')
    -- i can return the result of the last recursion
  in otherState

-- updateUsingStmt propagates security levels based on the Stmt type
updateUsingStmt :: State -> Context -> Memory -> (Int,Int) -> Stmt -> (State, Context, Memory)
updateUsingStmt s c mem (prevNode, currentNode) (AssignReg r e) = (updatedState, c, mem)
  where 
    secLevel = updateUsingExp s c (prevNode, currentNode)  e
    updatedState = updateRegisterSecurity r secLevel s
updateUsingStmt s c mem (prevNode, currentNode)  (AssignMem r e) = (s, c, updatedMemory)
  where
    secLevel = updateUsingExp s c (prevNode, currentNode)  e
    updatedMemory = updateMemorySecurity prevNode r secLevel mem
updateUsingStmt s c mem _ (Goto _) = (s, c, mem)  
updateUsingStmt s c mem (prevNode, currentNode)  (If test lbl) =  
  let 
    (exp1, exp2) = extractExps test
    secLevelExp1 = updateUsingExp s c (prevNode, currentNode)  exp1
    secLevelExp2 = updateUsingExp s c (prevNode, currentNode)  exp2
  in
    if secLevelExp1 == Low && secLevelExp2 == Low then (s, c, mem) else (s, c', mem) 
  where 
    c' = if lbl `elem` c then c else c ++ [lbl]
    extractExps :: Test -> (Exp, Exp)
    extractExps (EQUAL e1 e2)      = (e1, e2)
    extractExps (NOTEQUAL e1 e2)   = (e1, e2)
    extractExps (LESSTHAN e1 e2)   = (e1, e2)
    extractExps (LESSEQUAL e1 e2)  = (e1, e2)
    extractExps (GREATTHAN e1 e2)  = (e1, e2)
    extractExps (GREATEQUAL e1 e2) = (e1, e2)
updateUsingStmt s c mem _ SKIP = (s, c, mem)  -- ! undefined operations do not affect

updateUsingExp :: State -> Context -> (Int,Int) -> Exp -> SecurityLevel
updateUsingExp s c (prevNode, currentNode)  e = 
  case e of
    Register r -> 
      case lookup r s of
        Just secLevel ->
          case secLevel of
            High -> High
            Low -> if elem prevNode c || elem currentNode c then High else Low
        Nothing -> if elem prevNode c || elem currentNode c then High else Low
    Const _ -> if elem prevNode c || elem currentNode c then High else Low
    Main.Add e1 e2 -> 
      let 
        sec1 = updateUsingExp s c (prevNode, currentNode)  e1
        sec2 = updateUsingExp s c (prevNode, currentNode)  e2
        resultSecLevel = if sec1 == High || sec2 == High || elem prevNode c || elem currentNode c then High else Low
      in 
        resultSecLevel
    Main.Sub e1 e2 ->  
      let 
        sec1 = updateUsingExp s c (prevNode, currentNode)  e1
        sec2 = updateUsingExp s c (prevNode, currentNode)  e2
        resultSecLevel = if sec1 == High || sec2 == High || elem prevNode c || elem currentNode c then High else Low
      in 
        resultSecLevel
    Main.Mul e1 e2 ->  
      let 
        sec1 = updateUsingExp s c (prevNode, currentNode)  e1
        sec2 = updateUsingExp s c (prevNode, currentNode)  e2
        resultSecLevel = if sec1 == High || sec2 == High || elem prevNode c || elem currentNode c then High else Low
      in 
        resultSecLevel 
    Main.Div e1 e2 ->  
      let 
        sec1 = updateUsingExp s c (prevNode, currentNode)  e1
        sec2 = updateUsingExp s c (prevNode, currentNode)  e2
        resultSecLevel = if sec1 == High || sec2 == High || elem prevNode c || elem currentNode c then High else Low
      in 
        resultSecLevel 

updateRegisterSecurity :: Reg -> SecurityLevel -> State -> State
updateRegisterSecurity r secLevel = map (\(regist, sec) -> 
    if regist == r 
        then (regist, if sec == High then sec else secLevel)  -- Keep high security if already high
        else (regist, sec))

updateMemorySecurity :: Int -> Reg -> SecurityLevel -> Memory -> Memory
updateMemorySecurity prevNode r secLevel [] = [(prevNode, r, secLevel)] 
updateMemorySecurity prevNode ri secLevel ((idxj, rj, secLevelj) : rest) = 
  if prevNode == idxj &&  ri == rj 
    then  (prevNode, ri, if secLevelj == High then secLevelj else secLevel) : rest 
    else  (idxj, rj, secLevelj) : updateMemorySecurity prevNode ri secLevel rest 



------------------- The following is just for visualisation ------------------------

cfgToDot :: CFG -> String
cfgToDot graph = Set.toList graph >>= showTrans
  where
    showTrans (x, NonCF i, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (display i)
    showTrans (x, Unconditional, y) = printf "  %d -> %d [label=\"jmp\"];\n" x y
    showTrans (x, Assert c r ir, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (showJump c r ir)
    showJump c r ir = display c <> " " <> display r <> ", " <> display ir

dotPrelude :: String
dotPrelude =
  "digraph cfg { \n"++
  "node [fontname=\"monospace\"];\n"++
  "node [shape=box];\n"++
  "edge [fontname=\"monospace\"];\n"

markNodes :: Program -> String
markNodes prog = concat $ mapMaybe mark $ label prog
  where
    mark (lab, Exit) = return $ printf "%d [style=\"rounded,filled\",fillcolor=grey];\n" lab
    mark (lab, JCond _ _ _ _) = return $ printf "%d [shape=diamond];\n" lab
    mark _ = Nothing

formatMap :: Equations -> String
formatMap m = intercalate "\n" $ map formatEntry (Map.toList m)
  where
    formatEntry (key, valueList) = show key ++ " -> " ++ show valueList

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [ebpfFile, dotFile] -> do
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occurred while parsing:"
          print err
        Right prog -> do
          printf "The eBPF file %s has %d instructions\n" ebpfFile (length prog)
          let edges = cfgToDot $ cfg prog
          writeFile dotFile (dotPrelude ++
                             edges ++
                             markNodes prog ++ "}")
          printf "Visualised the CFG in %s\n" dotFile
    [ebpfFile] -> do
      putStrLn "Run Information Flow Analysis\n"
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occurred while parsing:"
          print err
        Right prog -> do
          let a = cfgToEquations (cfg prog) (Map.empty)
          putStrLn "Equations:"
          putStrLn $ formatMap a
          putStrLn "\nStates:"
          let st = informationFlowAnalysis a     
          mapM_ printWithIndex (zip ([0..] :: [Int]) st) 
            where
              printWithIndex (index, lst) = putStrLn (show index ++ ": " ++ show lst)
    _ -> 
      putStrLn "Usage <EBPF_FILE>\n"
