module Analysis (informationFlowAnalysis) where

import qualified Data.Map as Map

import Types
import Ebpf.Asm

-- Perform a the information flow  analysis on a set of equations.
informationFlowAnalysis :: Equations -> State -> [State]
informationFlowAnalysis e initialState =
  fixpointComputation (replicate ((length e) + 1) initialState, [], Low) (Map.toList e)

-- Perform fixpoint computation for the analysis.
fixpointComputation :: SystemState -> [(Label, [(Label, Stmt)])] -> [State]
fixpointComputation ss eq = 
  if ss == (state', context', memory') 
    then state'
    else fixpointComputation (state', context', memory') eq
      where 
        (state', context', memory') = foldl updateSystemState ss eq

-- This function updates the System state with a new state for the node being processed.
updateSystemState ::  SystemState -> (Label, [(Label, Stmt)]) -> SystemState
updateSystemState (states, context, mem) (nodeIdx, eqs) = 
  (before ++ [state'] ++ after , context', mem')
  where
    startState = states !! nodeIdx
    (state', context', mem') = processElement startState (states, context, mem) (nodeIdx, eqs)
    before = take nodeIdx states 
    after = drop (nodeIdx + 1) states
    
-- Processes the equations for a specific node, returning the updated state.     
processElement :: State -> SystemState -> (Label, [(Label, Stmt)]) -> (State, Context, Memory)
processElement state (_, c, m) (_,[]) = (state, c, m)
processElement state (states, context, mem) (currentNode, ((prevNode,stmt):es)) = otherState
  where 
    (state', context', mem') = updateUsingStmt (states !! prevNode) context mem (prevNode, currentNode) stmt 
    newState = unionStt state state'
    otherState = processElement newState (states, context', mem') (currentNode, es)

------------------- TODO ------------------------
updateUsingStmt :: State -> Context -> Memory -> (Int,Int) -> Stmt -> (State, Context, Memory)
updateUsingStmt s c mem (prevNode, currentNode) (AssignReg r e) = (updatedState, c, mem)
  where 
    secLevel = processExpression s c (prevNode, currentNode)  e
    updatedState = updateRegisterSecurity r secLevel s
updateUsingStmt s c mem (prevNode, currentNode)  (AssignMem _ e) = (s, c, updatedMemory)
  where
    secLevel = processExpression s c (prevNode, currentNode)  e
    updatedMemory = if secLevel == High then High else mem
updateUsingStmt s c mem _ (Goto _) = (s, c, mem)  
updateUsingStmt s c mem (prevNode, currentNode)  (If test lbl) =  
  let 
    (exp1, exp2) = extractExps test
    secLevelExp1 = processExpression s c (prevNode, currentNode)  exp1
    secLevelExp2 = processExpression s c (prevNode, currentNode)  exp2
  in
    if secLevelExp1 == Low && secLevelExp2 == Low then (s, c, mem) else (s, c', mem) 
  where 
    c' = if lbl `elem` c then c else c ++ [lbl]
    extractExps :: Cond -> (Exp, Exp)
    extractExps (Equal e1 e2)      = (e1, e2)
    extractExps (NotEqual e1 e2)   = (e1, e2)
    extractExps (LessThan e1 e2)   = (e1, e2)
    extractExps (LessEqual e1 e2)  = (e1, e2)
    extractExps (GreaterThan e1 e2)  = (e1, e2)
    extractExps (GreaterEqual e1 e2) = (e1, e2)
updateUsingStmt s c mem _ SKIP = (s, c, mem)  -- ! undefined operations do not affect
------------------- TODO ------------------------


-- Process an expression, returning the security level of the expression.
processExpression :: State -> Context -> (Int,Int) -> Exp -> SecurityLevel
processExpression s c (prevNode, currentNode) e = 
  case e of 
    Register r -> 
      case lookup r s of
            Just secLevel ->
              case secLevel of
                High -> High
                Low -> if elem prevNode c || elem currentNode c then High else Low
            Nothing -> error "Not defined register"
    Const _ -> 
      if elem prevNode c || elem currentNode c 
        then High 
        else Low
    AddOp e1 e2 -> processBinOp s c (prevNode, currentNode)  e1 e2
    SubOp e1 e2 -> processBinOp s c (prevNode, currentNode)  e1 e2
    MulOp e1 e2 -> processBinOp s c (prevNode, currentNode)  e1 e2
    DivOp e1 e2 -> processBinOp s c (prevNode, currentNode)  e1 e2

-- Processes a binary operation, returning the security level.
processBinOp :: State -> Context -> (Int,Int) -> Exp -> Exp -> SecurityLevel
processBinOp s c (prevNode, currentNode) e1 e2 = 
  let sec1 = processExpression s c (prevNode, currentNode)  e1
      sec2 = processExpression s c (prevNode, currentNode)  e2
      resultSecLevel = if sec1 == High || sec2 == High || elem prevNode c || elem currentNode c then High else Low
      in resultSecLevel

-- Update the register security in a state.
updateRegisterSecurity :: Reg -> SecurityLevel -> State -> State
updateRegisterSecurity r secLevel = map (\(reg, sec) -> 
    if reg == r 
        then (reg, if sec == High then sec else secLevel)
        else (reg, sec))

-- Union of two states.
unionStt :: State -> State -> State
unionStt = zipWith combine
  where
    combine (reg, sec1) (_, sec2) = (reg, if sec1 == High || sec2 == High then High else Low)