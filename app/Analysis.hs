module Analysis (informationFlowAnalysis) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find)


import Data.Graph.Dom as Dom

import Types
import Ebpf.Asm


-- Perform a the information flow  analysis on a set of equations.
informationFlowAnalysis :: CFG -> Equations -> State -> SystemState
informationFlowAnalysis cfg eq initialState =
  fixpointComputation graphDom (replicate ((length eq) + 1) initialState, Low, Set.empty) (Map.toList eq)
      where 
      lastNode = length eq
      edgesList = [(from, to) | (from, _, to) <- Set.toList cfg]
      graphDom = (lastNode, Dom.fromEdges edgesList)


-- Perform fixpoint computation for the analysis.
fixpointComputation :: Dom.Rooted -> SystemState -> [(Label, [(Label, Stmt)])] -> SystemState 
fixpointComputation graph ss eq = 
  if ss == ss'
    then ss'
    else fixpointComputation graph ss' eq
      where 
        ss' = foldl (updateSystemState graph) ss eq

-- This function updates the System state with a new state for the node being processed.
updateSystemState :: Dom.Rooted -> SystemState -> (Label, [(Label, Stmt)]) -> SystemState
updateSystemState graph (states, mem, jumps) (nodeIdx, eqs) = 
  (before ++ [state'] ++ after, mem', jumps')
  where
    startState = states !! nodeIdx
    (state', mem', jumps') = processElement graph startState (states, mem, jumps) (nodeIdx, eqs)
    before = take nodeIdx states 
    after = drop (nodeIdx + 1) states
    
-- Processes the equations for a specific node, returning the updated state.     
processElement :: Dom.Rooted -> State -> SystemState -> (Label, [(Label, Stmt)]) -> (State, Memory, HighSecurityJumps)
processElement _ state (_, m, j) (_,[]) = (state, m, j)
processElement graph state (states, mem, jumps) (currentNode, ((prevNode, stmt):es)) = otherState
  where 
    dependsOnJump = isDependent (prevNode, currentNode) (Set.toList jumps)
    prevState = (states !! prevNode)
    (state',  mem', jumps') = updateUsingStmt graph prevState mem jumps dependsOnJump (prevNode, currentNode) stmt 
    newState = unionStt state state'
    otherState = processElement graph newState (states,  mem', jumps') (currentNode, es)

-- Update a node's state by analysing the security level of an equation.
updateUsingStmt :: Dom.Rooted -> State -> Memory -> HighSecurityJumps -> Bool -> (Int,Int) -> Stmt -> (State, Memory, HighSecurityJumps)
updateUsingStmt _ state mem jumps dependsOnJump (prevNode, currentNode) (AssignReg r e) = 
  case lookup r state of 
    Nothing -> error ("Not defined register: " ++ show r)
    _ -> (updatedState, mem, jumps)
  where 
    secLevel = 
      if dependsOnJump 
        then High 
        else processExpression state (prevNode, currentNode) e
    updatedState = updateRegisterSecurity r secLevel state
updateUsingStmt _ state mem jumps dependsOnJump (prevNode, currentNode) (AssignMem r e) = 
  case lookup r state of 
    Nothing -> error ("Not defined register: " ++ show r)
    _ -> (state, mem', jumps)
  where
    secLevel = 
      if dependsOnJump 
        then High 
        else processExpression state (prevNode, currentNode) e
    mem' = if mem == High then High else secLevel
updateUsingStmt graph state mem jumps dependsOnJump (prevNode, currentNode) (If cond _) =  
  if secLevelCond == High || dependsOnJump 
    then 
      case find (\(n,_) -> n == prevNode) (ipdom graph) of
        Just (_, nodePD) -> let
            context = Set.fromList (concat $ highContextNodes prevNode nodePD graph Set.empty)
            context' = Set.delete prevNode context
            in
            (state, mem, Set.insert (prevNode, Set.toList context') jumps)
        Nothing -> (state, mem, jumps)
    else (state, mem, jumps)
  where
    (e1, e2) = extractExpsFromCond cond
    secLevelExp1 = processExpression state (prevNode, currentNode)  e1
    secLevelExp2 = processExpression state (prevNode, currentNode)  e2
    secLevelCond = if secLevelExp1 == Low && secLevelExp2 == Low then Low else High
updateUsingStmt _ state mem jumps _ _ (Goto _) = (state, mem, jumps)  
updateUsingStmt _ state mem jumps _ _ SKIP = (state, mem, jumps)

-- Process an expression, returning the security level of the expression.
processExpression :: State -> (Int,Int) -> Exp -> SecurityLevel
processExpression state (prevNode, currentNode) e = 
  case e of 
    Register r -> 
      case lookup r state of
            Just secLevel -> secLevel
            Nothing -> error ("Not defined register: " ++ show r)
    Const _ -> Low
    -- In the case it is a binOp both expressions are checked
    AddOp e1 e2 -> processBinOp state (prevNode, currentNode)  e1 e2
    SubOp e1 e2 -> processBinOp state (prevNode, currentNode)  e1 e2
    MulOp e1 e2 -> processBinOp state (prevNode, currentNode)  e1 e2
    DivOp e1 e2 -> processBinOp state (prevNode, currentNode)  e1 e2
    ModOp e1 e2 -> processBinOp state (prevNode, currentNode)  e1 e2
    AndOp e1 e2 -> processBinOp state (prevNode, currentNode)  e1 e2
    OrOp e1 e2 -> processBinOp state (prevNode, currentNode)  e1 e2


-- Processes a binary operation, returning the security level.
processBinOp :: State -> (Int,Int) -> Exp -> Exp -> SecurityLevel
processBinOp state (prevNode, currentNode) e1 e2 = 
  let sec1 = processExpression state (prevNode, currentNode)  e1
      sec2 = processExpression state (prevNode, currentNode)  e2
      resultSecLevel = if sec1 == High || sec2 == High then High else Low
      in resultSecLevel

-- Update the register security in a state.
updateRegisterSecurity :: Reg -> SecurityLevel -> State -> State
updateRegisterSecurity r secLevel = map (\(reg, sec) -> 
    if reg == r 
        then (reg, secLevel)
        else (reg, sec))

-- Extract the two expressions used in a Condition.
extractExpsFromCond :: Cond -> (Exp, Exp)
extractExpsFromCond (Equal e1 e2)      = (e1, e2)
extractExpsFromCond (NotEqual e1 e2)   = (e1, e2)
extractExpsFromCond (LessThan e1 e2)   = (e1, e2)
extractExpsFromCond (LessEqual e1 e2)  = (e1, e2)
extractExpsFromCond (GreaterThan e1 e2)  = (e1, e2)
extractExpsFromCond (GreaterEqual e1 e2) = (e1, e2)

-- Union of two states.
unionStt :: State -> State -> State
unionStt = zipWith combine
  where
    combine (reg, sec1) (_, sec2) = (reg, if sec1 == High || sec2 == High then High else Low)

-- Check wheter a node is reachable from a conditional jump and if it is a post dominant node.
-- If it is reachable and does not post dominates one of the nodes containing a conditional jump,
-- it is considered dependent, meaning it relies on a secret condition.
isDependent :: (Label, Label) -> [(Int, [Int])] -> Bool
isDependent _ [] = False
isDependent (prevNode, currentNode) ((cond,dependents):xs) = 
  if prevNode `elem` dependents || ((prevNode == cond) && currentNode `elem` dependents)
    then True 
    else isDependent (prevNode, currentNode) xs


-- Helper function with a set of visited nodes
highContextNodes :: Label -> Label -> Dom.Rooted -> Set.Set Label -> [[Label]]
highContextNodes start end (n,cfg) visited
    | start == end = [[]] -- Base case: Path ends when start equals end
    | start `Set.member` visited = [[]] -- Node already visited, avoid loops
    | otherwise = [if neighbor == end then [] else neighbor : path | neighbor <- neighbors, path <- highContextNodes neighbor end (n,cfg) (Set.insert start visited)]
        where neighbors = graphSucc start (toEdges cfg)


-- Get successors (neighbors) of a node
graphSucc :: Label -> [Edge] -> [Label]
graphSucc node cfg = [to | (from, to) <- cfg, from == node]


--TODO Fix case where edge inside -> pdom, pdom is not in / and node -> inside 