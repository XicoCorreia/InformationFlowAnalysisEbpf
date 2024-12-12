module Analysis (informationFlowAnalysis) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find)

import Data.Graph.Dom as Dom

import Types
import Ebpf.Asm

-- Perform a the information flow analysis on a set of equations.
informationFlowAnalysis :: Dom.Rooted -> Equations -> State -> SystemState
informationFlowAnalysis graph eq initialState =
  fixpointComputation graph (replicate ((length eq) + 1) initialState, Low, Set.empty) (Map.toList eq)

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
processElement :: Dom.Rooted -> State -> SystemState -> (Label, [(Label, Stmt)]) -> (State, Memory, HighSecurityContext)
processElement _ state (_, m, j) (_,[]) = (state, m, j)
processElement graph state (states, mem, jumps) (currentNode, ((prevNode, stmt):es)) = otherState
  where 
    dependsOnJump = isDependent (prevNode, currentNode) (Set.toList jumps)
    prevState = (states !! prevNode)
    (state',  mem', jumps') = updateUsingStmt graph prevState mem jumps dependsOnJump (prevNode, currentNode) stmt 
    newState = unionStt state state'
    otherState = processElement graph newState (states,  mem', jumps') (currentNode, es)

-- Update a node's state by analysing the security level of an equation, it also updates the context if the equation 
-- is a conditional jump, i.e. if cond.
updateUsingStmt :: Dom.Rooted -> State -> Memory -> HighSecurityContext -> Bool -> (Int,Int) -> Stmt -> (State, Memory, HighSecurityContext)
updateUsingStmt _ state mem jumps dependsOnJump _ (AssignReg r e) = 
  case lookup r state of 
    Nothing -> error ("Register: " ++ show r ++ " is not allowed to be used")
    _ -> (updatedState, mem, jumps)
  where 
    secLevel = 
      if dependsOnJump 
        then High 
        else processExpression state e
    updatedState = updateRegisterSecurity r secLevel state
updateUsingStmt _ state mem jumps dependsOnJump _ (AssignMem r e) = 
  case lookup r state of 
    Nothing -> error ("Register: " ++ show r ++ " is not allowed to be used")
    _ -> (state, mem', jumps)
  where
    secLevel = 
      if dependsOnJump 
        then High 
        else processExpression state e
    mem' = if mem == High then High else secLevel
updateUsingStmt graph state mem jumps dependsOnJump (prevNode, _) (If cond _) =  
  if secLevelCond == High || dependsOnJump 
    then 
      case find (\(n,_) -> n == prevNode) (ipdom graph) of
        Just (_, nodePD) -> 
          (state, mem, Set.insert (prevNode, (highContextNodes prevNode nodePD graph)) jumps)
        Nothing -> (state, mem, jumps)
    else (state, mem, jumps)
  where
    (e1, e2) = extractExpsFromCond cond
    secLevelExp1 = processExpression state e1
    secLevelExp2 = processExpression state e2
    secLevelCond = if secLevelExp1 == Low && secLevelExp2 == Low then Low else High
updateUsingStmt _ state mem jumps _ _ (Goto _) = (state, mem, jumps)  
updateUsingStmt _ state mem jumps _ _ SKIP = (state, mem, jumps)

-- Process an expression, returning the security level of the expression.
processExpression :: State -> Exp -> SecurityLevel
processExpression state  e = 
  case e of 
    Register r -> 
      case lookup r state of
            Just secLevel -> secLevel
            Nothing -> error ("Not defined register: " ++ show r)
    Const _ -> Low
    AddOp e1 e2 -> processBinOp state e1 e2
    SubOp e1 e2 -> processBinOp state e1 e2
    MulOp e1 e2 -> processBinOp state e1 e2
    DivOp e1 e2 -> processBinOp state e1 e2
    ModOp e1 e2 -> processBinOp state e1 e2
    AndOp e1 e2 -> processBinOp state e1 e2
    OrOp e1 e2  -> processBinOp state e1 e2


-- Processes a binary operation by processing both expressions, returning the higher security level of both.
processBinOp :: State -> Exp -> Exp -> SecurityLevel
processBinOp state e1 e2 = 
  let sec1 = processExpression state e1
      sec2 = processExpression state e2
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

isDependent :: (Label, Label) -> [(Int, [Int])] -> Bool
isDependent _ [] = False
isDependent (prevNode, currentNode) ((cond,dependents):xs) = 
  if prevNode `elem` dependents || ((prevNode == cond) && currentNode `elem` dependents)
    then True 
    else isDependent (prevNode, currentNode) xs

-- Returns the nodes that belong to the high security context starting in the node
-- containing the jump and ending in the immediate post dominator of that node.
highContextNodes :: Label -> Label -> Dom.Rooted -> [Label]
highContextNodes node nodePD graph = Set.toList $ Set.delete node res
  where res = Set.fromList (concat $ highContextNodes' node nodePD graph Set.empty)

-- Helper function that performs the logic to calculate the nodes inside the high security context
highContextNodes' :: Label -> Label -> Dom.Rooted -> Set.Set Label -> [[Label]]
highContextNodes' start end (root,cfg) visited
    | start == end = [[]] -- Base case: Path ends when start equals end
    | start `Set.member` visited = [[]] -- Node already visited, avoid loops
    | otherwise = [if neighbor == end then [] else neighbor : path | 
      neighbor <- neighbors, path <- highContextNodes' neighbor end (root,cfg) (Set.insert start visited)]
        where neighbors = graphSucc start (toEdges cfg)

-- Get successors (neighbors) of a node
graphSucc :: Label -> [Edge] -> [Label]
graphSucc node cfg = [to | (from, to) <- cfg, from == node]
