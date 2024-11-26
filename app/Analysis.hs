module Analysis (informationFlowAnalysis) where

import Types
import qualified Data.Map as Map
import Ebpf.Asm

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
  if sec1 == High || sec2 == High
    then (reg1, High) : unionStt s1r s2r
    else (reg1, Low) : unionStt s1r s2r

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
    AddOp e1 e2 -> 
      let 
        sec1 = updateUsingExp s c (prevNode, currentNode)  e1
        sec2 = updateUsingExp s c (prevNode, currentNode)  e2
        resultSecLevel = if sec1 == High || sec2 == High || elem prevNode c || elem currentNode c then High else Low
      in 
        resultSecLevel
    SubOp e1 e2 ->  
      let 
        sec1 = updateUsingExp s c (prevNode, currentNode)  e1
        sec2 = updateUsingExp s c (prevNode, currentNode)  e2
        resultSecLevel = if sec1 == High || sec2 == High || elem prevNode c || elem currentNode c then High else Low
      in 
        resultSecLevel
    MulOp e1 e2 ->  
      let 
        sec1 = updateUsingExp s c (prevNode, currentNode)  e1
        sec2 = updateUsingExp s c (prevNode, currentNode)  e2
        resultSecLevel = if sec1 == High || sec2 == High || elem prevNode c || elem currentNode c then High else Low
      in 
        resultSecLevel 
    DivOp e1 e2 ->  
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

