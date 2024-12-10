{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Equations
import Data.List (find)
import Analysis

import Data.Graph.Dom as Dom
import Data.Graph as G

import qualified System.Environment as Sys
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Printf
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

import Data.Text.Display

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()

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

printWithIndex :: (Int, State) -> IO ()
printWithIndex (index, lst) = putStrLn (show index ++ ": " ++ show lst)


------------------- Environment for the analysis ------------------------

initialState :: State
initialState = [
    (Reg 0, Low), (Reg 1, High), (Reg 2, Low), 
    (Reg 3, Low), (Reg 4, Low), (Reg 5, Low), 
    (Reg 6, Low), (Reg 7, Low), (Reg 8, Low), 
    (Reg 9, Low), (Reg 10, Low)]

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    -- Create CFG in dotFile
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
      printf "Run Information Flow Analysis\n"
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occurred while parsing:"
          print err
        Right prog -> do
          let edges = cfg prog
          let equations = cfgToEquations edges (Map.empty)
          printf "\nEquations:\n"
          putStrLn $ formatMap equations
          let (states, _, x) = informationFlowAnalysis edges equations initialState 
          printf "\nFinal states:\n"  
          mapM_ printWithIndex (zip ([0..] :: [Int]) states)
          let lastNode = length equations
          let edgesList = [(from, to) | (from, _, to) <- Set.toList edges]
          let graphDom = (lastNode, Dom.fromEdges edgesList)
          let graphG = G.buildG (0, lastNode) edgesList
          let a = pdom graphDom
          case find (\(n, _) -> n == 4) a of
            Nothing -> do printf "\nFinal states:\n"
            Just (_,postDoms) -> do
              printf $ show (2 `elem` (G.reachable graphG 4)) 
              printf $ show $ not (2 `elem` postDoms)
              printf $ (show x) ++ "\n"
              printf $ show a
 
    _ -> do
      putStrLn "Usage:"
      putStrLn "- Create CFG in dot file:\n <EBPF_FILE> <DOT_FILE>"
      putStrLn "- Run information flow analysis:\n <EBPF_FILE>"