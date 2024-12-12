module Main where

import Types
import Equations
import Analysis
import Cfg

import qualified System.Environment as Sys
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Graph.Dom as Dom
import Text.Printf

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()

------------------- Environment for the analysis ------------------------

initialState :: State
initialState = [
    (Reg 0, Low), (Reg 1, High), (Reg 2, Low), 
    (Reg 3, Low), (Reg 4, Low), (Reg 5, Low), 
    (Reg 6, Low), (Reg 7, Low), (Reg 8, Low), 
    (Reg 9, Low), (Reg 10, Low)]


------------------- Perform analysis/generate PDF with cfg ------------------------
main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [ebpfFile, dotFile] -> do
      printf $ "\nRun Information Flow Analysis on " ++ ebpfFile ++ "\n"
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occurred while parsing:"
          print err
        Right prog -> 
          let 
            cfg' = cfg prog
            equations = cfgToEquations cfg' (Map.empty)
            edgesList = [(from, to) | (from, _, to) <- Set.toList cfg']
            graphDom = ((length equations), Dom.fromEdges edgesList) 
            (states, memory, context) = informationFlowAnalysis graphDom equations initialState 
            flatContext = concatMap (\(x, xs) -> x : xs) (Set.toList context)
            edges = cfgToDot $ cfg prog
          in do
          printf "\nEquations:\n"
          putStrLn $ formatMap equations
          printf "\nFinal states:\n"  
          mapM_  (\(index,lst) -> putStrLn (show index ++ ": " ++ show lst)) 
            (zip ([0..] :: [Int]) states) 
          printf "\nMemory: "
          putStrLn $ show memory
          writeFile dotFile (dotPrelude ++
                    edges ++
                    (markHighContextNodes prog flatContext) ++ "}")
          printf "\nVisualised the CFG in %s\n" dotFile
          printf $ "\n-----------------------Analysis completed-----------------------\n"
    _ -> do
      putStrLn "Usage:"
      putStrLn "- Run information flow analysis and visualize cfg:\n <EBPF_FILE> <DOT_FILE>"


