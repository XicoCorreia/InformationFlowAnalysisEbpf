{-# LANGUAGE OverloadedStrings #-}
module Cfg (cfg, cfgToDot, dotPrelude, markHighContextNodes) where

import Types


import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.Text.Display
import Text.Printf

import Ebpf.Asm
import Ebpf.Display ()

label :: Program -> LabeledProgram
label = zip [0..]


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

markHighContextNodes :: Program -> [Int] -> String
markHighContextNodes prog redNodes = concat $ mapMaybe mark $ label prog
  where
    mark (lab, Exit)
      | lab `elem` redNodes = return $ printf "%d [style=\"rounded,filled\",fillcolor=firebrick2];\n" lab
      | otherwise           = return $ printf "%d [style=\"rounded,filled\",fillcolor=grey];\n" lab
    mark (lab, JCond _ _ _ _)
      | lab `elem` redNodes = return $ printf "%d [shape=diamond,style=filled,fillcolor=firebrick4];\n" lab
      | otherwise           = return $ printf "%d [shape=diamond];\n" lab
    mark (lab, _) 
      | lab `elem` redNodes = return $ printf "%d [style=filled,fillcolor=firebrick1];\n" lab
      | otherwise           = Nothing