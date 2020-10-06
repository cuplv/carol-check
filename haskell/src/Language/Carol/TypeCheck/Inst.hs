module Language.Carol.TypeCheck.Inst 
  ( instLV
  , instRC
  ) where

import Language.Carol.AST
import Language.Carol.TypeCheck.Context

instLV :: ExIdV -> ValT -> Context -> TErr Context
instLV a vt g = case vt of
  -- InstLReach
  ExVT a1 | a < a1 -> bindExV a1 (ExVT a) g
  -- InstLSolve
  vt -> bindExV a vt g

instRC :: CompT -> ExIdC -> Context -> TErr Context
instRC mt b g = case mt of
  -- InstRReach
  ExCT b1 | b < b1 -> bindExC b1 (ExCT b) g
  -- InstRArr
  FunT vt mt' -> do
    (aNew,bNew,g1) <- inb42 b g
    g2 <- instLV aNew vt g1
    mt'' <- substC g2 mt'
    instRC mt'' bNew g2
  mt -> bindExC b mt g
