module Language.Carol.TypeCheck.SubCheck where

import Language.Carol.AST
import Language.Carol.TypeCheck.Context
import Language.Carol.TypeCheck.Inst

subCheckV :: ValT -> ValT -> Context -> TErr Context
subCheckV vt1 vt2 g = case (vt1,vt2) of
  -- InstantiateL
  (ExVT a, vt2) -> instLV a vt2 g
  -- InstantiateR+InstRSolve
  (vt1, ExVT a) -> bindExV a vt1 g
  -- Unit, etc.
  _ | vt1 == vt2 -> return g
  _ -> Left $ show vt1 ++ " does not match " ++ show vt2

subCheckC :: CompT -> CompT -> Context -> TErr Context
subCheckC mt1 mt2 g = case (mt1,mt2) of
  -- Exvar
  (ExCT b1,ExCT b2) | b1 == b2 -> return g
  -- { InstantiateL, InstantiateR+InstRSolve, Unit, etc. }
  (RetT vt1, RetT vt2) -> subCheckV vt1 vt2 g
  -- InstantiateR (for all InstR rules except InstRSolve)
  (mt,ExCT b) -> instRC mt b g
