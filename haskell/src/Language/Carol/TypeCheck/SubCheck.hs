module Language.Carol.TypeCheck.SubCheck where

import Data.SBV
import Control.Monad.IO.Class

import Language.Carol.AST.Refinement
import Language.Carol.AST.Types
import Language.Carol.TypeCheck.Context
import Language.Carol.TypeCheck.Inst
import Language.Carol.TypeCheck.Error

subCheckV :: (RefDomain d)
  => ValT d
  -> ValT d
  -> Context d
  -> TErr d (Context d)
subCheckV vt1 vt2 g = case (vt1,vt2) of
  -- InstantiateL
  (ExVT a, vt2) -> instLV a vt2 g
  -- InstantiateR+InstRSolve
  (vt1, ExVT a) -> bindExV a vt1 g
  -- Unit, etc.
  _ | vt1 == vt2 -> return g
  (DsT t1 r1, DsT t2 r2) | t1 == t2 -> do
    result <- liftIO . prove $ do
      nu <- forall "nu"
      return $ rpred r2 nu .=> rpred r1 nu
    liftIO $ print result
    case result of
      ThmResult (Unsatisfiable _ _) -> return g
      ThmResult (Satisfiable _ _) -> 
        terr $ TMismatch vt1 vt2
    -- terr $ TOther "Ref comparison not implemented."
  _ -> terr $ TMismatch vt1 vt2

subCheckC :: (RefDomain d)
  => CompT d
  -> CompT d
  -> Context d
  -> TErr d (Context d)
subCheckC mt1 mt2 g = case (mt1,mt2) of
  -- Exvar
  (ExCT b1,ExCT b2) | b1 == b2 -> return g
  -- { InstantiateL, InstantiateR+InstRSolve, Unit, etc. }
  (RetT vt1, RetT vt2) -> subCheckV vt1 vt2 g
  -- InstantiateR (for all InstR rules except InstRSolve)
  (mt,ExCT b) -> instRC mt b g
