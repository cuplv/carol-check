module Language.Carol.TypeCheck.SubCheck where

import Data.SBV
import Control.Monad.IO.Class

import Language.Carol.AST.Refinement
import Language.Carol.AST.Types
import Language.Carol.TypeCheck.Context
import Language.Carol.TypeCheck.Inst
import Language.Carol.TypeCheck.Error
import Language.Carol.TypeCheck.Solve

-- | Check that the first value type a subtype of the second value
-- type.
subCheckV :: (RefSolve d)
  => ValTR d
  -> ValTR d
  -> Context d
  -> TErr d (Context d)
subCheckV vt1 vt2 g = case (vt1,vt2) of
  -- InstantiateL
  (ExVT a, vt2) -> instLV a vt2 g
  -- InstantiateR+InstRSolve
  (vt1, ExVT a) -> bindExV a vt1 g
  -- Unit, etc.
  (ValTR t1 r1, ValTR t2 r2) -> subRef (t1,r1) (t2,r2) g
  -- _ | vt1 == vt2 -> return g
  -- (PairT xt1 yt1, PairT xt2 yt2) -> do
  --   g1 <- subCheckV xt1 xt2 g
  --   g2 <- subCheckV yt1 yt2 g1
  --   return g2
  -- (DsT t1 r1, DsT t2 r2) | t1 == t2 -> do
  --   result <- liftIO . isTheorem $ do
  --     nu <- forall "nu"
  --     return $ rpred r1 nu .=> rpred r2 nu
  --   if result
  --      then return g
  --      else terr $ TMismatch vt1 vt2
  -- _ -> terr $ TMismatch vt1 vt2

-- | Check that the first computation type a subtype of the second
-- computation type.
subCheckC :: (RefSolve d)
  => CompT d
  -> CompT d
  -> Context d
  -> TErr d (Context d)
subCheckC mt1 mt2 g = case (mt1,mt2) of
  -- Exvar
  (ExCT b1,ExCT b2) | b1 == b2 -> return g
  -- { InstantiateL, InstantiateR+InstRSolve, Unit, etc. }
  (RetT vt1, RetT vt2) -> subCheckV vt1 vt2 g
  -- { <:--> }
  (FunT xt1 rt1, FunT xt2 rt2) -> do
    g1 <- subCheckV xt1 xt2 g
    rt1' <- substC g1 rt1
    rt2' <- substC g1 rt2
    subCheckC rt1' rt2' g1
  -- InstantiateR (for all InstR rules except InstRSolve)
  (mt,ExCT b) -> instRC mt b g
