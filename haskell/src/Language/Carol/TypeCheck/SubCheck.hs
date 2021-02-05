module Language.Carol.TypeCheck.SubCheck where

import Data.SBV
import Control.Monad.IO.Class
import Control.Monad.State
import Lens.Micro.Platform

import Language.Carol.AST.Refinement
import Language.Carol.AST.Types
import Language.Carol.TypeCheck.Context
import qualified Language.Carol.TypeCheck.Context.Base as CB
import Language.Carol.TypeCheck.Inst
import Language.Carol.TypeCheck.Error

-- | Check that the first value type is a subtype of the second value
-- type.
subCheckV :: (RefDomain d)
  => ValT d
  -> ValT d
  -> StateT (Context d) (TErr d) ()
subCheckV vt1 vt2 = case (vt1,vt2) of
  -- InstantiateL
  (ExVT a, vt2) -> zoom base $ instLV a vt2
  -- InstantiateR+InstRSolve
  (vt1, ExVT a) -> base %>= CB.bindExV a vt1
  -- Unit, etc.
  _ | vt1 == vt2 -> return ()
  (PairT xt1 yt1, PairT xt2 yt2) -> do
    subCheckV xt1 xt2
    subCheckV yt1 yt2
  (DsT t1 r1, DsT t2 r2) | t1 == t2 -> do
    g <- use base
    result <- lift . liftIO . isTheorem $ do
      nu <- forall "nu"
      m <- CB.quantifyContext g
      return $ rpred m r1 nu .=> rpred m r2 nu
    if result
       then return ()
       else lift.terr $ TMismatch vt1 vt2
  _ -> lift.terr $ TMismatch vt1 vt2

-- | Check that the first computation type is a subtype of the second
-- computation type.
subCheckC :: (RefDomain d)
  => CompT d
  -> CompT d
  -> StateT (Context d) (TErr d) ()
subCheckC mt1 mt2 = case (mt1,mt2) of
  -- Exvar
  (ExCT b1,ExCT b2) | b1 == b2 -> return ()
  -- Pi elim
  (mt1,Idx a s mt2') -> do
    base %= CB.idxBind a s
    subCheckC mt1 mt2'
  -- { InstantiateL, InstantiateR+InstRSolve, Unit, etc. }
  (RetT vt1, RetT vt2) -> subCheckV vt1 vt2
  -- { <:--> }
  (FunT xt1 rt1, FunT xt2 rt2) -> do
    subCheckV xt1 xt2
    g <- use base
    rt1' <- lift $ CB.substC g rt1
    rt2' <- lift $ CB.substC g rt2
    subCheckC rt1' rt2'
  -- InstantiateR (for all InstR rules except InstRSolve)
  -- (mt,ExCT b) -> onBase (instRC mt b) g
  (mt,ExCT b) -> zoom base $ instRC mt b
