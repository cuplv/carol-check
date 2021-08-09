module Language.Carol.TypeCheck.SubCheck where

import qualified Data.Map as Map
import Data.SBV
import Control.Monad.IO.Class

import Language.Carol.AST.Refinement
import Language.Carol.AST.Types
import Language.Carol.Prelude.Internal
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
      nu <- mkSym "nu" t1
      m <- CB.quantifyContext g
      pre <- case refs m of
               Right p -> return p
               Left e -> do liftIO $ putStrLn e
                            -- liftIO $ print m
                            undefined
      a <- case rpred m r1 nu of
             Right p -> return p
             Left e -> do liftIO $ putStrLn e
                          -- liftIO $ print m
                          undefined
      b <- case rpred m r2 nu of
             Right p -> return p
             Left e -> do liftIO $ putStrLn e
                          -- liftIO $ print m
                          undefined
      -- return $ refs m .&& rpred m r1 nu .=> rpred m r2 nu
      return $ pre .&& a .=> b
    if result
       then return ()
       else lift.terr $ TMismatch vt1 vt2
    where -- AND all the constraints from the
          -- context, applied to their quantified
          -- variables (just operate on the values
          -- of the map).
          -- refs m = Map.foldr (\(x,r) p -> rpred m r x .&& p) sTrue m
          refs m = (foldr (.&&) sTrue) <$> mapM (\(x,r) -> rpred m r x) (Map.elems m)
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
  -- (FunT (Just (VarId x1)) xt1 rt1, FunT (Just (VarId x2)) xt2 rt2) -> do
  --   -- Look at Dec-<:-Fun from LQ paper to fix this one once argument
  --   -- refinement vars are added to the FunT.
  --   -- 
  --   -- It looks like the solution is to replace the variable in one to
  --   -- match the other, and then use the stronger argument type to
  --   -- verify the bodies.
  --   subCheckV (addEqRef (IVarId x2) xt2) (addEqRef (IVarId x1) xt1)

  --   base %= CB.varBind (VarId x2) (addEqRef (IVarId x1) xt2)
  --   rt1' <- CB.substC' base (subiC (IVarId x1) (IVarId x2) rt1)
  --   rt2' <- CB.substC' base rt2
  --   subCheckC rt1' rt2'
  --   -- base %>= CB.trimToVar (VarId x2)
  (FunT Nothing xt1 rt1, FunT (Just (VarId x2)) xt2 rt2) -> do
    subCheckV (addEqRef (IVarId x2) xt2) xt1
    -- subCheckV xt2 xt1
    base %= CB.varBind (VarId x2) xt1
    rt1' <- CB.substC' base rt1
    rt2' <- CB.substC' base rt2
    subCheckC rt1' rt2'
    -- base %>= CB.trimToVar (VarId x2)
  (FunT _ xt1 rt1, FunT Nothing xt2 rt2) -> do
    subCheckV xt2 xt1
    rt1' <- CB.substC' base rt1
    rt2' <- CB.substC' base rt2
    subCheckC rt1' rt2'
  -- InstantiateR (for all InstR rules except InstRSolve)
  (mt,ExCT b) -> zoom base $ instRC mt b
  _ -> lift.terr $ TCMismatch mt1 mt2
