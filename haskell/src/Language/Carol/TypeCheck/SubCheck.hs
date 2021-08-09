{-# LANGUAGE FlexibleContexts #-}

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
  (FunT x1 vt1 mt1, FunT x2 vt2 mt2) -> do
    -- This could be made cleaner by moving the attachVar logic into
    -- FunT's own structure, by splitting FunT into a Forall (which
    -- adds the varBind when checked) and FunT, and giving a function
    -- for constructing a Forall.FunT of this form.
    vt1' <- attachVar x1 vt1
    vt2' <- attachVar x2 vt2
    subCheckV vt2' vt1'
    mt1' <- CB.substC' base mt1
    mt2' <- CB.substC' base mt2
    subCheckC mt1' mt2'

    where attachVar mx vt = case mx of
            Just (VarId x) -> do base %= CB.varBind (VarId x) vt
                                 return $ addEqRef (IVarId x) vt
            Nothing -> return vt

  -- InstantiateR (for all InstR rules except InstRSolve)
  (mt,ExCT b) -> zoom base $ instRC mt b
  _ -> lift.terr $ TCMismatch mt1 mt2
