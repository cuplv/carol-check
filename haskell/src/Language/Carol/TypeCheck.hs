{-# LANGUAGE LambdaCase #-}

module Language.Carol.TypeCheck
  ( closedC
  , checkV
  , synthV
  , checkC
  , synthC
  , TErr
  , runTErr
  , TypeError
  , Context
  ) where

import Language.Carol.AST
import Language.Carol.Prelude.Internal
import Language.Carol.TypeCheck.Context
import qualified Language.Carol.TypeCheck.Context.Base as CB
import Language.Carol.TypeCheck.Error
import Language.Carol.TypeCheck.SubCheck

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as M

checkV :: (CompDomain e d)
  => Val e d
  -> ValT d
  -> StateT (Context d) (TErr d) ()
checkV v vt = do
  vt1 <- synthV v
  subCheckV vt1 vt

synthV :: (CompDomain e d)
  => Val e d
  -> StateT (Context d) (TErr d) (ValT d)
synthV v = case v of
  Var x -> do
    g <- use base
    case CB.isBound x g of
      Just t -> return t
      Nothing -> lift.terr $ TOther ("Unbound variable \""
                                     ++ show x ++ "\"")
  Thunk m -> undefined
  Sum sc i v' -> case M.lookup i sc of
    Just vt -> checkV v' vt >> return (SumT sc)
    Nothing -> lift.terr $ TOther ("Sum, " ++ show i
                                   ++ " not in alts.")
  Unit -> return UnitT
  Pair v1 v2 -> PairT <$> synthV v1 <*> synthV v2
  DsV dv -> let (d,r) = dValType dv
            in return (DsT d r)
  Anno v1 vt -> checkV v1 vt >> return vt

checkC :: (CompDomain e d)
  => Comp e d
  -> CompT d
  -> StateT (Context d) (TErr d) ()
checkC m mt2 = do
  mt1 <- synthC m
  mt1s <- CB.substC' base mt1
  mt2s <- CB.substC' base mt2
  subCheckC mt1s mt2s

synthC :: (CompDomain e d)
  => Comp e d
  -> StateT (Context d) (TErr d) (CompT d)
synthC m = case m of
  Ret v -> RetT <$> synthV v
  Prod parts -> undefined
  Fun (x,m') -> do
    g <- use base
    let (g1,a) = CB.newExV g
    let (g2,b) = CB.newExC g1
    base .= g2

    -- Try writing a "withBinding" computation that adds a variable,
    -- does whatever, and the removes the variable before returing.
    -- That would be less error-prone than adding and removing the
    -- variable manually.
    base %= CB.varBind x (ExVT a)
    checkC m' (ExCT b)
    base %>= CB.trimToVar x
    return (FunT (ExVT a) (ExCT b))
  Let v abs -> synthC (Ap v (Fun abs))
  Bind m1 abs -> do
    ft <- synthC (Fun abs)
    case ft of
      FunT vt mt2 -> do
        mt1 <- synthC m1
        subCheckC mt1 (RetT vt)
        return mt2
      _ -> lift.terr $ TOther "Fun did not typecheck as FunT?"
  Force v -> undefined
  Pmp v (x,y,m') -> undefined
  Pms v alts -> undefined
  Proj i m' -> undefined
  Ap v m -> do
    mt <- CB.substC' base =<< synthC m
    appSynth mt v
  DsC d v (mx,m') -> do
    let (vars,vt,outVT) = dCompSigR d
    base %= (\g -> foldr (\(a,s) -> CB.exIdx a s) g vars)
    -- Here, we don't replace the index variables in vt with our
    -- existentially quantified vars, because they are the same.  To
    -- avoid namespace collisions, we should actually generate fresh
    -- existential vars and then replace them accordingly.
    checkV v vt
    case mx of
      Just x -> base %= CB.varBind x outVT
      Nothing -> return ()
    synthC m'
  AnnoC m mt -> checkC m mt >> return mt

-- | Synthesize a type for a closed computation (by running synthC
-- with an empty context).
closedC :: (CompDomain e d) => Comp e d -> TErr d (CompT d)
closedC m = evalStateT (CB.substC' base =<< synthC m) emptyContext

appSynth :: (CompDomain e d)
         => CompT d
         -> Val e d
         -> StateT (Context d) (TErr d) (CompT d)
appSynth (FunT vt mt') v = checkV v vt >> return mt'
appSynth _ _ = lift.terr.TOther $
  "Non-function type given to appSynth."
