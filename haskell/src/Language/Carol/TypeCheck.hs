{-# LANGUAGE LambdaCase #-}

module Language.Carol.TypeCheck
  ( checkV
  , synthV
  , checkC
  , synthC
  , Context
  , emptyContext
  , substC
  ) where

import Control.Monad (foldM)
import Language.Carol.AST
import Language.Carol.TypeCheck.Context
import Language.Carol.TypeCheck.SubCheck

import Data.Map (Map)
import qualified Data.Map as M

checkV :: (CompDomain e d) => Val e d -> ValT d -> Context d -> TErr (Context d)
checkV v vt1 g = do
  (vt2,g1) <- synthV v g
  subCheckV vt1 vt2 g1

synthV :: (CompDomain e d) => Val e d -> Context d -> TErr (ValT d, Context d)
synthV v g = case v of
  Var x -> case isBound x g of
             Just t -> Right (t,g)
             Nothing -> Left $ "Unbound variable \"" ++ show x ++ "\""
  Thunk m -> undefined
  Sum sc i v' -> case M.lookup i sc of
    Just vt -> checkV v' vt g >>= \g' -> return (SumT sc, g')
    Nothing -> Left $ " not in "
  Unit -> return (UnitT, g)
  Pair v1 v2 -> do
    (vt1,g1) <- synthV v1 g
    (vt2,g2) <- synthV v2 g1
    return (PairT vt1 vt2, g2)
  DsV dv -> let (d,r) = dValType dv
            in return (DsT d r, g)
  Anno v1 vt -> do
    g1 <- checkV v1 vt g
    return (vt, g1)

checkC :: (CompDomain e d) => Comp e d -> CompT d -> Context d -> TErr (Context d)
checkC m mt2 g = do
  (mt1,g1) <- synthC m g
  mt1s <- substC g1 mt1
  mt2s <- substC g1 mt2
  g2 <- subCheckC mt1s mt2s g1
  return g2

synthC :: (CompDomain e d) => Comp e d -> Context d -> TErr (CompT d, Context d)
synthC m g = case m of
  Ret v -> do
    (vt,g1) <- synthV v g
    return (RetT vt, g1)
  Prod parts -> undefined
  Fun (x,m') -> do
    let (g1,a) = newExV g
    let (g2,b) = newExC g1
    g3 <- checkC m' (ExCT b) (varBind x (ExVT a) g2)
    g4 <- trimToVar x g3
    return (FunT (ExVT a) (ExCT b), g4)
  Let v abs -> synthC (Ap v (Fun abs)) g
  Bind m1 abs -> do
    (ft,g1) <- synthC (Fun abs) g
    case ft of
      FunT vt mt2 -> do
        (mt1,g2) <- synthC m1 g1
        g3 <- subCheckC mt1 (RetT vt) g2
        return (mt2, g3)
      _ -> Left "Fun did not typecheck as FunT?"
  Force v -> undefined
  Pmp v (x,y,m') -> undefined
  Pms v alts -> undefined
  Proj i m' -> undefined
  Ap v m -> do
    (mt,g1) <- synthC m g
    mt' <- substC g1 mt
    appSynth mt' v g1
  DsC d vs (mx,m') -> do
    let (vts,outVT) = dCompSig d
    g1 <- foldM (\g (v,vt) -> checkV v vt g) g (zip vs vts)
    case mx of
      Just x -> synthC m' (varBind x outVT g1)
      Nothing -> synthC m' g1

appSynth :: (CompDomain e d) 
         => CompT d 
         -> Val e d 
         -> Context d 
         -> TErr (CompT d,Context d)
appSynth mt v g = case mt of
  FunT vt mt' -> do 
    g1 <- checkV v vt g
    return (mt',g1)
