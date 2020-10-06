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

checkV :: (Domain d) => Val d -> ValT -> Context -> TErr Context
checkV v vt1 g = do
  (vt2,g1) <- synthV v g
  subCheckV vt1 vt2 g1

synthV :: (Domain d) => Val d -> Context -> TErr (ValT, Context)
synthV v g = case v of
  Var x -> case isBound x g of
             Just t -> Right (t,g)
             Nothing -> Left $ "Unbound variable \"" ++ show x ++ "\""
  Thunk m -> undefined
  Sum sc i v' -> case M.lookup i sc of
    Just vt -> checkV v' vt g >>= \g' -> return (SumT sc, g')
    Nothing -> Left $ show i ++ " not in " ++ show sc
  Unit -> return (UnitT, g)
  IntConst _ -> return (IntT, g)
  Pair v1 v2 -> do
    (vt1,g1) <- synthV v1 g
    (vt2,g2) <- synthV v2 g1
    return (PairT vt1 vt2, g2)
  Anno v1 vt -> do
    g1 <- checkV v1 vt g
    return (vt, g1)

checkC :: (Domain d) => Comp d -> CompT -> Context -> TErr Context
checkC m mt2 g = do
  (mt1,g1) <- synthC m g
  mt1s <- substC g1 mt1
  mt2s <- substC g1 mt2
  g2 <- subCheckC mt1s mt2s g1
  return g2

synthC :: (Domain d) => Comp d -> Context -> TErr (CompT, Context)
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
  DMod d op arg (x,m') ->
    synthC m'
    =<< return . varBind x (dst d)
    =<< checkV arg (dst d)
    =<< checkV op (dmt d) g
  DTest d op (arg1,arg2) (x,m') ->
    synthC m'
    =<< return . varBind x boolT
    =<< checkV arg2 (dst d)
    =<< checkV arg1 (dst d)
    =<< checkV op (dot d) g
  DIssue d op m' -> synthC m' =<< checkV op (dmt d) g
  DQuery d op (x,m') ->
    synthC m'
    =<< return . varBind x (dst d)
    =<< checkV op (dot d) g
  DProduce d op m' -> synthC m' =<< checkV op (dmt d) g
  DConsume d op m' -> synthC m' =<< checkV op (dmt d) g

appSynth :: (Domain d) => CompT -> Val d -> Context -> TErr (CompT,Context)
appSynth mt v g = case mt of
  FunT vt mt' -> do 
    g1 <- checkV v vt g
    return (mt',g1)

dst :: (Domain d) => d -> ValT
dst = domStateType
dmt :: (Domain d) => d -> ValT
dmt = domModType
dot :: (Domain d) => d -> ValT
dot = domOrderType
