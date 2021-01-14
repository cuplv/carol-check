{-# LANGUAGE LambdaCase #-}

module Language.Carol.TypeCheck
  ( checkV
  , synthV
  , checkC
  , synthC
  , TErr
  , TypeError
  , Context
  , emptyContext
  , substC
  ) where

import Control.Monad (foldM)
import Language.Carol.AST
import Language.Carol.TypeCheck.Context
import Language.Carol.TypeCheck.Error
import Language.Carol.TypeCheck.SubCheck

import Data.Map (Map)
import qualified Data.Map as M

checkV :: (CompDomain e d)
  => Val e d
  -> ValT d
  -> Context d
  -> TErr d (Context d)
checkV v vt g = do
  (vt1,g1) <- synthV v g
  subCheckV vt1 vt g1

synthV :: (CompDomain e d)
  => Val e d
  -> Context d
  -> TErr d (ValT d, Context d)
synthV v g = case v of
  Var x -> case isBound x g of
    Just t -> return (t,g)
    Nothing -> terr $ TOther ("Unbound variable \""
                              ++ show x ++ "\"")
  Thunk m -> undefined
  Sum sc i v' -> case M.lookup i sc of
    Just vt -> checkV v' vt g >>= \g' -> return (SumT sc, g')
    Nothing -> terr  $ TOther ("Sum, " ++ show i
                               ++ " not in alts.")
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

checkC :: (CompDomain e d)
  => Comp e d
  -> CompT d
  -> Context d
  -> TErr d (Context d)
checkC m mt2 g = do
  (mt1,g1) <- synthC m g
  mt1s <- substC g1 mt1
  mt2s <- substC g1 mt2
  g2 <- subCheckC mt1s mt2s g1
  return g2

synthC :: (CompDomain e d)
  => Comp e d
  -> Context d
  -> TErr d (CompT d, Context d)
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
      _ -> terr $ TOther "Fun did not typecheck as FunT?"
  Force v -> undefined
  Pmp v (x,y,m') -> undefined
  Pms v alts -> undefined
  Proj i m' -> undefined
  Ap v m -> do
    (mt,g1) <- synthC m g
    mt' <- substC g1 mt
    appSynth mt' v g1
  DsC d vs (mx,m') -> do
    case mx of
      Just x -> do
        (ft,g1) <- synthC (Fun (x,m')) g
        case ft of
          FunT vt mt2 -> do
            -- Use domain logic to produce input type from vt (the output
            -- type) and new comp-refinement for mt2
            undefined
          _ -> terr $ TOther "Fun did not typecheck as FunT?"
      Nothing -> do
        synthC m' g
        -- Use domain logic to produce new comp-refinement for mt2
        undefined
  AnnoC m mt -> do
    g1 <- checkC m mt g
    return (mt,g1)

appSynth :: (CompDomain e d) 
         => CompT d 
         -> Val e d 
         -> Context d 
         -> TErr d (CompT d,Context d)
appSynth mt v g = case mt of
  FunT vt mt' -> do 
    g1 <- checkV v vt g
    return (mt',g1)
