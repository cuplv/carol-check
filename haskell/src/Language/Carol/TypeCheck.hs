{-# LANGUAGE LambdaCase #-}

module Language.Carol.TypeCheck
  ( checkV
  , synthV
  , checkC
  , synthC
  , Context
  , Ctx.emptyContext
  , Ctx.substC
  ) where

import Control.Monad (foldM)
import Language.Carol.AST
import Language.Carol.TypeContext (Context, TErr)
import qualified Language.Carol.TypeContext as Ctx

import Data.Map (Map)
import qualified Data.Map as M

checkV :: (Domain d) => Val d -> ValT -> Context -> TErr Context
checkV v vt g = do
  (vt1,g1) <- synthV v g
  case (vt,vt1) of
    (ExVar a, vt1) -> Ctx.bindEx a vt1 g1
    (vt,ExVar a) -> Ctx.bindEx a vt g1
    _ | vt == vt1 -> return g1
    _ -> Left $ show vt ++ " does not match " ++ show vt1

synthV :: (Domain d) => Val d -> Context -> TErr (ValT, Context)
synthV v g = case v of
  Var x -> case Ctx.isBound x g of
             Just t -> Right (t,g)
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
checkC m mt g = case (m,mt) of
                  _ -> undefined

dst :: (Domain d) => d -> ValT
dst = domStateType
det :: (Domain d) => d -> ValT
det = domEffectType
dot :: (Domain d) => d -> ValT
dot = domOrderType

synthC :: (Domain d) => Comp d -> Context -> TErr (CompT, Context)
synthC m g = case m of
  Ret v -> do
    (vt,g1) <- synthV v g
    return (RetT vt, g1)
  Fun (x,m') -> do
    let (g1,a) = Ctx.newEx g
    (mt,g2) <- synthC m' (Ctx.varBind x (ExVar a) g1)
    mt' <- Ctx.substC g2 mt
    (g3,_) <- Ctx.trimToVar x g2
    return (FunT (ExVar a) mt', g3)
  Ap v m -> do
    (mt,g1) <- synthC m g
    case mt of
      FunT vt mt2 -> (,) <$> pure mt2 <*> checkV v vt g1
      -- -- The following alternative case performs an extra substV on
      -- -- vt that the bidir algorithm rules seem to ask for, but
      -- -- which is not so far needed for tests.  This can be deleted
      -- -- if it is still not needed after sufficient testing.
      --
      -- FunT vt mt2 -> do
      --   vt' <- Ctx.substV g1 vt
      --   g2 <- checkV v vt' g1
      --   return (mt2,g2)
      _ -> Left $ "Ap to non-function " ++ show m ++ " (" ++ show mt ++ ")"
  DMod d op arg (x,m') -> 
    synthC m'
    =<< return . Ctx.varBind x (dst d)
    =<< checkV arg (dst d)
    =<< checkV op (det d) g
  DTest d op (arg1,arg2) (x,m') ->
    synthC m'
    =<< return . Ctx.varBind x boolT
    =<< checkV arg2 (dst d)
    =<< checkV arg1 (dst d)
    =<< checkV op (dot d) g
