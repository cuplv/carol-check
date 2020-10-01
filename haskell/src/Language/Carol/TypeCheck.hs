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

checkV :: Context -> Val -> ValT -> TErr Context
checkV g v vt = do
  (vt1,g1) <- synthV g v
  case (vt,vt1) of
    (ExVar a, vt1) -> Ctx.bindEx a vt1 g1
    _ | vt == vt1 -> return g1

synthV :: Context -> Val -> TErr (ValT, Context)
synthV g = \case
  Var x -> case Ctx.isBound x g of
             Just t -> Right (t,g)
  Thunk m -> undefined
  Unit -> return (UnitT, g)
  IntConst _ -> return (IntT, g)
  Pair v1 v2 -> do
    (vt1,g1) <- synthV g v1
    (vt2,g2) <- synthV g1 v2
    return (PairT vt1 vt2, g2)
  Anno v1 vt -> do
    g1 <- checkV g v1 vt
    return (vt, g1)

checkC :: Context -> Comp -> CompT -> TErr Context
checkC g m mt = case (m,mt) of
                  _ -> undefined

synthC :: Context -> Comp -> TErr (CompT, Context)
synthC g = \case
  Ret v -> do
    (vt,g1) <- synthV g v
    return (RetT vt, g1)
  Fun (x,m') -> do
    let (g1,a) = Ctx.newEx g
    (mt,g2) <- synthC (Ctx.varBind x (ExVar a) g1) m'
    (g3,_) <- Ctx.trimToVar x g2
    return (FunT (ExVar a) mt, g3)
  Ap v m -> do
    (mt,g1) <- synthC g m
    case mt of
      FunT vt mt2 -> do
        g2 <- checkV g1 v vt
        return (mt2, g2)
      _ -> Left $ "Ap to non-function " ++ show m ++ " (" ++ show mt ++ ")"
  Pute vs op (x,m') -> do
    let (vts,outt) = opSig op
    g' <- foldM (\g (v,vt) -> checkV g v vt) g (zip vs vts)
    synthC (Ctx.varBind x outt g') m'
