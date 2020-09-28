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

import Language.Carol.AST
import Language.Carol.TypeContext (Context, MonoType, TErr)
import qualified Language.Carol.TypeContext as Ctx

import Data.Map (Map)
import qualified Data.Map as M

checkV :: (Domain d) => Context d -> Val d -> MonoType d -> TErr (Context d)
checkV g v vt = do
  (vt1,g1) <- synthV g v
  case (vt,vt1) of
    (Ctx.Concrete vt2,Ctx.Concrete vt3) | vt2 == vt3 -> return g1
    (Ctx.Exist a, vt1) -> Ctx.bindEx a vt1 g1
  -- case vt of
  --   Ctx.Concrete vt2 | vt1 == vt2 -> return g1
  --   Ctx.Exist a -> Ctx.bindEx a vt1 g1

synthV :: (Domain d) => Context d -> Val d -> TErr (MonoType d, Context d)
synthV g = \case
  Var x -> case Ctx.isBound x g of
             Just t -> Right (t,g)
  Unit -> return (Ctx.Concrete UnitT, g)
  -- Prod v1 v2 -> do
  --   (vt1,g1) <- synthV g v1
  --   (vt2,g2) <- synthV g1 v2
  --   return (ProdT vt1 vt2, g2)
  Anno v1 vt -> do
    g1 <- checkV g v1 (Ctx.Concrete vt)
    return (Ctx.Concrete vt, g1)

checkC :: (Domain d) => Context d -> Comp d -> CompT (MonoType d) -> TErr (Context d)
checkC g m mt = case (m,mt) of
                  _ -> undefined

synthC :: (Show d, Domain d) => Context d -> Comp d -> TErr (CompT (MonoType d), Context d)
synthC g = \case
  Ret v -> do
    (vt,g1) <- synthV g v
    return (RetT vt, g1)
  Fun x m' -> do
    let (g1,a) = Ctx.newEx g
    (mt,g2) <- synthC (Ctx.varBind x (Ctx.Exist a) g1) m'
    (g3,_) <- Ctx.trimToVar x g2
    return (FunT (Ctx.Exist a) mt, g3)
  Ap v m -> do
    (mt,g1) <- synthC g m
    case mt of
      FunT vt mt2 -> do
        g2 <- checkV g1 v vt
        return (mt2, g2)
      _ -> Left $ "Ap to non-function " ++ show m ++ " (" ++ show mt ++ ")"
