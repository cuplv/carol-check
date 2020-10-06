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
import Language.Carol.TypeCheck.Context (Context, TErr)
import qualified Language.Carol.TypeCheck.Context as Ctx

import Data.Map (Map)
import qualified Data.Map as M

checkV :: (Domain d) => Val d -> ValT -> Context -> TErr Context
checkV v vt1 g = do
  (vt2,g1) <- synthV v g
  matchV vt1 vt2 g1

-- | The Sub rule for values, basically
matchV :: ValT -> ValT -> Context -> TErr Context
matchV vt1 vt2 g = case (vt1,vt2) of
  (ExVar a1, ExVar a2) | a1 > a2 -> Ctx.bindExV a1 (ExVar a2) g
  (ExVar a1, ExVar a2) | a1 < a2 -> Ctx.bindExV a2 (ExVar a1) g
  (ExVar a, vt2) -> Ctx.bindExV a vt2 g
  (vt1, ExVar a) -> Ctx.bindExV a vt1 g
  _ | vt1 == vt2 -> return g
  _ -> Left $ show vt1 ++ " does not match " ++ show vt2

synthV :: (Domain d) => Val d -> Context -> TErr (ValT, Context)
synthV v g = case v of
  Var x -> case Ctx.isBound x g of
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

instLV :: ExTypeId -> ValT -> Context -> TErr Context
instLV a vt g = case vt of
  -- InstLReach
  ExVar a1 | a < a1 -> Ctx.bindExV a1 (ExVar a) g
  -- InstLSolve
  vt -> Ctx.bindExV a vt g

instRC :: CompT -> ExTypeId -> Context -> TErr Context
instRC mt b g = case mt of
  -- InstRReach
  ExVarC b1 | b < b1 -> Ctx.bindExC b1 (ExVarC b) g
  -- InstRArr
  FunT vt mt' -> do
    (aNew,bNew,g1) <- Ctx.inb42 b g
    g2 <- instLV aNew vt g1
    mt'' <- Ctx.substC g2 mt'
    instRC mt'' bNew g2
  mt -> Ctx.bindExC b mt g

subC :: CompT -> CompT -> Context -> TErr Context
subC mt1 mt2 g = case (mt1,mt2) of
  -- Exvar
  (ExVarC b1,ExVarC b2) | b1 == b2 -> return g
  -- InstantiateL
  (RetT (ExVar a),RetT vt) -> instLV a vt g
  -- InstantiateR
  (mt,ExVarC b) -> instRC mt b g

checkC :: (Domain d) => Comp d -> CompT -> Context -> TErr Context
checkC m mt2 g = do
  (mt1,g1) <- synthC m g
  mt1s <- Ctx.substC g1 mt1
  mt2s <- Ctx.substC g1 mt2
  g2 <- subC mt1s mt2s g1
  return g2

dst :: (Domain d) => d -> ValT
dst = domStateType
dmt :: (Domain d) => d -> ValT
dmt = domModType
dot :: (Domain d) => d -> ValT
dot = domOrderType

matchC :: CompT -> CompT -> Context -> TErr Context
matchC mt1 mt2 g = case (mt1,mt2) of
  (RetT vt1, RetT vt2) -> do
    -- vt1' <- Ctx.substV g vt1
    -- vt2' <- Ctx.substV g vt2
    matchV vt1 vt2 g
  
  _ -> Left $ "Whoops, need more matchC cases: " ++ show (mt1,mt2)

synthC :: (Domain d) => Comp d -> Context -> TErr (CompT, Context)
synthC m g = case m of
  Ret v -> do
    (vt,g1) <- synthV v g
    return (RetT vt, g1)
  Prod parts -> undefined
  Fun (x,m') -> do
    let (g1,a) = Ctx.newExV g
    let (g2,b) = Ctx.newExC g1
    g3 <- checkC m' (ExVarC b) (Ctx.varBind x (ExVar a) g2)
    (g4,_) <- Ctx.trimToVar x g3
    return (FunT (ExVar a) (ExVarC b), g4)
  Let v abs -> synthC (Ap v (Fun abs)) g
  Bind m1 abs -> do
    (ft,g1) <- synthC (Fun abs) g
    case ft of
      FunT vt mt2 -> do
        (mt1,g2) <- synthC m1 g1
        g3 <- matchC mt1 (RetT vt) g2
        return (mt2, g3)
      _ -> Left "Fun did not typecheck as FunT?"
  Force v -> undefined
  Pmp v (x,y,m') -> undefined
  Pms v alts -> undefined
  Proj i m' -> undefined
  Ap v m -> do
    (mt,g1) <- synthC m g
    mt' <- Ctx.substC g1 mt
    appSynth mt' v g1
  DMod d op arg (x,m') -> 
    synthC m'
    =<< return . Ctx.varBind x (dst d)
    =<< checkV arg (dst d)
    =<< checkV op (dmt d) g
  DTest d op (arg1,arg2) (x,m') ->
    synthC m'
    =<< return . Ctx.varBind x boolT
    =<< checkV arg2 (dst d)
    =<< checkV arg1 (dst d)
    =<< checkV op (dot d) g

appSynth :: (Domain d) => CompT -> Val d -> Context -> TErr (CompT,Context)
appSynth mt v g = case mt of
  FunT vt mt' -> do 
    g1 <- checkV v vt g
    return (mt',g1)
