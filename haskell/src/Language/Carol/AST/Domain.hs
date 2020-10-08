{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Carol.AST.Domain
  ( EmptyVD
  , IntVD (..)
  , intT
  , intV
  , IntCD (..)
  , intModT
  , intOrdT
  , addV
  , subV
  , leqV
  , geqV
  , Val'
  , ValT'
  , Comp'
  , CompT'
  , HasValDomain
  ) where

import Language.Carol.AST.Terms
import Language.Carol.AST.Types

import Data.Map (Map)
import qualified Data.Map as M

class (ValDomain s, ValDomain d) => HasValDomain s d where
  liftType :: s -> d
  liftVal :: DVal s -> DVal d

instance (ValDomain d) => HasValDomain d d where
  liftType = id
  liftVal = id

class HasCompDomain f e where
  liftComp :: f -> e

instance HasCompDomain e e where
  liftComp = id

data EmptyVD

data EmptyVDV

instance Show EmptyVD where
  show = undefined
instance Eq EmptyVD where
  a == b = undefined
instance Ord EmptyVD where
  a <= b = undefined

instance ValDomain EmptyVD where
  data DVal EmptyVD
  dValType = undefined

instance Show (DVal EmptyVD) where
  show = undefined
instance Eq (DVal EmptyVD) where
  a == b = undefined
instance Ord (DVal EmptyVD) where
  a <= b = undefined

data EmptyCD

instance (ValDomain d) => CompDomain EmptyCD d where
  dCompSig = undefined
  dCompShow = undefined

data IntVD = IntVD deriving (Eq,Ord)

instance Show IntVD where
  show _ = "Int"

instance ValDomain IntVD where
  data DVal IntVD = IntConst Int deriving (Eq,Ord)
  dValType (IntConst _) = IntVD

instance Show (DVal IntVD) where
  show (IntConst n) = show n

intT :: (HasValDomain IntVD d) => ValT d
intT = DsT (liftType IntVD)

intV :: (CompDomain e d, HasValDomain IntVD d) => Int -> Val e d
intV = DsV . liftVal . IntConst

data IntCD =
    IntMod
  | IntTest
  | IntQuery
  | IntIssue
  | IntProduce
  | IntConsume
  deriving (Show,Eq,Ord)

intDEffSum :: (HasValDomain IntVD d) => Map SumId (ValT d)
intDEffSum = M.fromList
  [(SumId "Add", intT)
  ,(SumId "Sub", intT)]

intModT :: (HasValDomain IntVD d) => ValT d
intModT = SumT intDEffSum

intDOrdSum :: (HasValDomain IntVD d) => Map SumId (ValT d)
intDOrdSum = M.fromList
  [(SumId "LEQ", UnitT)
  ,(SumId "GEQ", UnitT)]

intOrdT :: (HasValDomain IntVD d) => ValT d
intOrdT = SumT intDOrdSum

instance CompDomain IntCD IntVD where
  dCompSig IntMod = ([SumT intDEffSum, intT], intT)
  dCompSig IntTest = ([SumT intDOrdSum, intT, intT], boolT)
  dCompSig IntQuery = ([SumT intDOrdSum], intT)
  dCompSig IntIssue = ([SumT intDEffSum], UnitT)
  dCompSig IntProduce = ([SumT intDEffSum], UnitT)
  dCompSig IntConsume = ([SumT intDEffSum], UnitT)

  dCompShow IntMod vs = "mod " ++ show vs
  dCompShow IntTest vs = "test " ++ show vs

addV :: (CompDomain e d, HasValDomain IntVD d) => Val e d -> Val e d
addV = Sum intDEffSum (SumId "Add")
subV :: (CompDomain e d, HasValDomain IntVD d) => Val e d -> Val e d
subV = Sum intDEffSum (SumId "Sub")

leqV :: (CompDomain e d, HasValDomain IntVD d) => Val e d
leqV = Sum intDOrdSum (SumId "LEQ") Unit
geqV :: (CompDomain e d, HasValDomain IntVD d) => Val e d
geqV = Sum intDOrdSum (SumId "GEQ") Unit

type Val' = Val IntCD IntVD

type ValT' = ValT IntVD

type Comp' = Comp IntCD IntVD

type CompT' = CompT IntVD
