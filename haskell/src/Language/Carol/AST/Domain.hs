{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Carol.AST.Domain
  ( EmptyVD
  , StdVD (..)
  , DVal (..)
  , intT
  , intV
  , stringT
  , stringV
  , StdCD (..)
  , intModT
  , intOrdT
  , addV
  , subV
  , leqV
  , geqV
  , optionT
  , someV
  , noneV
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

data StdVD = IntT | StrT deriving (Eq,Ord)

instance Show StdVD where
  show IntT = "Int"
  show StrT = "String"

instance ValDomain StdVD where
  data DVal StdVD = IntConst Int | StrConst String deriving (Eq,Ord)
  dValType (IntConst _) = IntT
  dValType (StrConst _) = StrT

instance Show (DVal StdVD) where
  show (IntConst n) = show n

intT :: (HasValDomain StdVD d) => ValT d
intT = DsT (liftType IntT)

intV :: (CompDomain e d, HasValDomain StdVD d) => Int -> Val e d
intV = DsV . liftVal . IntConst

stringT :: (HasValDomain StdVD d) => ValT d
stringT = DsT (liftType StrT)

stringV :: (CompDomain e d, HasValDomain StdVD d) => String -> Val e d
stringV = DsV . liftVal . StrConst

data StdCD =
    IntMod
  | IntTest
  | IntQuery
  | IntIssue
  | IntProduce
  | IntConsume
  | StrCat
  | StrCmp
  | StrGet
  | StrPut
  | StrFromInt
  | IntFromStr
  deriving (Show,Eq,Ord)

intDEffSum :: (HasValDomain StdVD d) => Map SumId (ValT d)
intDEffSum = M.fromList
  [(SumId "Add", intT)
  ,(SumId "Sub", intT)]

intModT :: (HasValDomain StdVD d) => ValT d
intModT = SumT intDEffSum

intDOrdSum :: (HasValDomain StdVD d) => Map SumId (ValT d)
intDOrdSum = M.fromList
  [(SumId "LEQ", UnitT)
  ,(SumId "GEQ", UnitT)]

intOrdT :: (HasValDomain StdVD d) => ValT d
intOrdT = SumT intDOrdSum

instance CompDomain StdCD StdVD where
  dCompSig = \case
    StrCat -> ([stringT,stringT],stringT)
    StrCmp -> ([stringT,stringT],boolT)
    StrGet -> ([],stringT)
    StrPut -> ([stringT],UnitT)

    StrFromInt -> ([intT],stringT)
    IntFromStr -> ([stringT], optionT intT)

    IntMod -> ([SumT intDEffSum, intT], intT)
    IntTest -> ([SumT intDOrdSum, intT, intT], boolT)
    IntQuery -> ([SumT intDOrdSum], intT)
    IntIssue -> ([SumT intDEffSum], UnitT)
    IntProduce -> ([SumT intDEffSum], UnitT)
    IntConsume -> ([SumT intDEffSum], UnitT)

  dCompShow IntMod vs = "mod " ++ show vs
  dCompShow IntTest vs = "test " ++ show vs
  dCompShow _ _ = "[thing]"

addV :: (CompDomain e d, HasValDomain StdVD d) => Val e d -> Val e d
addV = Sum intDEffSum (SumId "Add")
subV :: (CompDomain e d, HasValDomain StdVD d) => Val e d -> Val e d
subV = Sum intDEffSum (SumId "Sub")

leqV :: (CompDomain e d, HasValDomain StdVD d) => Val e d
leqV = Sum intDOrdSum (SumId "LEQ") Unit
geqV :: (CompDomain e d, HasValDomain StdVD d) => Val e d
geqV = Sum intDOrdSum (SumId "GEQ") Unit

someS = SumId "Some"
noneS = SumId "None"

optSum :: (ValDomain d) => ValT d -> Map SumId (ValT d)
optSum vt = M.fromList [(someS, vt), (noneS, UnitT)]

optionT :: (ValDomain d) => ValT d -> ValT d
optionT = SumT . optSum

someV :: (CompDomain e d) => ValT d -> Val e d -> Val e d
someV vt v = Sum (optSum vt) someS v

noneV :: (CompDomain e d) => ValT d -> Val e d
noneV vt = Sum (optSum vt) noneS Unit

type Val' = Val StdCD StdVD

type ValT' = ValT StdVD

type Comp' = Comp StdCD StdVD

type CompT' = CompT StdVD
