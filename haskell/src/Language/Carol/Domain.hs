{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Carol.Domain
  ( EmptyVD
  , StdVD (..)
  , DVal (..)
  , intT
  , intTLe
  , intTGe
  , intTR
  , intV
  , StdCD (..)
  , Val'
  , ValT'
  , ValTR'
  , Comp'
  , CompT'
  ) where

import Language.Carol.AST.PrettyPrint
import Language.Carol.AST.Terms
import Language.Carol.AST.Types
import Language.Carol.TypeCheck.Solve

import Data.Map (Map)
import qualified Data.Map as M
import Data.SBV

data EmptyVD

instance Eq EmptyVD where
  a == b = undefined
instance Ord EmptyVD where
  a <= b = undefined
instance Show EmptyVD where
  show = undefined
instance Pretty EmptyVD where
  pretty = undefined

instance RefDomain EmptyVD where
  data DRef EmptyVD = EmptyVD deriving (Eq,Ord,Show)
  refEmpty = EmptyVD

instance Pretty (DRef EmptyVD) where
  pretty = show

instance RefSolve EmptyVD where
  subRef _ _ g = return g

instance ValDomain EmptyVD where
  data DVal EmptyVD
  dValType = undefined

instance Show (DVal EmptyVD) where
  show = undefined
instance Eq (DVal EmptyVD) where
  a == b = undefined
instance Ord (DVal EmptyVD) where
  a <= b = undefined
instance Pretty (DVal EmptyVD) where
  pretty = undefined

data EmptyCD

instance (ValDomain d) => CompDomain EmptyCD d where
  dCompSig = undefined
  dCompPretty = undefined


data StdVD = IntT deriving (Eq,Ord)

instance Show StdVD where
  show IntT = "Int"

instance Pretty StdVD where
  pretty = show

instance RefDomain StdVD where
  data DRef StdVD = 
      RefTrue 
    | RefAnd (DRef StdVD) (DRef StdVD)
    | LEQ Int 
    | GEQ Int 
    deriving (Show,Eq,Ord)

instance Pretty (DRef StdVD) where
  pretty RefTrue = "true"
  pretty (RefAnd a b) = pretty a ++ " and " ++ pretty b
  pretty (LEQ n) = "ν ≤ " ++ show n
  pretty (GEQ n) = "ν ≥ " ++ show n

instance ValDomain StdVD where
  data DVal StdVD = IntConst Int | StrConst String deriving (Show,Eq,Ord)
  dValType (IntConst n) = (IntT, RefAnd (GEQ n) (LEQ n))

instance Pretty (DVal StdVD) where
  pretty (IntConst n) = show n

intT :: ValT StdVD
intT = (DsT IntT)

intTLe :: Int -> ValTR StdVD
intTLe n = ValTR (DsT IntT) (LEQ n)

intTGe :: Int -> ValTR StdVD
intTGe n = ValTR (DsT IntT) (GEQ n)

intTR :: Int -> Int -> ValTR StdVD
intTR low high = ValTR (DsT IntT) (RefAnd (GEQ low) (LEQ high))

intV :: (CompDomain e StdVD) => Int -> Val e StdVD
intV = DsV . IntConst

data StdCD =
    IntAdd
  | IntSub
  deriving (Show,Eq,Ord)

lsPretty :: (Pretty a) => [a] -> String
lsPretty [] = ""
lsPretty (a:as) = pretty a ++ "," ++ lsPretty as

instance CompDomain StdCD StdVD where
  -- dCompSig = \case
  --   StrCat -> ([stringT,stringT],stringT)
  --   StrCmp -> ([stringT,stringT],boolT)
  --   StrGet -> ([],stringT)
  --   StrPut -> ([stringT],UnitT)

  --   StrFromInt -> ([intT],stringT)
  --   IntFromStr -> ([stringT], optionT intT)

  --   IntMod -> ([SumT intDEffSum, intT], intT)
  --   IntTest -> ([SumT intDOrdSum, intT, intT], boolT)
  --   IntQuery -> ([SumT intDOrdSum], intT)
  --   IntIssue -> ([SumT intDEffSum], UnitT)
  --   IntProduce -> ([SumT intDEffSum], UnitT)
  --   IntConsume -> ([SumT intDEffSum], UnitT)

  -- dCompPretty IntMod vs = "mod " ++ lsPretty vs
  -- dCompPretty IntTest vs = "test " ++ lsPretty vs
  -- dCompPretty _ _ = "[thing]"

-- addV :: (CompDomain e StdVD) => Val e StdVD -> Val e StdVD
-- addV = Sum intDEffSum (SumId "Add")
-- subV :: (CompDomain e StdVD) => Val e StdVD -> Val e StdVD
-- subV = Sum intDEffSum (SumId "Sub")

-- leqV :: (CompDomain e StdVD) => Val e StdVD
-- leqV = Sum intDOrdSum (SumId "LEQ") Unit
-- geqV :: (CompDomain e StdVD) => Val e StdVD
-- geqV = Sum intDOrdSum (SumId "GEQ") Unit

-- someS = SumId "Some"
-- noneS = SumId "None"

-- optSum :: (ValDomain d) => ValT d -> Map SumId (ValT d)
-- optSum vt = M.fromList [(someS, vt), (noneS, UnitT)]

-- optionT :: (ValDomain d) => ValT d -> ValT d
-- optionT = SumT . optSum

-- someV :: (CompDomain e d) => ValT d -> Val e d -> Val e d
-- someV vt v = Sum (optSum vt) someS v

-- noneV :: (CompDomain e d) => ValT d -> Val e d
-- noneV vt = Sum (optSum vt) noneS Unit

type Val' = Val StdCD StdVD

type ValT' = ValT StdVD

type ValTR' = ValTR StdVD

type Comp' = Comp StdCD StdVD

type CompT' = CompT StdVD
