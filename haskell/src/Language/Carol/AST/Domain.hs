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
  , intTLe
  , intTLe'
  , intTGe
  , intTGe'
  , intTR
  , intTEq
  , intV
  , intSort
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
  ) where

import Language.Carol.AST.PrettyPrint
import Language.Carol.AST.Terms
import Language.Carol.AST.Types

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
  data DRef EmptyVD
  data ISort EmptyVD
  refConstraint = undefined
  subVar = undefined

instance Eq (DRef EmptyVD) where
  a == b = undefined
instance Ord (DRef EmptyVD) where
  a <= b = undefined
instance Show (DRef EmptyVD) where
  show = undefined
instance Pretty (DRef EmptyVD) where
  pretty = undefined

instance Eq (ISort EmptyVD) where
  a == b = undefined
instance Ord (ISort EmptyVD) where
  a <= b = undefined
instance Show (ISort EmptyVD) where
  show = undefined
instance Pretty (ISort EmptyVD) where
  pretty = undefined

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
  dCompSigR = undefined
  dCompPretty = undefined

data StdVD = IntT | StrT deriving (Eq,Ord)

instance Show StdVD where
  show IntT = "Int"
  show StrT = "String"

instance Pretty StdVD where
  pretty = show

data IntObject = Literal Int | IntVar IVarId | IntAddObj IntObject IntObject deriving (Show,Eq,Ord)

instance Pretty IntObject where
  pretty (Literal n) = show n
  pretty (IntVar a) = pretty a
  pretty (IntAddObj a b) = pretty a ++ " + " ++ pretty b

instance RefDomain StdVD where
  data DRef StdVD = LEQ IntObject | GEQ IntObject deriving (Show,Eq,Ord)
  data ISort StdVD = IntS deriving (Show,Eq,Ord)
  refConstraint m = \case
    -- LEQ (Literal i) -> \v -> v .<= literal (fromIntegral i)
    -- LEQ (IntVar a) -> case M.lookup a m of
    --                     Just x -> \v -> v .<= x
    -- GEQ (Literal i) -> \v -> v .>= literal (fromIntegral i)
    -- GEQ (IntVar a) -> case M.lookup a m of
    --                     Just x -> \v -> v .>= x
    LEQ o -> let s = mkobj o
             in \v -> v .<= s
    GEQ o -> let s = mkobj o
             in \v -> v .>= s
    where mkobj = \case
            Literal i -> literal (fromIntegral i)
            IntVar a -> case M.lookup a m of
                          Just x -> x
            IntAddObj o1 o2 -> mkobj o1 + mkobj o2
  subVar a (LEQ (IntVar a')) = if a == a'
                               then LEQ (IntVar a)
                               else LEQ (IntVar a')
  subVar _ (LEQ n) = GEQ n
  subVar a (GEQ (IntVar a')) = if a == a'
                               then GEQ (IntVar a)
                               else GEQ (IntVar a')
  subVar _ (GEQ n) = GEQ n

intSort = IntS

instance Pretty (DRef StdVD) where
  pretty (LEQ n) = "ν ≤ " ++ pretty n
  pretty (GEQ n) = "ν ≥ " ++ pretty n

instance ValDomain StdVD where
  data DVal StdVD = IntConst Int | StrConst String deriving (Show,Eq,Ord)
  dValType (IntConst n) = (IntT, RefAnd 
                                   (RefAtom $ GEQ (Literal n)) 
                                   (RefAtom $ LEQ (Literal n)))
  dValType (StrConst _) = (StrT, RefTrue)

instance Pretty (DVal StdVD) where
  pretty (IntConst n) = show n
  pretty (StrConst s) = show s

intT :: ValT StdVD
intT = DsT IntT RefTrue

intTLe :: Int -> ValT StdVD
intTLe n = DsT IntT (RefAtom $ LEQ (Literal n))

intTLe' :: IVarId -> ValT StdVD
intTLe' a = DsT IntT (RefAtom $ LEQ (IntVar a))

intTGe :: Int -> ValT StdVD
intTGe n = DsT IntT (RefAtom $ GEQ (Literal n))

intTGe' :: IVarId -> ValT StdVD
intTGe' a = DsT IntT (RefAtom $ GEQ (IntVar a))

intTR :: Int -> Int -> ValT StdVD
intTR low high = DsT IntT (RefAnd 
                             (RefAtom $ GEQ (Literal low)) 
                             (RefAtom $ LEQ (Literal high)))

intTEq :: IVarId -> ValT StdVD
intTEq a = DsT IntT (RefAnd (RefAtom $ GEQ (IntVar a))
                            (RefAtom $ LEQ (IntVar a)))

intV :: (CompDomain e StdVD) => Int -> Val e StdVD
intV = DsV . IntConst

stringT :: ValT StdVD
stringT = DsT StrT RefTrue

stringV :: (CompDomain e StdVD) => String -> Val e StdVD
stringV = DsV . StrConst

data StdCD =
    IntMod
  | IntAdd
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

intDEffSum :: Map SumId (ValT StdVD)
intDEffSum = M.fromList
  [(SumId "Add", intT)
  ,(SumId "Sub", intT)]

intModT :: ValT StdVD
intModT = SumT intDEffSum

intDOrdSum :: Map SumId (ValT StdVD)
intDOrdSum = M.fromList
  [(SumId "LEQ", UnitT)
  ,(SumId "GEQ", UnitT)]

intOrdT :: ValT StdVD
intOrdT = SumT intDOrdSum

lsPretty :: (Pretty a) => [a] -> String
lsPretty [] = ""
lsPretty (a:as) = pretty a ++ "," ++ lsPretty as

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
  dCompSigR = \case
    IntAdd -> ([(IVarId "n1", intSort), (IVarId "n2", intSort)]
              ,PairT (intTEq (IVarId "n1")) (intTEq (IVarId "n2"))
              ,DsT IntT (RefAnd (RefAtom $ GEQ (IntAddObj 
                                                  (IntVar$ IVarId "n1")
                                                  (IntVar$ IVarId "n2")))
                                (RefAtom $ LEQ (IntAddObj 
                                                  (IntVar$ IVarId "n1")
                                                  (IntVar$ IVarId "n2")))))

  dCompPretty IntMod vs = "mod " ++ lsPretty vs
  dCompPretty IntTest vs = "test " ++ lsPretty vs
  dCompPretty _ _ = "[thing]"

addV :: (CompDomain e StdVD) => Val e StdVD -> Val e StdVD
addV = Sum intDEffSum (SumId "Add")
subV :: (CompDomain e StdVD) => Val e StdVD -> Val e StdVD
subV = Sum intDEffSum (SumId "Sub")

leqV :: (CompDomain e StdVD) => Val e StdVD
leqV = Sum intDOrdSum (SumId "LEQ") Unit
geqV :: (CompDomain e StdVD) => Val e StdVD
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
