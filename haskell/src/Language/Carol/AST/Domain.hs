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
  , intTEqAdd
  , intV
  , intSort
  , StdCD (..)
  , Val'
  , ValT'
  , Comp'
  , CompT'
  ) where

import Language.Carol.AST.Refinement (AnySym (..))
import Language.Carol.AST.Terms
import Language.Carol.AST.Types
import Language.Carol.Prelude.Types

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
  dCompSigR = undefined
  dCompPretty = undefined

data StdVD = IntT deriving (Eq,Ord)

instance Show StdVD where
  show IntT = "Int"

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
  data DRSym StdVD = IntSym SInteger
  mkSym s IntT = RSym . IntSym <$> forall s
  refConstraint m = \case
    LEQ o -> do s <- mkobj o
                return (\(RSym (IntSym v)) -> v .<= s)
    GEQ o -> do s <- mkobj o
                return (\(RSym (IntSym v)) -> v .>= s)
    where mkobj = \case
            Literal i -> Right $ literal (fromIntegral i)
            IntVar a -> case M.lookup a m of
                          Just (RSym (IntSym x),_) -> Right x
                          _ -> Left $ "No context for " ++ pretty a ++ " in " ++ show (pretty <$> M.keys m)
            IntAddObj o1 o2 -> do a <- mkobj o1 
                                  b <- mkobj o2
                                  return (a + b)
  subVar a (LEQ (IntVar a')) = if a == a'
                               then LEQ (IntVar a)
                               else LEQ (IntVar a')
  subVar _ (LEQ n) = GEQ n
  subVar a (GEQ (IntVar a')) = if a == a'
                               then GEQ (IntVar a)
                               else GEQ (IntVar a')
  subVar _ (GEQ n) = GEQ n
  eqRef a = RefAnd (RefAtom $ GEQ (IntVar a))
                   (RefAtom $ LEQ (IntVar a))
  subiDR x y = \case
    LEQ o -> LEQ $ subiO x y o
    GEQ o -> GEQ $ subiO x y o
    where subiO x y = \case
            IntVar x' | x == x' -> IntVar y
            IntVar x' -> IntVar x'
            IntAddObj o1 o2 -> IntAddObj (subiO x y o1) (subiO x y o2)
            Literal n -> Literal n

intSort = IntS

instance Pretty (DRef StdVD) where
  pretty (LEQ n) = "ν ≤ " ++ pretty n
  pretty (GEQ n) = "ν ≥ " ++ pretty n

instance Pretty (ISort StdVD) where
  pretty IntS = "Int"

instance ValDomain StdVD where
  data DVal StdVD = IntConst Int deriving (Show,Eq,Ord)
  dValType (IntConst n) = (IntT, RefAnd 
                                   (RefAtom $ GEQ (Literal n)) 
                                   (RefAtom $ LEQ (Literal n)))

instance Pretty (DVal StdVD) where
  pretty (IntConst n) = show n

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

intTEqAdd :: Int -> Int -> ValT StdVD
intTEqAdd x y = DsT IntT (RefAnd (RefAtom $ GEQ (IntAddObj (Literal x) (Literal y)))
                                 (RefAtom $ LEQ (IntAddObj (Literal x) (Literal y))))

intV :: (CompDomain e StdVD) => Int -> Val e StdVD
intV = DsV . IntConst

data StdCD =
    IntAdd
  | IntMul
  | IntInc
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
  dCompOutput = \case
    IntAdd -> Just intT
    IntInc -> Just intT
  dCompInputs = \case
    IntAdd -> \(Just (x,vt)) -> [(IVarId "n1",intT), (IVarId "n2",undefined)]
    IntInc -> \(Just (x,vt)) -> undefined
  dCompSigR = \case
    IntAdd -> ([(IVarId "n1", intSort), (IVarId "n2", intSort)]
              ,PairT (intTEq (IVarId "n1")) (intTEq (IVarId "n2"))
              ,DsT IntT (RefAnd (RefAtom $ GEQ (IntAddObj 
                                                  (IntVar$ IVarId "n1")
                                                  (IntVar$ IVarId "n2")))
                                (RefAtom $ LEQ (IntAddObj 
                                                  (IntVar$ IVarId "n1")
                                                  (IntVar$ IVarId "n2")))))
    IntInc -> ([(IVarId "n1", intSort)]
              ,intT
              ,DsT IntT (RefAnd (RefAtom $ GEQ (IntAddObj (IntVar$ IVarId "n1")
                                                          (Literal 1)))
                                (RefAtom $ LEQ (IntAddObj (IntVar$ IVarId "n1")
                                                          (Literal 1)))))

  -- dCompPretty IntMod vs = "mod " ++ lsPretty vs
  -- dCompPretty IntTest vs = "test " ++ lsPretty vs
  dCompPretty _ _ = "[thing]"

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

type Comp' = Comp StdCD StdVD

type CompT' = CompT StdVD
