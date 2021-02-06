{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.AST.Types
  ( ValT (..)
  , unitT
  , unit1T
  , unit2T
  , unit3T
  , boolT
  , boolSchema
  , trueS
  , falseS
  , CompT (..)
  , SumId (..)
  , ProdId (..)
  , ExIdV
  , ExIdC
  , baseTypeV
  , baseTypeC
  , RefDomain (..)
  , Refinement (..)
  , IVarId
  , fori
  ) where

import Language.Carol.AST.Refinement
import Language.Carol.AST.Types.ExVars (ExIdV,ExIdC)
import Language.Carol.Prelude.Types

import Data.Map (Map)
import qualified Data.Map as M

newtype SumId = SumId String deriving (Show,Eq,Ord)

newtype ProdId = ProdId String deriving (Show,Eq,Ord)

data (RefDomain d) => ValT d =
    ThunkT (CompT d)
  | SumT (Map SumId (ValT d))
  | UnitT
  | PairT (ValT d) (ValT d)
  | DsT d (Refinement d)
  | ExVT ExIdV
  deriving (Eq,Ord)

deriving instance (Show d, RefDomain d, Show (DRef d), Show (ISort d))
    => Show (ValT d)

instance (RefDomain d, Pretty d, Pretty (DRef d), Pretty (ISort d))
    => Pretty (ValT d) where
  pretty = \case
    ThunkT mt -> "U(" ++ pretty mt ++ ")"
    SumT mp -> "Σ(" ++ "..." ++ ")"
    UnitT -> "{}"
    PairT vt1 vt2 -> "(" ++ pretty vt1 ++ ", " ++ pretty vt2 ++ ")"
    DsT t RefTrue -> pretty t
    DsT t r -> "{ ν:" ++ pretty t ++ " | " ++ pretty r ++ " }"
    ExVT e -> pretty e

baseTypeV :: (RefDomain d) => ValT d -> ValT d
baseTypeV = \case
  ThunkT mt -> ThunkT $ baseTypeC mt
  SumT mp -> SumT $ M.map baseTypeV mp
  UnitT -> UnitT
  PairT vt1 vt2 -> PairT (baseTypeV vt1) (baseTypeV vt2)
  DsT t r -> DsT t RefTrue -- remove refinement
  ExVT e -> ExVT e

unitT :: (RefDomain d) => ValT d
unitT = UnitT

unit1T :: (RefDomain d) => ValT d
unit1T = unitT

unit2T :: (RefDomain d) => ValT d
unit2T = PairT unitT unit1T

unit3T :: (RefDomain d) => ValT d
unit3T = PairT unitT unit2T

trueS = SumId "True"
falseS = SumId "False"

boolSchema = M.fromList [(trueS,UnitT), (falseS,UnitT)]

boolT :: (RefDomain d) => ValT d
boolT = SumT boolSchema

data CompT d =
    RetT (ValT d)
  | ProdT (Map ProdId (CompT d))
  | FunT (ValT d) (CompT d)
  | Idx IVarId (ISort d) (CompT d)
  | ExCT ExIdC

deriving instance (Eq d, RefDomain d, Eq (ISort d)) => Eq (CompT d)
deriving instance (Ord d, RefDomain d, Ord (ISort d)) => Ord (CompT d)

fori n s c = Idx (IVarId n) s c

deriving instance (Show d, RefDomain d, Show (DRef d), Show (ISort d))
    => Show (CompT d)

instance (RefDomain d, Pretty d, Pretty (DRef d), Pretty (ISort d))
    => Pretty (CompT d) where
  pretty = \case
    RetT vt -> "F(" ++ pretty vt ++ ")"
    ProdT mp -> "Π(" ++ "..." ++ ")"
    FunT vt mt' -> pretty vt ++ " → " ++ pretty mt'
    ExCT e -> pretty e
    Idx a s mt' -> "π " ++ pretty a ++ ":" ++ pretty s ++ ". " ++ pretty mt'


baseTypeC :: (RefDomain d) => CompT d -> CompT d
baseTypeC = \case
  RetT vt -> RetT $ baseTypeV vt
  ProdT mp -> ProdT $ M.map baseTypeC mp
  FunT vt mt -> FunT (baseTypeV vt) (baseTypeC mt)
  ExCT e -> ExCT e
