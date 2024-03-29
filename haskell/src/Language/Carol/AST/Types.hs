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
  -- , baseTypeC
  , subiC
  , RefDomain (..)
  , Refinement (..)
  , IVarId
  , funT
  , funTR
  , fori
  , addEqRef
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

subiV :: (RefDomain d) => IVarId -> IVarId -> ValT d -> ValT d
subiV x y = \case
  ThunkT m -> ThunkT $ subiC x y m
  SumT mp -> undefined
  PairT vt1 vt2 -> PairT (subiV x y vt1) (subiV x y vt2)
  DsT t r -> DsT t (subiR x y r)
  vt -> vt

baseTypeV :: (RefDomain d) => ValT d -> ValT d
baseTypeV = \case
  ThunkT mt -> ThunkT $ baseTypeC mt
  SumT mp -> SumT $ M.map baseTypeV mp
  UnitT -> UnitT
  PairT vt1 vt2 -> PairT (baseTypeV vt1) (baseTypeV vt2)
  DsT t r -> DsT t RefTrue -- remove refinement
  ExVT e -> ExVT e

addEqRef :: (RefDomain d) => IVarId -> ValT d -> ValT d
addEqRef a = \case
  DsT t r -> DsT t $ RefAnd r (eqRef a)
  t -> t

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
  | FunT (Maybe VarId) (ValT d) (CompT d)
  | Idx IVarId (ISort d) (CompT d)
  | ExCT ExIdC

deriving instance (Eq d, RefDomain d, Eq (ISort d)) => Eq (CompT d)
deriving instance (Ord d, RefDomain d, Ord (ISort d)) => Ord (CompT d)

subiC :: (RefDomain d) => IVarId -> IVarId -> CompT d -> CompT d
subiC x y = \case
  RetT vt -> RetT $ subiV x y vt
  ProdT mp -> undefined
  -- FunT (Just (VarId x')) vt mt | x == IVarId x' -> 
  --                                FunT (Just (VarId x')) vt mt
  FunT mx vt mt -> FunT mx (subiV x y vt) (subiC x y mt)
  mt -> mt

funT = FunT Nothing

funTR x = FunT (Just x)

fori n s c = Idx (IVarId n) s c

deriving instance (Show d, RefDomain d, Show (DRef d), Show (ISort d))
    => Show (CompT d)

instance (RefDomain d, Pretty d, Pretty (DRef d), Pretty (ISort d))
    => Pretty (CompT d) where
  pretty = \case
    RetT vt -> "F(" ++ pretty vt ++ ")"
    ProdT mp -> "Π(" ++ "..." ++ ")"
    FunT (Just x) vt mt' -> "(" ++ pretty x ++ ":" ++ pretty vt ++ ")" 
                            ++ " → " ++ pretty mt'
    FunT Nothing vt mt' -> pretty vt ++ " → " ++ pretty mt'
    ExCT e -> pretty e
    Idx a s mt' -> "π " ++ pretty a ++ ":" ++ pretty s ++ ". " ++ pretty mt'


baseTypeC :: (RefDomain d) => CompT d -> CompT d
baseTypeC = \case
  RetT vt -> RetT $ baseTypeV vt
  ProdT mp -> ProdT $ M.map baseTypeC mp
  FunT _ vt mt -> FunT Nothing (baseTypeV vt) (baseTypeC mt)
  ExCT e -> ExCT e
