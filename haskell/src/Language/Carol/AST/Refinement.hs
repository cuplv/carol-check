{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.AST.Refinement
  ( AnySym (..)
  , Refinement (..)
  , RefDomain (..)
  , subiR
  , rpred
  , IVarId
  ) where

import Data.SBV
import Data.Map (Map)

import Language.Carol.Prelude.Types

data (RefDomain d) => AnySym d = RSym (DRSym d)

class (Eq d, Eq (DRef d), Eq (ISort d), Ord d, Ord (DRef d), Ord (ISort d))
    => RefDomain d where
  data DRef d
  data ISort d
  data DRSym d
  refConstraint :: Map IVarId (AnySym d, Refinement d) -> DRef d -> Either String (AnySym d -> SBool)
  subVar :: IVarId -> DRef d -> DRef d
  mkSym :: String -> d -> SymbolicT IO (AnySym d)
  eqRef :: IVarId -> Refinement d
  subiDR :: IVarId -> IVarId -> DRef d -> DRef d

data (RefDomain d) => Refinement d =
    RefTrue
  | RefFalse
  | RefAtom (DRef d)
  | RefAnd (Refinement d) (Refinement d)
  deriving (Eq,Ord)

deriving instance (RefDomain d, Show (DRef d))
    => Show (Refinement d)

instance (RefDomain d, Pretty (DRef d))
    => Pretty (Refinement d) where
  pretty = \case
    RefTrue -> "⊤"
    RefFalse -> "⊥"
    RefAtom r -> pretty r
    RefAnd r1 r2 -> pretty r1 ++ " ∧ " ++ pretty r2

subiR :: (RefDomain d) => IVarId -> IVarId -> Refinement d -> Refinement d
subiR x y = \case
  RefAnd r1 r2 -> RefAnd (subiR x y r1) (subiR x y r2)
  RefAtom r -> RefAtom $ subiDR x y r

rpred :: (RefDomain d) => Map IVarId (AnySym d, Refinement d) -> Refinement d -> AnySym d -> Either String (SBool)
rpred m = \case
  RefTrue -> const (return sTrue)
  RefFalse -> const (return sFalse)
  RefAtom dr -> \nu -> refConstraint m dr <*> pure nu
  RefAnd r1 r2 -> \nu -> do p1 <- rpred m r1 nu
                            p2 <- rpred m r2 nu
                            return (p1 .&& p2)
