{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.AST.Types
  ( ValT (..)
  , boolT
  , boolSchema
  , trueS
  , falseS
  , CompT (..)
  , SumId (..)
  , ProdId (..)
  , ExIdV
  , ExIdC
  , RefDomain (..)
  , Refinement (..)
  ) where

import Language.Carol.AST.PrettyPrint
import Language.Carol.AST.Types.ExVars (ExIdV,ExIdC)

import Data.Map (Map)
import qualified Data.Map as M

newtype SumId = SumId String deriving (Show,Eq,Ord)

newtype ProdId = ProdId String deriving (Show,Eq,Ord)

class (Eq d, Eq (DRef d), Ord d, Ord (DRef d))
    => RefDomain d where
  data DRef d

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

data (RefDomain d) => ValT d =
    ThunkT (CompT d)
  | SumT (Map SumId (ValT d))
  | UnitT
  | PairT (ValT d) (ValT d)
  | DsT d (Refinement d)
  | ExVT ExIdV
  deriving (Eq,Ord)

deriving instance (Show d, RefDomain d, Show (DRef d))
    => Show (ValT d)

instance (RefDomain d, Pretty d, Pretty (DRef d))
    => Pretty (ValT d) where
  pretty = \case
    ThunkT mt -> "U(" ++ pretty mt ++ ")"
    SumT mp -> "Σ(" ++ "..." ++ ")"
    UnitT -> "{}"
    PairT vt1 vt2 -> "(" ++ pretty vt1 ++ ", " ++ pretty vt2 ++ ")"
    DsT t RefTrue -> pretty t
    DsT t r -> "{ ν:" ++ pretty t ++ " | " ++ pretty r ++ " }"
    ExVT e -> pretty e

trueS = SumId "True"
falseS = SumId "False"

boolSchema = M.fromList [(trueS,UnitT), (falseS,UnitT)]

boolT :: (RefDomain d) => ValT d
boolT = SumT boolSchema

data CompT d =
    RetT (ValT d)
  | ProdT (Map ProdId (CompT d))
  | FunT (ValT d) (CompT d)
  | ExCT ExIdC
  deriving (Eq,Ord)

deriving instance (Show d, RefDomain d, Show (DRef d))
    => Show (CompT d)

instance (RefDomain d, Pretty d, Pretty (DRef d))
    => Pretty (CompT d) where
  pretty = \case
    RetT vt -> "F(" ++ pretty vt ++ ")"
    ProdT mp -> "Π(" ++ "..." ++ ")"
    FunT vt mt' -> pretty vt ++ " → " ++ pretty mt'
    ExCT e -> pretty e
