{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

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
  ) where

import Language.Carol.AST.Types.ExVars (ExIdV,ExIdC)

import Data.Map (Map)
import qualified Data.Map as M

newtype SumId = SumId String deriving (Show,Eq,Ord)

newtype ProdId = ProdId String deriving (Show,Eq,Ord)

class RefDomain d where
  data DRef d

data (RefDomain d) => Refinement d =
    RefAtom (DRef d)
  | RefAnd (Refinement d) (Refinement d)
  deriving (Eq,Ord)

data (RefDomain d) => ValT d =
    ThunkT (CompT d)
  | SumT (Map SumId (ValT d))
  | UnitT
  | PairT (ValT d) (ValT d)
  | DsT d (Refinement d)
  | ExVT ExIdV
  deriving (Eq,Ord)

instance (Show d, Eq d) => Show (ValT d) where
  show = \case
    ThunkT mt -> "U(" ++ show mt ++ ")"
    SumT mp | mp == boolSchema -> "Bool"
    SumT mp -> "Σ(" ++ show mp ++ ")"
    UnitT -> "{}"
    PairT vt1 vt2 -> "(" ++ show vt1 ++ ", " ++ show vt2 ++ ")"
    DsT t -> show t
    ExVT e -> show e

trueS = SumId "True"
falseS = SumId "False"

boolSchema = M.fromList [(trueS,UnitT), (falseS,UnitT)]

boolT :: ValT d
boolT = SumT boolSchema

data CompT d =
    RetT (ValT d)
  | ProdT (Map ProdId (CompT d))
  | FunT (ValT d) (CompT d)
  | ExCT ExIdC
  deriving (Eq,Ord)

instance (Show d, Eq d) => Show (CompT d) where
  show = \case
    RetT vt -> "F(" ++ show vt ++ ")"
    ProdT mp -> "Π(" ++ show mp ++ ")"
    FunT vt mt' -> show vt ++ " → " ++ show mt'
    ExCT e -> show e
