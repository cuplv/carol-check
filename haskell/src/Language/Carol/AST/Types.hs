{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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

class (Show (DType d), Eq (DType d), Ord (DType d)) 
      => TypeDomain d where
  type DType d

newtype SumId = SumId String deriving (Show,Eq,Ord)

newtype ProdId = ProdId String deriving (Show,Eq,Ord)

data (TypeDomain d) => ValT d =
    ThunkT (CompT d)
  | SumT (Map SumId (ValT d))
  | UnitT
  | IntT
  | PairT (ValT d) (ValT d)
  | DsT (DType d)
  | ExVT ExIdV
  deriving (Eq,Ord)
  
instance (TypeDomain d) => Show (ValT d) where
  show = \case
    ThunkT mt -> "U(" ++ show mt ++ ")"
    SumT mp | mp == boolSchema -> "Bool"
    SumT mp -> "Σ(" ++ show mp ++ ")"
    UnitT -> "{}"
    IntT -> "Int"
    PairT vt1 vt2 -> "(" ++ show vt1 ++ ", " ++ show vt2 ++ ")"
    DsT t -> show t
    ExVT e -> show e

trueS = SumId "True"
falseS = SumId "False"

boolSchema = M.fromList [(trueS,UnitT), (falseS,UnitT)]

boolT :: (TypeDomain d) => ValT d
boolT = SumT boolSchema

data (TypeDomain d) => CompT d =
    RetT (ValT d)
  | ProdT (Map ProdId (CompT d))
  | FunT (ValT d) (CompT d)
  | ExCT ExIdC
  deriving (Eq,Ord)

instance (TypeDomain d) => Show (CompT d) where
  show = \case
    RetT vt -> "F(" ++ show vt ++ ")"
    ProdT mp -> "Π(" ++ show mp ++ ")"
    FunT vt mt' -> show vt ++ " → " ++ show mt'
    ExCT e -> show e
