{-# LANGUAGE LambdaCase #-}

module Language.Carol.AST.Types 
  ( ValT (..)
  , boolT
  , boolSchema
  , trueS
  , falseS
  , CompT (..)
  , SumId (..)
  , ProdId (..)
  , ExTypeId
  , exTypeIdInit
  , exTypeIdSub
  , exTypeIdNext
  ) where

import Language.Carol.AST.Types.Existential

import Data.Map (Map)
import qualified Data.Map as M

newtype SumId = SumId String deriving (Show,Eq,Ord)

newtype ProdId = ProdId String deriving (Show,Eq,Ord)

data ValT =
    ThunkT CompT
  | SumT (Map SumId ValT)
  | UnitT
  | IntT
  | PairT ValT ValT
  | ExVar ExTypeId
  deriving (Eq,Ord)
  
instance Show ValT where
  show = \case
    ThunkT mt -> "U(" ++ show mt ++ ")"
    SumT mp | mp == boolSchema -> "Bool"
    SumT mp -> "Σ(" ++ show mp ++ ")"
    UnitT -> "{}"
    IntT -> "Int"
    PairT vt1 vt2 -> "(" ++ show vt1 ++ ", " ++ show vt2 ++ ")"
    ExVar e -> "<" ++ show e ++ ">"

trueS = SumId "True"
falseS = SumId "False"

boolSchema = M.fromList [(trueS,UnitT), (falseS,UnitT)]

boolT :: ValT
boolT = SumT boolSchema

data CompT =
    RetT ValT
  | ProdT (Map ProdId CompT)
  | FunT ValT CompT
  deriving (Eq,Ord)

instance Show CompT where
  show = \case
    RetT vt -> "F(" ++ show vt ++ ")"
    ProdT mp -> "Π(" ++ show mp ++ ")"
    FunT vt mt' -> show vt ++ " → " ++ show mt'
