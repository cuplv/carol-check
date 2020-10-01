{-# LANGUAGE LambdaCase #-}

module Language.Carol.AST.Terms 
  ( Val (..)
  , boolV
  , Op (..)
  , opSig
  , Comp (..)
  , VarId (..)
  ) where

import Data.Map (Map)
import qualified Data.Map as M

import Language.Carol.AST.Types

newtype VarId = VarId String deriving (Show,Eq,Ord)

data Op = 
    Add
  | Sub
  | Neg
  | TestEq
  | TestLe
  | TestGe
  deriving (Show,Eq,Ord)

opSig :: Op -> ([ValT], ValT)
opSig = \case
  Add -> ([IntT,IntT], IntT)
  Sub -> ([IntT,IntT], IntT)
  Neg -> ([IntT], IntT)
  TestEq -> ([IntT,IntT], boolT)
  TestLe -> ([IntT,IntT], boolT)
  TestGe -> ([IntT,IntT], boolT)

data Val =
    Var VarId
  | Thunk Comp
  | Sum (Map SumId ValT) SumId Val
  | Unit
  | IntConst Int
  | Pair Val Val
  | Anno Val ValT
  deriving (Show,Eq,Ord)

boolV :: Bool -> Val
boolV b = Sum boolSchema i Unit
  where i = if b
               then trueS
               else falseS

data Comp =
    Ret Val
  | Prod (Map ProdId Comp)
  | Fun (VarId, Comp)
  | Let Val (VarId, Comp)
  | Bind Comp (VarId, Comp)
  | Force Val
  | Pmp Val (VarId, VarId, Comp)
  | Pms Val (Map SumId (VarId, Comp))
  | Proj ProdId Comp
  | Ap Val Comp
  | Pute [Val] Op (VarId, Comp)
  deriving (Show,Eq,Ord)
