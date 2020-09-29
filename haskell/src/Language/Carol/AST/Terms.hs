module Language.Carol.AST.Terms 
  ( Val (..)
  , Comp (..)
  , VarId (..)
  ) where

import Data.Map (Map)
import qualified Data.Map as M

import Language.Carol.AST.Types

newtype VarId = VarId String deriving (Show,Eq,Ord)

data Val =
    Var VarId
  | Thunk Comp
  | Sum (Map SumId Val)
  | Unit
  | Pair Val Val
  | Anno Val ValT
  deriving (Show,Eq,Ord)

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
  deriving (Show,Eq,Ord)
