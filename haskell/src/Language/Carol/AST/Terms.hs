{-# LANGUAGE LambdaCase #-}

module Language.Carol.AST.Terms 
  ( Val (..)
  , boolV
  , Comp (..)
  , Comp'
  , VarId (..)
  , EmptyD
  , IntD (..)
  , intModT
  , intOrdT
  , addV
  , subV
  , leqV
  , geqV
  ) where

import Data.Map (Map)
import qualified Data.Map as M

import Language.Carol.AST.Types

newtype VarId = VarId String deriving (Eq,Ord)

instance Show VarId where
  show (VarId s) = s

data (DomainLike d) => Val d =
    Var VarId
  | Thunk (Comp d)
  | Sum (Map SumId ValT) SumId (Val d)
  | Unit
  | IntConst Int
  | Pair (Val d) (Val d)
  | Anno (Val d) ValT
  deriving (Eq,Ord)

instance (DomainLike d) => Show (Val d) where
  show = \case
    Var i -> show i
    Thunk m -> "thunk " ++ show m
    Sum mp (SumId s) v -> s ++ "(" ++ show v ++ ")"
    Unit -> "{=}"
    IntConst i -> show i
    Pair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
    Anno v vt -> show v ++ " : " ++ show vt

boolV :: (DomainLike d) => Bool -> Val d
boolV b = Sum boolSchema i Unit
  where i = if b
               then trueS
               else falseS

type Abst d = (VarId, Comp d)

data (DomainLike d) => Comp d =
    Ret (Val d)
  | Prod (Map ProdId (Comp d))
  | Fun (Abst d)
  | Let (Val d) (Abst d)
  | Bind (Comp d) (Abst d)
  | Force (Val d)
  | Pmp (Val d) (VarId, VarId, Comp d)
  | Pms (Val d) (Map SumId (Abst d))
  | Proj ProdId (Comp d)
  | Ap (Val d) (Comp d)
  | DSC d [Val d] (Maybe VarId, Comp d)
  deriving (Show,Eq,Ord)

type Comp' = Comp IntD
