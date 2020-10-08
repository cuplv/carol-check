{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Carol.AST.Terms 
  ( Val (..)
  , ValDomain (..)
  , boolV
  , Comp (..)
  , CompDomain (..)
  , VarId (..)
  ) where

import Data.Map (Map)
import qualified Data.Map as M

import Language.Carol.AST.Types

class (Show d, Show (DVal d), Eq d, Eq (DVal d), Ord d, Ord (DVal d)) => ValDomain d where
  data DVal d
  dValType :: DVal d -> d

class (ValDomain d) => CompDomain e d where
  dCompSig :: e -> ([ValT d], ValT d)
  dCompShow :: e -> [Val e d] -> String

newtype VarId = VarId String deriving (Eq,Ord)

instance Show VarId where
  show (VarId s) = s

data (CompDomain e d) => Val e d =
    Var VarId
  | Thunk (Comp e d)
  | Sum (Map SumId (ValT d)) SumId (Val e d)
  | Unit
  | Pair (Val e d) (Val e d)
  | DsV (DVal d)
  | Anno (Val e d) (ValT d)
  deriving (Eq,Ord)

instance (CompDomain e d) => Show (Val e d) where
  show = \case
    Var i -> show i
    Thunk m -> "thunk " ++ show m
    Sum mp (SumId s) v -> s ++ "(" ++ show v ++ ")"
    Unit -> "{=}"
    Pair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
    Anno v vt -> show v ++ " : " ++ show vt

boolV :: (CompDomain e d) => Bool -> Val e d
boolV b = Sum boolSchema i Unit
  where i = if b
               then trueS
               else falseS

type Abst e d = (VarId, Comp e d)

data (CompDomain e d) => Comp e d =
    Ret (Val e d)
  | Prod (Map ProdId (Comp e d))
  | Fun (Abst e d)
  | Let (Val e d) (Abst e d)
  | Bind (Comp e d) (Abst e d)
  | Force (Val e d)
  | Pmp (Val e d) (VarId, VarId, Comp e d)
  | Pms (Val e d) (Map SumId (Abst e d))
  | Proj ProdId (Comp e d)
  | Ap (Val e d) (Comp e d)
  | DsC e [Val e d] (Maybe VarId, Comp e d)
  deriving (Eq,Ord)

sha :: (Show a, Show b) => (a,b) -> String
sha (x,m') = show x ++ "| " ++ show m'

instance (CompDomain e d) => Show (Comp e d) where
  show = \case
    Ret v -> "return " ++ show v
    Prod pm -> "{" ++ show pm ++ "}"
    Fun xm -> "|" ++ sha xm
    Let v xm -> "let " ++ show v ++ " be " ++ sha xm
    Bind m1 xm -> show m1 ++ " to " ++ sha xm
    Pmp v (x,y,m') -> "pm " ++ show v ++ " as " ++ show (x,y) ++ "| " ++ show m'
    Pms v mp -> "pm " ++ show v ++ " as {" ++ show mp ++ "}"
    Proj i m' -> show i ++ "`" ++ show m'
    Ap v m' -> show v ++ "`" ++ show m'
    DsC e vs (Just x,m') -> dCompShow e vs ++ " as " ++ sha (x,m')
    DsC e vs (Nothing,m') -> dCompShow e vs ++ " |" ++ show m'
