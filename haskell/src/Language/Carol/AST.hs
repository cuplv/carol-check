{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Carol.AST where

import Data.Map (Map)
import qualified Data.Map as M

data VarId = VarId String deriving (Show,Eq,Ord)

data SumId = SumId String deriving (Show,Eq,Ord)

data ProdId = ProdId String deriving (Show,Eq,Ord)

class (Show d, Eq d, Ord d) => Domain d where
  data DType d
  typeOf :: d -> DType d

data EmptyD = EmptyD deriving (Show,Eq,Ord)

instance Domain EmptyD where
  data DType EmptyD = EmptyDT deriving (Show,Eq,Ord)
  typeOf _ = EmptyDT

data (Domain d) => Val d = 
    Var VarId
  | Thunk (Comp d)
  | Sum (SumSchema d) SumId (Val d)
  | Unit
  | Prod (Val d) (Val d)
  | DSV d
  | Anno (Val d) (ValT d)
  deriving (Show,Eq,Ord)

boolV :: (Domain d) => Bool -> Val d
boolV b = Sum boolSchema s Unit
  where s = if b
               then trueS
               else falseS

data (Domain d) => ValT d =
    SumT (SumSchema d)
  | UnitT
  | ProdT (ValT d) (ValT d)
  | DSVT d
  deriving (Show,Eq,Ord)

data (Domain d) => Comp d = 
    Ret (Val d)
  | CProd (CProd d)
  | Fun VarId (Comp d)
  | Let (Val d) VarId (Comp d)
  | Bind (Comp d) VarId (Comp d)
  | Force (Val d)
  | Pm (Val d) (SumMerge d)
  | Proj ProdId (Comp d)
  | Ap (Val d) (Comp d)
  deriving (Show,Eq,Ord)

data CompT vt =
    RetT vt
  | FunT vt (CompT vt)
  deriving (Show,Eq,Ord)

type SumSchema d = Map SumId (ValT d)

trueS = SumId "True"
falseS = SumId "False"

boolSchema :: (Domain d) => SumSchema d
boolSchema = M.fromList [(trueS, UnitT), (falseS, UnitT)]

type SumMerge d = Map SumId (VarId, (Comp d))

type CProd d = Map ProdId (Comp d)
