{-# LANGUAGE LambdaCase #-}

module Language.Carol.AST.Terms 
  ( Val (..)
  , boolV
  , Comp (..)
  , Comp'
  , domain'
  , VarId (..)
  , Domain (..)
  , EmptyD
  , IntD (..)
  , addV
  , subV
  , leqV
  , geqV
  ) where

import Data.Map (Map)
import qualified Data.Map as M

import Language.Carol.AST.Types

class (Show d, Eq d, Ord d) => Domain d where
  domStateType :: d -> ValT
  domModType :: d -> ValT
  domModDef :: d -> Val d -> Val d -> Val d
  domOrderType :: d -> ValT
  domOrderDef :: d -> Val d -> (Val d,Val d) -> Bool
  domOrderFp :: d -> Val d -> Val d -> Bool

data EmptyD

instance Show EmptyD where
  show = undefined
instance Eq EmptyD where
  a == b = undefined
instance Ord EmptyD where
  a <= b = undefined
instance Domain EmptyD where
  domStateType = undefined
  domModType = undefined
  domModDef = undefined
  domOrderType = undefined
  domOrderDef = undefined
  domOrderFp = undefined

data IntD = IntD deriving (Show,Eq,Ord)

intDEffSum = M.fromList 
  [(SumId "Add", IntT)
  ,(SumId "Sub", IntT)]

intDOrdSum = M.fromList
  [(SumId "LEQ", UnitT)
  ,(SumId "GEQ", UnitT)]

instance Domain IntD where
  domStateType _ = IntT
  domModType _ = SumT intDEffSum
  domModDef _ (Sum _ (SumId "Add") (IntConst b)) (IntConst a) =
    IntConst (a + b)
  domModDef _ (Sum _ (SumId "Sub") (IntConst b)) (IntConst a) =
    IntConst (a - b)
  domOrderType _ = SumT intDOrdSum
  domOrderDef _ (Sum _ (SumId "LEQ") _) (IntConst a, IntConst b) =
    a <= b
  domOrderDef _ (Sum _ (SumId "GEQ") _) (IntConst a, IntConst b) =
    a >= b
  domOrderFp _ (Sum _ (SumId "LEQ") _) (Sum _ (SumId "Add") (IntConst n)) =
    n >= 0
  domOrderFp _ (Sum _ (SumId "GEQ") _) (Sum _ (SumId "Sub") (IntConst n)) =
    n >= 0

addV :: (Domain d) => Val d -> Val d
addV = Sum intDEffSum (SumId "Add")
subV :: (Domain d) => Val d -> Val d
subV = Sum intDEffSum (SumId "Sub")

leqV :: (Domain d) => Val d
leqV = Sum intDOrdSum (SumId "LEQ") Unit
geqV :: (Domain d) => Val d
geqV = Sum intDOrdSum (SumId "GEQ") Unit

newtype VarId = VarId String deriving (Show,Eq,Ord)

data (Domain d) => Val d =
    Var VarId
  | Thunk (Comp d)
  | Sum (Map SumId ValT) SumId (Val d)
  | Unit
  | IntConst Int
  | Pair (Val d) (Val d)
  | Anno (Val d) ValT
  deriving (Show,Eq,Ord)

boolV :: (Domain d) => Bool -> Val d
boolV b = Sum boolSchema i Unit
  where i = if b
               then trueS
               else falseS

type Abst d = (VarId, Comp d)

data (Domain d) => Comp d =
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
  | DMod d (Val d) (Val d) (Abst d)
  | DTest d (Val d) (Val d, Val d) (Abst d)
  deriving (Show,Eq,Ord)

type Comp' = Comp IntD
domain' = IntD
