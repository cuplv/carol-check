{-# LANGUAGE LambdaCase #-}

module Language.Carol.AST.Terms 
  ( Val (..)
  , boolV
  , Comp (..)
  , Comp'
  , VarId (..)
  , Domain (..)
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

class (Show d, Eq d, Ord d) => Domain d where
  domSig :: d -> ([ValT], ValT)
  domShow :: d -> [Val d] -> (Abst d) -> String

data EmptyD

instance Show EmptyD where
  show = undefined
instance Eq EmptyD where
  a == b = undefined
instance Ord EmptyD where
  a <= b = undefined
instance Domain EmptyD where
  domSig = undefined
  domShow = undefined

data IntD =
    IntMod
  | IntTest
  | IntQuery
  | IntIssue
  | IntProduce
  | IntConsume
  deriving (Show,Eq,Ord)

intDEffSum = M.fromList 
  [(SumId "Add", IntT)
  ,(SumId "Sub", IntT)]

intModT = SumT intDEffSum

intDOrdSum = M.fromList
  [(SumId "LEQ", UnitT)
  ,(SumId "GEQ", UnitT)]

intOrdT = SumT intDOrdSum

instance Domain IntD where
  domSig IntMod = ([SumT intDEffSum, IntT], IntT)
  domSig IntTest = ([SumT intDOrdSum, IntT, IntT], boolT)
  domSig IntQuery = ([SumT intDOrdSum], IntT)
  domSig IntIssue = ([SumT intDEffSum], UnitT)
  domSig IntProduce = ([SumT intDEffSum], UnitT)
  domSig IntConsume = ([SumT intDEffSum], UnitT)

  domShow IntMod vs (x,m') = 
    "mod " ++ show vs ++ " as " ++ show x ++ "| " ++ show m'
  domShow IntTest vs (x,m') = 
    "test " ++ show vs ++ " as " ++ show x ++ "| " ++ show m'
  domShow _ _ (x,m') = "... " ++ show x ++ "| " ++ show m'

addV :: (Domain d) => Val d -> Val d
addV = Sum intDEffSum (SumId "Add")
subV :: (Domain d) => Val d -> Val d
subV = Sum intDEffSum (SumId "Sub")

leqV :: (Domain d) => Val d
leqV = Sum intDOrdSum (SumId "LEQ") Unit
geqV :: (Domain d) => Val d
geqV = Sum intDOrdSum (SumId "GEQ") Unit

newtype VarId = VarId String deriving (Eq,Ord)

instance Show VarId where
  show (VarId s) = s

data (Domain d) => Val d =
    Var VarId
  | Thunk (Comp d)
  | Sum (Map SumId ValT) SumId (Val d)
  | Unit
  | IntConst Int
  | Pair (Val d) (Val d)
  | Anno (Val d) ValT
  deriving (Eq,Ord)

instance (Domain d) => Show (Val d) where
  show = \case
    Var i -> show i
    Thunk m -> "thunk " ++ show m
    Sum mp (SumId s) v -> s ++ "(" ++ show v ++ ")"
    Unit -> "{=}"
    IntConst i -> show i
    Pair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
    Anno v vt -> show v ++ " : " ++ show vt

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
  | DSC d [Val d] (Maybe VarId, Comp d)
  deriving (Show,Eq,Ord)

type Comp' = Comp IntD
