module Language.Carol.AST.Domain
  ( Domain (..)
  ) where

import Language.Carol.AST.Terms
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
