module Language.Carol.AST.Types 
  ( ValT (..)
  , boolT
  , CompT (..)
  , SumId (..)
  , ProdId (..)
  , ExTypeId (..)
  ) where

import Data.Map (Map)
import qualified Data.Map as M

newtype SumId = SumId String deriving (Show,Eq,Ord)

newtype ProdId = ProdId String deriving (Show,Eq,Ord)

newtype ExTypeId = ExTypeId Int deriving (Show,Eq,Ord)

data ValT =
    ThunkT CompT
  | SumT (Map SumId ValT)
  | UnitT
  | IntT
  | PairT ValT ValT
  | ExVar ExTypeId
  deriving (Show,Eq,Ord)

trueS = SumId "True"
falseS = SumId "False"

boolT :: ValT
boolT = SumT (M.fromList [(trueS,UnitT), (falseS,UnitT)])

test :: ValT
test = PairT (PairT UnitT UnitT) UnitT

data CompT =
    RetT ValT
  | ProdT (Map ProdId CompT)
  | FunT ValT CompT
  deriving (Show,Eq,Ord)
