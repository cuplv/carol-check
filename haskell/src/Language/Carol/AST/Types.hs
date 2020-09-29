{-# LANGUAGE TypeFamilies #-}

module Language.Carol.AST.Types where

import Data.Map (Map)
import qualified Data.Map as M

data ValT d c a =
    ThunkT c
  | SumT (Map String a)
  | UnitT
  | PairT a a
  | DSV d
  deriving (Show,Eq,Ord)

data Fix f = Fix (f (Fix f))

data ValTC d c = ValTC (Fix (ValT d c))

test :: ValTC d c
test = ValTC $ Fix $ PairT (Fix (PairT (Fix UnitT) (Fix UnitT))) (Fix UnitT)

data ExT vt a = Ex String | Conc (vt a)

data ValTE d c = ValTE (Fix (ExT (ValT d c)))

data CompT vt a =
    RetT (vt a)
  | ProdT (Map String a)
  | FunT (vt a) a
  deriving (Show,Eq,Ord)

type CompTC d = Fix (CompT (ValTC d))

type CompTE d = Fix (CompT (ValTE d))
