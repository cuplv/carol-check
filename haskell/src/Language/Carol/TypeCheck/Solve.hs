module Language.Carol.TypeCheck.Solve where

import Language.Carol.AST.Refinement
import Language.Carol.AST.Types
import Language.Carol.TypeCheck.Context
import Language.Carol.TypeCheck.Error

class (RefDomain d) => RefSolve d where
  subRef :: (ValT d, DRef d) -> (ValT d, DRef d) -> Context d -> TErr d (Context d)
