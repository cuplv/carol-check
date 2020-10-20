{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.TypeCheck.Error
  ( TErr
  , terr
  , TypeError (..)
  ) where

import Language.Carol.AST.PrettyPrint
import Language.Carol.AST.Types

type TErr d = Either (TypeError d)

terr :: (RefDomain d) => TypeError d -> TErr d a
terr = Left

data TypeError d =
    TMismatch (ValT d) (ValT d)
  | TOther String
  deriving (Eq,Ord)

instance (RefDomain d, Pretty d, Pretty (DRef d))
    => Pretty (TypeError d) where
  pretty (TMismatch vt1 vt2) = 
    pretty vt2 ++ " does not satisfy " ++ pretty vt1
  pretty (TOther s) = "Error: " ++ show s
