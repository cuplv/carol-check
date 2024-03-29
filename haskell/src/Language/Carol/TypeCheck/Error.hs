{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.TypeCheck.Error
  ( TErr
  , runTErr
  , terr
  , TypeError (..)
  ) where

import Control.Monad.Except

import Language.Carol.AST.Types
import Language.Carol.Prelude.Types

type TErr d = ExceptT (TypeError d) IO

terr :: (RefDomain d) => TypeError d -> TErr d a
terr = throwError

data TypeError d =
    TMismatch (ValT d) (ValT d)
  | TCMismatch (CompT d) (CompT d)
  | TOther String
  deriving (Eq,Ord)

instance (RefDomain d, Pretty d, Pretty (DRef d), Pretty (ISort d))
    => Pretty (TypeError d) where
  pretty (TMismatch vt1 vt2) = 
    pretty vt1 ++ " does not satisfy " ++ pretty vt2
  pretty (TCMismatch mt1 mt2) = 
    pretty mt1 ++ " does not satisfy " ++ pretty mt2
  pretty (TOther s) = "Error: " ++ show s

runTErr :: TErr d a -> IO (Either (TypeError d) a)
runTErr = runExceptT
