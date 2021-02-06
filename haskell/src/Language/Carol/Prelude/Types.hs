module Language.Carol.Prelude.Types
  ( Pretty (..)
  , VarId (..)
  , IVarId (..)
  ) where

class Pretty d where
  pretty :: d -> String

newtype VarId = VarId String deriving (Eq,Ord)
instance Show VarId where
  show (VarId s) = s
instance Pretty VarId where
  pretty = show

newtype IVarId = IVarId String deriving (Eq,Ord)
instance Show IVarId where
  show (IVarId s) = s
instance Pretty IVarId where
  pretty (IVarId s) = s
