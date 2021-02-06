module Language.Carol.Prelude.PrettyPrint
  ( Pretty (..)
  ) where

class Pretty d where
  pretty :: d -> String
