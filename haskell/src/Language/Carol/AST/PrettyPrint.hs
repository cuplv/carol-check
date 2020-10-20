module Language.Carol.AST.PrettyPrint
  ( Pretty (..)
  ) where

class Pretty d where
  pretty :: d -> String
