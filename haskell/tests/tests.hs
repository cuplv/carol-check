module Main where

import Language.Carol.AST
import Language.Carol.Parse
import Language.Carol.TypeCheck

import System.Exit

pComp :: String -> IO Comp'
pComp s = case parseComp s of
            Right m -> return m
            Left e -> die (show e)

main = do
  prog <- pComp
            "  +1`          |a|  \
            \  5`           |x|  \
            \  mod a <- x as y|  \
            \  return y          "
  case synthC prog emptyContext of
    Right (mt,g) -> case substC g mt of
      Right (RetT IntT) -> return ()
      Right mt -> die (show mt)
      Left e -> die e
    Left e -> die e
