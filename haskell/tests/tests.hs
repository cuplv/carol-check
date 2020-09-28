module Main where

import Language.Carol.AST
import Language.Carol.TypeCheck

import System.Exit

fun :: Comp EmptyD
fun = Fun (VarId "x") (Ret (Var (VarId "x")))

prog :: Comp EmptyD
prog = Ap Unit fun

main = do
  case synthC emptyContext prog of
    Right (mt,g) -> case substC g mt of
      Right (RetT UnitT) -> return ()
      Right mt -> die (show mt)
      Left e -> die e
    Left e -> die e
