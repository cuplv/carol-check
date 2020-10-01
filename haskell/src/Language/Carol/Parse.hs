module Language.Carol.Parse where

import Language.Carol.AST

import Text.Parsec

compP :: Parsec String () Comp
compP = 
  returnP
  <|> funP

returnP = do
  string "return" >> spaces
  v <- valP
  return (Ret v)

funP = do
  string "!" >> spaces
  c0 <- letter
  cs <- many alphaNum
  m <- compP
  return (Fun (VarId (c0:cs), m))

valP = do
  string "()"
  return Unit
