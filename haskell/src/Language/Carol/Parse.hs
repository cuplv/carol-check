module Language.Carol.Parse 
  ( parseComp
  , compP
  , valP
  , mtypeP
  , vtypeP
  , parse
  , ParsecT
  , Parsec
  , ParseError
  ) where

import Language.Carol.AST

import Text.Parsec

keywordP :: String -> Parsec String s ()
keywordP w = string w >> space >> return ()

mtypeP :: Parsec String s CompT
mtypeP = 
  retTP
  <|> funTP

retTP = RetT <$> (char 'F' >> spaces >> vtypeP)
funTP = do
  vt <- vtypeP
  spaces
  string "->"
  spaces
  mt <- mtypeP
  return (FunT vt mt)

vtypeP :: Parsec String s ValT
vtypeP = 
  unitTP
  <|> intTP
  <|> boolTP
  <|> pairTP

unitTP = string "{}" >> return UnitT
intTP = string "Int" >> return IntT
boolTP = string "Bool" >> return boolT
pairTP = do
  at <- vtypeP
  spaces
  char '*'
  spaces
  bt <- vtypeP
  return (PairT at bt)

parseComp :: String -> Either ParseError Comp'
parseComp = parse compP ""

compP :: Parsec String s Comp'
compP = choice $ map try [retP,funP,apP,parensP compP]

parensP :: Parsec String s a -> Parsec String s a
parensP = between (char '(' >> spaces) (spaces >> char ')')

retP = do
  string "return" >> space >> spaces
  v <- valP
  return (Ret v)

varIdentP :: Parsec String s VarId
varIdentP = VarId <$> ((:) <$> letter <*> many alphaNum)

funP = do
  char '?'
  spaces
  x <- varIdentP
  spaces
  char '.'
  spaces
  m <- compP
  return $ Fun (x,m)

apP = do
  v <- valP
  spaces >> char '`' >> spaces
  m <- compP
  return (Ap v m)

valP :: (Domain d) => Parsec String s (Val d)
valP = do
  v <- choice $ map try
         [varP
         ,unitP
         ,intP
         ,pairP
         ,trueP
         ,falseP
         ,parensP valP]
  mvt <- optionMaybe (try annoEndP)
  case mvt of
    Just vt -> return $ Anno v vt
    Nothing -> return v

annoEndP = spaces >> char ':' >> spaces >> vtypeP

varP = Var <$> varIdentP
unitP = string "{=}" >> return Unit

intP :: (Domain d) => Parsec String s (Val d)
intP = (IntConst . read) <$> many1 digit

pairP :: (Domain d) => Parsec String s (Val d)
pairP = parensP $ do
  a <- valP
  spaces >> char ',' >> spaces
  b <- valP
  return (Pair a b)

trueP :: (Domain d) => Parsec String s (Val d)
trueP = string "True" >> return (boolV True)

falseP :: (Domain d) => Parsec String s (Val d)
falseP = string "False" >> return (boolV False)
