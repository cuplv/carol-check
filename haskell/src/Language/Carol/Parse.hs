{-# LANGUAGE FlexibleContexts #-}

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
import Language.Carol.Prelude.Types

import Text.Parsec

keywordP :: String -> Parsec String s ()
keywordP w = string w >> space >> return ()

mtypeP :: Parsec String s (CompT')
mtypeP = 
  retTP
  <|> funTP

retTP :: Parsec String s (CompT')
retTP = RetT <$> (char 'F' >> spaces >> vtypeP)

funTP = do
  vt <- vtypeP
  spaces
  string "->"
  spaces
  mt <- mtypeP
  return (funT vt mt)

vtypeP :: Parsec String s (ValT')
vtypeP = 
  unitTP
  <|> unit1TP
  <|> unit2TP
  <|> unit3TP
  <|> intTP
  <|> boolTP
  <|> pairTP

unitTP = string "{}" >> return unitT

unit1TP = string "{1}" >> return unit1T

unit2TP = string "{2}" >> return unit2T

unit3TP = string "{3}" >> return unit3T

intTP :: Parsec String s (ValT')
intTP = string "Int" >> return intT

boolTP = string "Bool" >> return boolT
pairTP = parensP $ do
  at <- vtypeP
  spaces >> char ',' >> spaces
  bt <- vtypeP
  return (PairT at bt)

parseComp :: String -> Either ParseError Comp'
parseComp = parse p ""
  where p = do spaces
               m <- compP
               spaces
               eof
               return m

compP :: Parsec String s Comp'
compP = do 
  m1 <- choice $ map try 
          [retP
          ,funP
          ,apP
          ,addP
          ,parensP compP]
  mxm2 <- optionMaybe (try extBindP)
  case mxm2 of
    Just abs -> return $ Bind m1 abs
    Nothing -> return m1

extBindP :: Parsec String s (VarId,Comp')
extBindP = do
  spaces >> string "to" >> spaces
  x <- varIdentP
  spaces >> char '|' >> spaces
  m2 <- compP
  return $ (x,m2)

addP :: Parsec String s Comp'
addP = do
  string "add" >> spaces
  arg1 <- spaces >> valP
  arg2 <- spaces >> char ',' >> spaces >> valP
  spaces >> string "as" >> spaces
  x <- varIdentP
  spaces >> char '|' >> spaces
  m' <- compP
  return $ DsC IntAdd (Pair arg1 arg2) (Just x,m')

-- testP :: Parsec String s Comp'
-- testP = do
--   string "test" >> spaces
--   op <- valP
--   arg1 <- spaces >> string "<-" >> spaces >> valP
--   arg2 <- spaces >> char ',' >> spaces >> valP
--   spaces >> string "as" >> spaces
--   x <- varIdentP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC IntTest [op,arg1,arg2] (Just x,m')

-- modP :: Parsec String s Comp'
-- modP = do
--   string "mod" >> spaces
--   op <- valP
--   arg <- spaces >> string "<-" >> spaces >> valP
--   spaces >> string "as" >> spaces
--   x <- varIdentP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC IntMod [op,arg] (Just x,m')

-- strcatP :: Parsec String s Comp'
-- strcatP = do
--   string "strcat" >> spaces
--   s1 <- valP
--   spaces >> char ',' >> spaces
--   s2 <- valP
--   spaces >> string "as" >> spaces
--   x <- varIdentP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC StrCat [s1,s2] (Just x, m')

-- strcmpP :: Parsec String s Comp'
-- strcmpP = do
--   string "strcmp" >> spaces
--   s1 <- valP
--   spaces >> char ',' >> spaces
--   s2 <- valP
--   spaces >> string "as" >> spaces
--   x <- varIdentP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC StrCmp [s1,s2] (Just x, m')

-- strgetP :: Parsec String s Comp'
-- strgetP = do
--   string "strget"
--   spaces >> string "as" >> spaces
--   x <- varIdentP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC StrGet [] (Just x, m')

-- strputP :: Parsec String s Comp'
-- strputP = do
--   string "strput" >> spaces
--   s1 <- valP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC StrPut [s1] (Nothing, m')

-- queryP :: Parsec String s Comp'
-- queryP = do
--   string "query" >> spaces
--   op <- valP
--   spaces >> string "as" >> spaces
--   x <- varIdentP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC IntQuery [op] (Just x,m')

-- issueP :: Parsec String s Comp'
-- issueP = do
--   string "issue" >> spaces
--   op <- valP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC IntIssue [op] (Nothing,m')

-- produceP :: Parsec String s Comp'
-- produceP = do
--   string "produce" >> spaces
--   op <- valP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC IntProduce [op] (Nothing,m')

-- consumeP :: Parsec String s Comp'
-- consumeP = do
--   string "consume" >> spaces
--   op <- valP
--   spaces >> char '|' >> spaces
--   m' <- compP
--   return $ DsC IntConsume [op] (Nothing,m')

parensP :: Parsec String s a -> Parsec String s a
parensP = between (char '(' >> spaces) (spaces >> char ')')

retP = do
  string "return" >> space >> spaces
  v <- valP
  return (Ret v)

varIdentP :: Parsec String s VarId
varIdentP = VarId <$> ((:) <$> lower <*> many alphaNum)

funVar :: Parsec String s VarId
funVar = between (char '|' >> spaces) (spaces >> char '|') varIdentP

funP = do
  x <- funVar
  spaces
  m <- compP
  return $ Fun (x,m)

apP = do
  v <- valP
  spaces >> char '`' >> spaces
  m <- compP
  return (Ap v m)

valP :: Parsec String s (Val')
valP = do
  v <- choice $ map try
         [varP
         ,unitP
         ,unit1P
         ,unit2P
         ,unit3P
         ,intP
         -- ,strP
         ,pairP
         -- ,trueP
         -- ,falseP
         -- ,addP
         -- ,subP
         -- ,leqP
         -- ,geqP
         ,parensP valP]
  spaces
  mvt <- optionMaybe (try annoEndP)
  case mvt of
    Just vt -> return $ Anno v vt
    Nothing -> return v

annoEndP = char ':' >> spaces >> vtypeP

varP = Var <$> varIdentP

unitP :: Parsec String s Val'
unitP = string "{=}" >> return unit

unit1P :: Parsec String s Val'
unit1P = string "{=1}" >> return unit1

unit2P :: Parsec String s Val'
unit2P = string "{=2}" >> return unit2

unit3P :: Parsec String s Val'
unit3P = string "{=3}" >> return unit3

intP :: Parsec String s (Val')
intP = ((intV . read) <$> (char '-' >> many1 digit)) <|> natP

natP :: Parsec String s (Val')
natP = (intV . read) <$> many1 digit

-- strP :: Parsec String s Val'
-- strP = do
--   s <- between (char '"') (char '"') (many p)
--   return (stringV s)
--   where p = satisfy (\c -> c /= '"' && c /= '\\')
--             <|> try (string "\\\"" >> return '"')
--             <|> try (string "\\n" >> return '\n')

pairP :: Parsec String s (Val')
pairP = parensP $ do
  a <- valP
  spaces >> char ',' >> spaces
  b <- valP
  return (Pair a b)

trueP :: Parsec String s (Val')
trueP = string "True" >> return (boolV True)

falseP :: Parsec String s (Val')
falseP = string "False" >> return (boolV False)
