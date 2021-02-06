{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.TypeCheck.Context.Index
  ( Context
  , IVarId
  ) where

import Language.Carol.AST.Refinement
import Language.Carol.Prelude.Internal

type Vid = IVarId

data ExIVarId = ExId Int deriving (Show,Eq,Ord)

type EVid = ExIVarId

data (RefDomain d) => Context d = 
    Empty
  | VarBind Vid (ISort d) (Context d)
  | Ex EVid (Maybe Vid)
  deriving (Eq,Ord)
