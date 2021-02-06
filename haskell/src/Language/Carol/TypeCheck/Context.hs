{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.TypeCheck.Context
  ( Context
  , emptyContext
  , base
  ) where

import Language.Carol.AST.Types (CompT)
import Language.Carol.AST.Refinement
import qualified Language.Carol.TypeCheck.Context.Base as CB
import Language.Carol.TypeCheck.Error

import Lens.Micro.Platform (lens,Lens')

data (RefDomain d) => Context d = Context
  { contextBase :: CB.Context d }

base :: (RefDomain d) => Lens' (Context d) (CB.Context d)
base = lens contextBase (\c b -> c {contextBase = b})

emptyContext :: (RefDomain d) => Context d
emptyContext = Context CB.emptyContext
