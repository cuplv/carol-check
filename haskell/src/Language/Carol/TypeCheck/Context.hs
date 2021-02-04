{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.TypeCheck.Context
  ( Context
  , empty
  , base
  , onBase
  , Base.VarId
  , modifyM
  , (%>=)
  ) where

import Language.Carol.AST.Refinement
import qualified Language.Carol.TypeCheck.Context.Base as Base

import Control.Monad.State
import Lens.Micro.Platform

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = put =<< lift . f =<< get

(%>=) :: (Monad m) => Lens' s a -> (a -> m a) -> StateT s m ()
-- (%>=) l f = l <~ (lift . f =<< use l)
(%>=) l = modifyM . traverseOf l

data (RefDomain d) => Context d = Context
  { contextBase :: Base.Context d }

base :: (RefDomain d) => Lens' (Context d) (Base.Context d)
base = lens contextBase (\c b -> c {contextBase = b})

onBase :: (Functor m, RefDomain d) 
       => (Base.Context d -> m (Base.Context d))
       -> Context d
       -> m (Context d)
onBase = traverseOf base

empty :: (RefDomain d) => Context d
empty = Context Base.emptyContext
