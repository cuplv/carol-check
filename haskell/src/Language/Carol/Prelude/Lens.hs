{-# LANGUAGE RankNTypes #-}

module Language.Carol.Prelude.Lens
  ( (%>=)
  , module Lens.Micro.Platform
  ) where

import Language.Carol.Prelude.State

import Lens.Micro.Platform

-- | Just like '%=', except that the modifying function is monadic, on
-- the underlying monad of the StateT.
(%>=) :: (Monad m) => Lens' s a -> (a -> m a) -> StateT s m ()
(%>=) l = zoom l . modifyM
