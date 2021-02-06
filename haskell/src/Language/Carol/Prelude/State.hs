module Language.Carol.Prelude.State 
  ( modifyM
  , module Control.Monad.State
  ) where

import Control.Monad.State

-- | Modify the state in a StateT using a monadic function on the
-- underlying monad.
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = put =<< lift . f =<< get
