{-# LANGUAGE LambdaCase #-}

module Language.Carol where

import Control.Monad.Free

import qualified Language.CBPV as CBPV
import Language.CBPV hiding (Node,Term)

data Carol i m v = Query i v (m v) | Issue v (m v)
  deriving (Show,Eq,Ord)

instance (Functor m) => Functor (Carol i m) where
  fmap f = \case
              Query i v m -> Query i (f v) (fmap f m)
              Issue v m -> Issue (f v) (fmap f m)

type Node i = Carol i (CBPV.Node i)
type Term i = Free (Node i)
