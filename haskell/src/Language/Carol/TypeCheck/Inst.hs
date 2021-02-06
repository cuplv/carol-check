module Language.Carol.TypeCheck.Inst 
  ( instLV
  , instRC
  ) where

import Language.Carol.AST.Types
import Language.Carol.TypeCheck.Context (modifyM,substC')
import Language.Carol.TypeCheck.Context.Base
import Language.Carol.TypeCheck.Error

import Control.Monad.State
import Lens.Micro.Platform

instLV :: (RefDomain d)
  => ExIdV
  -> ValT d
  -> StateT (Context d) (TErr d) ()
instLV a vt = case vt of
  -- InstLReach
  ExVT a1 | a < a1 -> modifyM $ bindExV a1 (ExVT a)
  -- InstLSolve
  vt -> modifyM $ bindExV a vt

instRC :: (RefDomain d)
  => CompT d
  -> ExIdC
  -> StateT (Context d) (TErr d) ()
instRC mt b = case mt of
  -- InstRReach
  ExCT b1 | b < b1 -> modifyM $ bindExC b1 (ExCT b)
  -- InstRArr
  FunT vt mt' -> do
    (aNew,bNew) <- inb42' b
    instLV aNew vt

    mt'' <- substC' id mt'
    instRC mt'' bNew
  mt -> modifyM $ bindExC b mt

inb42' :: (RefDomain d)
       => ExIdC
       -> StateT (Context d) (TErr d) (ExIdV,ExIdC)
inb42' b = do g <- get
              (aNew,bNew,g') <- lift $ inb42 b g
              put g'
              return (aNew,bNew)

-- substC' :: (RefDomain d)
--         => CompT d
--         -> StateT (Context d) (TErr d) (CompT d)
-- substC' mt = do g <- get
--                 lift $ substC g mt
