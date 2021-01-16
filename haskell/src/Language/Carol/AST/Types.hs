{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.AST.Types
  ( ValT (..)
  , ValTR (..)
  , fromBase
  , boolT
  , boolSchema
  , trueS
  , falseS
  , CompT (..)
  , SumId (..)
  , ProdId (..)
  , ExIdV
  , ExIdC
  -- , baseTypeV
  -- , baseTypeC
  , RefDomain (..)
  ) where

import Language.Carol.AST.PrettyPrint
import Language.Carol.AST.Refinement
import Language.Carol.AST.Types.ExVars (ExIdV,ExIdC)

import Data.Map (Map)
import qualified Data.Map as M

newtype SumId = SumId String deriving (Show,Eq,Ord)

newtype ProdId = ProdId String deriving (Show,Eq,Ord)

data (RefDomain d) => ValTR d = 
    ValTR (ValT d) (DRef d)
  | ExVT ExIdV
  deriving (Eq,Ord)

deriving instance (Show d, RefDomain d, Show (DRef d))
    => Show (ValTR d)

instance (RefDomain d, Pretty d, Pretty (DRef d))
    => Pretty (ValTR d) where
  pretty (ValTR t r) = "{ ν:" ++ pretty t ++ " | " ++ pretty r ++ " }"
  pretty (ExVT e) = pretty e

data (RefDomain d) => ValT d =
    ThunkT (CompT d)
  | SumT (Map SumId (ValTR d))
  | UnitT
  | PairT (ValTR d) (ValTR d)
  | DsT d
  deriving (Eq,Ord)

fromBase :: (RefDomain d) => ValT d -> ValTR d
fromBase t = ValTR t refEmpty

deriving instance (Show d, RefDomain d, Show (DRef d))
    => Show (ValT d)

instance (RefDomain d, Pretty d, Pretty (DRef d))
    => Pretty (ValT d) where
  pretty = \case
    ThunkT mt -> "U(" ++ pretty mt ++ ")"
    SumT mp -> "Σ(" ++ "..." ++ ")"
    UnitT -> "{}"
    PairT vt1 vt2 -> "(" ++ pretty vt1 ++ ", " ++ pretty vt2 ++ ")"
    -- DsT t RefTrue -> pretty t
    -- DsT t r -> "{ ν:" ++ pretty t ++ " | " ++ pretty r ++ " }"
    -- ExVT e -> pretty e

-- baseTypeV :: (RefDomain d) => ValT d -> ValT d
-- baseTypeV = \case
--   ThunkT mt -> ThunkT $ baseTypeC mt
--   SumT mp -> SumT $ M.map baseTypeV mp
--   UnitT -> UnitT
--   PairT vt1 vt2 -> PairT (baseTypeV vt1) (baseTypeV vt2)
--   DsT t r -> DsT t RefTrue -- remove refinement
--   ExVT e -> ExVT e

trueS = SumId "True"
falseS = SumId "False"

boolSchema :: (RefDomain d) => Map SumId (ValTR d)
boolSchema = M.fromList [(trueS,fromBase UnitT)
                        ,(falseS,fromBase UnitT)]

boolT :: (RefDomain d) => ValT d
boolT = SumT boolSchema

data CompT d =
    RetT (ValTR d)
  | ProdT (Map ProdId (CompT d))
  | FunT (ValTR d) (CompT d)
  | ExCT ExIdC
  deriving (Eq,Ord)

deriving instance (Show d, RefDomain d, Show (DRef d))
    => Show (CompT d)

instance (RefDomain d, Pretty d, Pretty (DRef d))
    => Pretty (CompT d) where
  pretty = \case
    RetT vt -> "F(" ++ pretty vt ++ ")"
    ProdT mp -> "Π(" ++ "..." ++ ")"
    FunT vt mt' -> pretty vt ++ " → " ++ pretty mt'
    ExCT e -> pretty e


-- baseTypeC :: (RefDomain d) => CompT d -> CompT d
-- baseTypeC = \case
--   RetT vt -> RetT $ baseTypeV vt
--   ProdT mp -> ProdT $ M.map baseTypeC mp
--   FunT vt mt -> FunT (baseTypeV vt) (baseTypeC mt)
--   ExCT e -> ExCT e
