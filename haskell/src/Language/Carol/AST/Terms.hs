{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Carol.AST.Terms 
  ( Val (..)
  , ValDomain (..)
  , boolV
  , unit
  , unit1
  , unit2
  , unit3
  , Comp (..)
  , CompDomain (..)
  , dCompType
  , VarId
  ) where

import Data.Map (Map)
import qualified Data.Map as M

import Language.Carol.AST.Types
import Language.Carol.Prelude.Types

class (RefDomain d, Eq (DVal d), Ord (DVal d))
    => ValDomain d where
  data DVal d
  dValType :: DVal d -> (d, Refinement d)

class (ValDomain d) => CompDomain e d where
  dCompSigR :: e -> ([(IVarId, ISort d)], ValT d, ValT d)
  dCompSig :: e -> ([(IVarId, ValT d)], ValT d)
  dCompOutput :: e -> Maybe (ValT d)
  dCompInputs :: e -> Maybe (IVarId, ValT d) -> [(IVarId, ValT d)]
  dCompPretty :: e -> [Val e d] -> String

dCompType :: (CompDomain e d) => e -> CompT d
dCompType e = 
  let f (IVarId x, vt) mt = FunT (Just$ VarId x) vt mt
      (ins,out) = dCompSig e
  in foldr f (RetT out) ins

data (CompDomain e d) => Val e d =
    Var VarId
  | Thunk (Comp e d)
  | Sum (Map SumId (ValT d)) SumId (Val e d)
  | Unit
  | Pair (Val e d) (Val e d)
  | DsV (DVal d)
  | Anno (Val e d) (ValT d)
  deriving (Eq,Ord)

instance (Pretty d, Pretty (DRef d), CompDomain e d, Pretty (ISort d)) 
    => Pretty (Val e d) where
  pretty = \case
    Var i -> pretty i
    Thunk m -> "thunk " ++ pretty m
    Sum mp (SumId s) v -> s ++ "(" ++ pretty v ++ ")"
    Unit -> "{=}"
    Pair v1 v2 -> "(" ++ pretty v1 ++ ", " ++ pretty v2 ++ ")"
    Anno v vt -> pretty v ++ " : " ++ pretty vt

unit :: (CompDomain e d) => Val e d
unit = Unit

unit1 :: (CompDomain e d) => Val e d
unit1 = unit

unit2 :: (CompDomain e d) => Val e d
unit2 = Pair unit unit1

unit3 :: (CompDomain e d) => Val e d
unit3 = Pair unit unit2

boolV :: (CompDomain e d) => Bool -> Val e d
boolV b = Sum boolSchema i Unit
  where i = if b
               then trueS
               else falseS

type Abst e d = (VarId, Comp e d)

data (CompDomain e d) => Comp e d =
    Ret (Val e d)
  | Prod (Map ProdId (Comp e d))
  | Fun (Abst e d)
  | Let (Val e d) (Abst e d)
  | Bind (Comp e d) (Abst e d)
  | Force (Val e d)
  | Pmp (Val e d) (VarId, VarId, Comp e d)
  | Pms (Val e d) (Map SumId (Abst e d))
  | Proj ProdId (Comp e d)
  | Ap (Val e d) (Comp e d)
  | DsC e [(Val e d)] (Maybe VarId, Comp e d)
  | AnnoC (Comp e d) (CompT d)
  deriving (Eq,Ord)

sha :: (Pretty a, Pretty b) => (a,b) -> String
sha (x,m') = pretty x ++ "| " ++ pretty m'

instance (Pretty d, Pretty (DRef d), CompDomain e d, Pretty (ISort d)) 
    => Pretty (Comp e d) where
  pretty = \case
    Ret v -> "return " ++ pretty v
    Prod pm -> "{" ++ "..." ++ "}"
    Fun xm -> "|" ++ sha xm
    Let v xm -> "let " ++ pretty v ++ " be " ++ sha xm
    Bind m1 xm -> pretty m1 ++ " to " ++ sha xm
    Pmp v (x,y,m') -> "pm " ++ pretty v ++ " as " ++ show (x,y) 
                      ++ "| " ++ pretty m'
    Pms v mp -> "pm " ++ pretty v ++ " as {" ++ "..." ++ "}"
    Proj i m' -> show i ++ "`" ++ pretty m'
    Ap v m' -> pretty v ++ "`" ++ pretty m'
    DsC e vs (Just x,m') -> dCompPretty e vs ++ " as " 
                            ++ sha (x,m')
    DsC e vs (Nothing,m') -> dCompPretty e vs ++ " |" 
                             ++ pretty m'
