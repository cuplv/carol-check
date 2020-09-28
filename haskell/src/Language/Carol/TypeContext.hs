{-# LANGUAGE LambdaCase #-}

module Language.Carol.TypeContext 
  ( Context
  , emptyContext
  , isBound
  , highEx
  , newEx
  , newEx2
  , bindEx
  , varBind
  , trimToVar
  , substV
  , substC
  , TErr
  , MonoType (..)
  , ExVarId
  ) where

import Language.Carol.AST

import Data.Map (Map)
import qualified Data.Map as M

type TErr = Either String

data ExVarId = ExVarId Int deriving (Show,Eq,Ord)

data (Domain d) => MonoType d =
    Concrete (ValT d)
  | Exist ExVarId
  deriving (Show,Eq,Ord)

data (Domain d) => Context d = 
    Empty
  | ExInit ExVarId (Context d)
  | ExBind ExVarId (MonoType d) (Context d)
  | VarBind VarId (MonoType d) (Context d)
  deriving (Show,Eq,Ord)

emptyContext :: (Domain d) => Context d
emptyContext = Empty

isBound :: (Domain d) => VarId -> Context d -> Maybe (MonoType d)
isBound x = \case
  Empty -> Nothing
  ExInit _ g' -> isBound x g'
  ExBind _ _ g' -> isBound x g'
  VarBind y t g' -> if x == y
                       then Just t
                       else isBound x g'

existStatus :: (Domain d) => ExVarId -> Context d -> Maybe (Maybe (MonoType d))
existStatus a = \case
  Empty -> Nothing
  ExInit b g' -> if a == b
                    then Just Nothing
                    else existStatus a g'
  ExBind b t g' -> if a == b
                      then Just (Just t)
                      else existStatus a g'
  VarBind _ _ g' -> existStatus a g'

highEx :: (Domain d) => Context d -> ExVarId
highEx = \case
            ExInit a _ -> a
            ExBind a _ _ -> a
            Empty -> ExVarId 0
            VarBind _ _ g' -> highEx g'

newEx :: (Domain d) => Context d -> (Context d, ExVarId)
newEx g = let ExVarId i = highEx g
              a = ExVarId $ i + 1
          in (ExInit a g, a)

newEx2 :: (Domain d) => Context d -> (Context d, ExVarId, ExVarId)
newEx2 g = let (g',a) = newEx g
               (g'',b) = newEx g'
           in (g'',a,b)

bindEx :: (Domain d) => ExVarId -> MonoType d -> Context d -> TErr (Context d)
bindEx a t = \case
  Empty -> Left $ show a ++ " does not exist in the context."
  ExInit b g | a == b -> return $ ExBind a t g
  ExBind b t1 g | a == b -> Left $ show a ++ " is already solved?"
  ExBind b t1 g | a /= b -> ExBind b t <$> bindEx a t g
  VarBind x t1 g -> VarBind x t1 <$> bindEx a t g

varBind :: (Domain d) => VarId -> MonoType d -> Context d -> Context d
varBind = VarBind

trimToVar :: (Domain d) => VarId -> Context d -> TErr (Context d, MonoType d)
trimToVar x = \case
  Empty -> Left $ "Trim to var " ++ show x ++ " failed."
  ExInit _ g' -> trimToVar x g'
  ExBind _ _ g' -> trimToVar x g'
  VarBind y t g' -> if x == y
                       then Right (g',t)
                       else trimToVar x g'

substV :: (Domain d) => Context d -> MonoType d -> TErr (ValT d)
substV g = \case
  Concrete vt -> return vt
  Exist a -> case existStatus a g of
               Just (Just t) -> substV g t
               Just Nothing -> Left $ show a ++ " was left unsolved."
               Nothing -> Left $ show a ++ " doesn't exist?"

substC :: (Domain d) => Context d -> CompT (MonoType d) -> TErr (CompT (ValT d))
substC g = \case
  RetT vt -> RetT <$> substV g vt
  FunT vt mt -> FunT <$> substV g vt <*> substC g mt
