{-# LANGUAGE LambdaCase #-}

module Language.Carol.TypeContext 
  ( Context
  , emptyContext
  , isBound
  , highEx
  , newEx
  , bindEx
  , varBind
  , trimToVar
  , substV
  , substC
  , TErr
  ) where

import Language.Carol.AST

import Data.Map (Map)
import qualified Data.Map as M

type TErr = Either String

data Context = 
    Empty
  | ExInit ExTypeId Context
  | ExBind ExTypeId ValT Context
  | VarBind VarId ValT Context

emptyContext :: Context
emptyContext = Empty

isBound :: VarId -> Context -> Maybe ValT
isBound x = \case
  Empty -> Nothing
  ExInit _ g' -> isBound x g'
  ExBind _ _ g' -> isBound x g'
  VarBind y t g' -> if x == y
                       then Just t
                       else isBound x g'

data ExStatus = ExNonExist | ExUnBound | ExBound ValT

existStatus :: ExTypeId -> Context -> ExStatus
existStatus a = \case
  Empty -> ExNonExist
  ExInit b g' -> if a == b
                    then ExUnBound
                    else existStatus a g'
  ExBind b t g' -> if a == b
                      then ExBound t
                      else existStatus a g'
  VarBind _ _ g' -> existStatus a g'

highEx :: Context -> ExTypeId
highEx = \case
            ExInit a _ -> a
            ExBind a _ _ -> a
            Empty -> ExTypeId 0
            VarBind _ _ g' -> highEx g'

newEx :: Context -> (Context, ExTypeId)
newEx g = let ExTypeId i = highEx g
              a = ExTypeId $ i + 1
          in (ExInit a g, a)

bindEx :: ExTypeId -> ValT -> Context -> TErr Context
bindEx a t = \case
  Empty -> Left $ show a ++ " does not exist in the context."
  ExInit b g | a == b -> return $ ExBind a t g
  ExBind b t1 g | a == b -> Left $ show a ++ " is already solved?"
  ExBind b t1 g | a /= b -> ExBind b t <$> bindEx a t g
  VarBind x t1 g -> VarBind x t1 <$> bindEx a t g

varBind :: VarId -> ValT -> Context -> Context
varBind = VarBind

trimToVar :: VarId -> Context -> TErr (Context, ValT)
trimToVar x = \case
  Empty -> Left $ "Trim to var " ++ show x ++ " failed."
  ExInit _ g' -> trimToVar x g'
  ExBind _ _ g' -> trimToVar x g'
  VarBind y t g' -> if x == y
                       then Right (g',t)
                       else trimToVar x g'

substV :: Context -> ValT -> TErr ValT
substV g = \case
  ThunkT mt -> ThunkT <$> substC g mt
  SumT ss -> do
    let sl = M.toList ss
    sl' <- mapM (\(i,vt) -> (,) <$> return i <*> substV g vt) sl
    return $ SumT (M.fromList sl')
  UnitT -> return UnitT
  PairT vt1 vt2 -> PairT <$> substV g vt1 <*> substV g vt2
  ExVar a -> case existStatus a g of
    ExBound t -> substV g t
    ExUnBound -> Left $ show a ++ " was left unsolved."
    ExNonExist -> Left $ show a ++ " doesn't exist?"

substC :: Context -> CompT -> TErr CompT
substC g = \case
  RetT vt -> RetT <$> substV g vt
  ProdT pp -> do
    let pl = M.toList pp
    pl' <- mapM (\(i,mt) -> (,) <$> return i <*> substC g mt) pl
    return $ ProdT (M.fromList pl')
  FunT vt mt -> FunT <$> substV g vt <*> substC g mt
