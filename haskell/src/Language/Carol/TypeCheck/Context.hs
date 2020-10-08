{-# LANGUAGE LambdaCase #-}

module Language.Carol.TypeCheck.Context
  ( Context
  , emptyContext
  , isBound
  , newExV
  , newExC
  , bindExV
  , bindExC
  , varBind
  , trimToVar
  , substV
  , substC
  , inb42
  , TErr
  , VarId
  ) where

import Language.Carol.AST.Terms (VarId)
import Language.Carol.AST.Types
import Language.Carol.AST.Types.ExVars

import Data.Map (Map)
import qualified Data.Map as M

type TErr = Either String

-- | Existential variable declaration/binding for value types.
data ExV d = ExV ExIdV (Maybe (ValT d)) deriving (Eq,Ord)

instance (Show d, Eq d) => Show (ExV d) where
  show (ExV e Nothing) = show e
  show (ExV e (Just vt)) = show e ++ "=" ++ show vt

-- | Existential variable declaration/binding for computation types.
data ExC d = ExC ExIdC (Maybe (CompT d)) deriving (Eq,Ord)

instance (Show d, Eq d) => Show (ExC d) where
  show (ExC e Nothing) = show e
  show (ExC e (Just mt)) = show e ++ "=" ++ show mt

data Context d =
    Empty
  | ExValT (ExV d) (Context d)
  | ExCompT (ExC d) (Context d)
  | VarBind VarId (ValT d) (Context d)
  deriving (Eq,Ord)

instance (Show d, Eq d) => Show (Context d) where
  show Empty = "*"
  show (ExValT e g) =
    show g ++ ", " ++ show e
  show (ExCompT e g) =
    show g ++ ", " ++ show e
  show (VarBind x vt g) =
    show g ++ ", " ++ show x ++ ":" ++ show vt

emptyContext :: (Show d, Eq d) => Context d
emptyContext = Empty

isBound :: (Show d, Eq d) => VarId -> Context d -> Maybe (ValT d)
isBound x = \case
  Empty -> Nothing
  ExValT _ g' -> isBound x g'
  ExCompT _ g' -> isBound x g'
  VarBind y t g' -> if x == y
                       then Just t
                       else isBound x g'

data ExStatus d = ExNonExist | ExUnBound | ExBound (ValT d)

data ExStatusC d = ExNonExistC | ExUnBoundC | ExBoundC (CompT d)

existStatusV :: (Show d, Eq d) => ExIdV -> Context d -> ExStatus d
existStatusV a = \case
  Empty -> ExNonExist
  ExValT (ExV a1   Nothing) _ | a == a1 -> ExUnBound
  ExValT (ExV a1 (Just vt)) _ | a == a1 -> ExBound vt
  ExValT _ g' -> existStatusV a g'
  ExCompT _ g' -> existStatusV a g'
  VarBind _ _ g' -> existStatusV a g'

existStatusC :: (Show d, Eq d) => ExIdC -> Context d -> ExStatusC d
existStatusC b = \case
  Empty -> ExNonExistC
  -- ExValT (ExV a1   Nothing) _ | a == a1 -> ExUnBound
  -- ExValT (ExV a1 (Just vt)) _ | a == a1 -> ExBound vt
  ExValT _ g' -> existStatusC b g'
  ExCompT (ExC b1   Nothing) _ | b == b1 -> ExUnBoundC
  ExCompT (ExC b1 (Just mt)) _ | b == b1 -> ExBoundC mt
  ExCompT _ g' -> existStatusC b g'
  VarBind _ _ g' -> existStatusC b g'

collectExs :: (Show d, Eq d) => Context d -> [ExTypeId]
collectExs = \case
  Empty -> []
  ExValT (ExV (ExIdV e) _) g' -> e : collectExs g'
  ExCompT (ExC (ExIdC e) _) g' -> e : collectExs g'
  VarBind _ _ g' -> collectExs g'

nextEx :: (Show d, Eq d) => Context d -> ExTypeId
nextEx g = case collectExs g of
  [] -> exTypeIdInit
  es -> exTypeIdNext (maximum es)

newExV :: (Show d, Eq d) => Context d -> (Context d, ExIdV)
newExV g = let a = ExIdV (nextEx g)
           in (ExValT (ExV a Nothing) g, a)

newExC :: (Show d, Eq d) => Context d -> (Context d, ExIdC)
newExC g = let b = ExIdC (nextEx g)
           in (ExCompT (ExC b Nothing) g, b)

bindExV :: (Show d, Eq d) => ExIdV -> ValT d -> Context d -> TErr (Context d)
bindExV a vt g = bindExVR g a vt g

bindExVR :: (Show d, Eq d) => Context d -> ExIdV -> ValT d -> Context d -> TErr (Context d)
bindExVR g0 a vt = \case
  Empty -> Left $ "Cannot bind " ++ show vt ++ " because " 
                  ++ "<" ++ show a ++ ">" 
                  ++ " does not exist in context " ++ show g0
  ExValT (ExV a1    Nothing) g' | a == a1 -> 
    return $ ExValT (ExV a1 (Just vt)) g'
  ExValT (ExV a1 (Just vt1)) g' | a == a1 && vt == vt1 -> 
    return $ ExValT (ExV a1 (Just vt1)) g'
  ExValT (ExV a1 (Just vt1)) g' | a == a1 && vt /= vt1 ->
    Left $ show a ++ " is already solved to non-match: " 
           ++ show vt1 ++ " != " ++ show vt

  ExValT     e g' -> ExValT     e <$> bindExVR g0 a vt g'
  ExCompT    e g' -> ExCompT    e <$> bindExVR g0 a vt g'
  VarBind x t1 g' -> VarBind x t1 <$> bindExVR g0 a vt g'

bindExC :: (Show d, Eq d) => ExIdC -> CompT d -> Context d -> TErr (Context d)
bindExC b mt g = bindExCR g b mt g

bindExCR :: (Show d, Eq d) => Context d -> ExIdC -> CompT d -> Context d -> TErr (Context d)
bindExCR g0 b mt = \case
  Empty -> Left $ "<|" ++ show b ++ "|>" 
                  ++ " does not exist in context " ++ show g0
  ExCompT (ExC b1    Nothing) g' | b == b1 -> 
    return $ ExCompT (ExC b1 (Just mt)) g'
  ExCompT (ExC b1 (Just mt1)) g' | b == b1 && mt == mt1 -> 
    return $ ExCompT (ExC b1 (Just mt1)) g'
  ExCompT (ExC b1 (Just mt1)) g' | b == b1 && mt /= mt1 ->
    Left $ show b ++ " is already solved to non-match: " 
           ++ show mt1 ++ " != " ++ show mt

  ExValT     e g' -> ExValT     e <$> bindExCR g0 b mt g'
  ExCompT    e g' -> ExCompT    e <$> bindExCR g0 b mt g'
  VarBind x t1 g' -> VarBind x t1 <$> bindExCR g0 b mt g'

varBind :: (Show d, Eq d) => VarId -> ValT d -> Context d -> Context d
varBind = VarBind

trimToVar :: (Show d, Eq d) => VarId -> Context d -> TErr (Context d)
trimToVar x = \case
  Empty -> Left $ "Trim to var " ++ show x ++ " failed."
  ExValT _ g' -> trimToVar x g'
  ExCompT _ g' -> trimToVar x g'
  VarBind y t g' -> if x == y
                       then return g'
                       else trimToVar x g'

substV :: (Show d, Eq d) => Context d -> ValT d -> TErr (ValT d)
substV g = \case
  ThunkT mt -> ThunkT <$> substC g mt
  SumT ss -> do
    let sl = M.toList ss
    sl' <- mapM (\(i,vt) -> (,) <$> return i <*> substV g vt) sl
    return $ SumT (M.fromList sl')
  UnitT -> return UnitT
  PairT vt1 vt2 -> PairT <$> substV g vt1 <*> substV g vt2
  DsT d -> return (DsT d)
  ExVT a -> case existStatusV a g of
    ExBound t -> substV g t
    ExUnBound -> return $ ExVT a
    ExNonExist -> Left $ "<" ++ show a ++ ">" 
                         ++ " doesn't exist? " ++ show g

substC :: (Show d, Eq d) => Context d -> CompT d -> TErr (CompT d)
substC g = \case
  RetT vt -> RetT <$> substV g vt
  ProdT pp -> do
    let pl = M.toList pp
    pl' <- mapM (\(i,mt) -> (,) <$> return i <*> substC g mt) pl
    return $ ProdT (M.fromList pl')
  FunT vt mt -> FunT <$> substV g vt <*> substC g mt
  ExCT b -> case existStatusC b g of
    ExBoundC mt -> substC g mt
    ExUnBoundC -> return $ ExCT b
    ExNonExistC -> Left $ "<|" ++ show b ++ "|>" 
                          ++ " doesn't exist? " ++ show g

onSnd :: (a -> b) -> (c1,c2,a) -> (c1,c2,b)
onSnd f (c1,c2,a) = (c1,c2,f a)

inb42 :: (Show d, Eq d) => ExIdC -> Context d -> TErr (ExIdV,ExIdC,Context d)
inb42 b = \case
  Empty -> Left $ show b ++ " was not in context."
  ExValT e g' -> onSnd (ExValT e) <$> inb42 b g'
  ExCompT (ExC (ExIdC b1) Nothing) g' | b == ExIdC b1 ->
    let eTmp = exTypeIdSub b1
        aNew = ExIdV (eTmp)
        bNew = ExIdC (exTypeIdNext eTmp)
        gNew = ExCompT
                 (ExC (ExIdC b1) (Just $ FunT (ExVT aNew) (ExCT bNew)))
                 (ExCompT
                    (ExC bNew Nothing)
                    (ExValT
                       (ExV aNew Nothing)
                       g'))
    in return (aNew, bNew, gNew)
  ExCompT e g' -> onSnd (ExCompT e) <$> inb42 b g'
  VarBind x vt g' -> onSnd (VarBind x vt) <$> inb42 b g'
