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
  ) where

import Language.Carol.AST

import Data.Map (Map)
import qualified Data.Map as M

type TErr = Either String

-- | Existential variable declaration/binding for value types.
data ExV = ExV ExTypeId (Maybe ValT) deriving (Eq,Ord)

instance Show ExV where
  show (ExV e Nothing) = "<" ++ show e ++ ">"
  show (ExV e (Just vt)) = "<" ++ show e ++ ">=" ++ show vt

-- | Existential variable declaration/binding for computation types.
data ExC = ExC ExTypeId (Maybe CompT) deriving (Eq,Ord)

instance Show ExC where
  show (ExC e Nothing) = "<|" ++ show e ++ "|>"
  show (ExC e (Just mt)) = "<|" ++ show e ++ "|>=" ++ show mt

data Context =
    Empty
  | ExValT ExV Context
  | ExCompT ExC Context
  | VarBind VarId ValT Context
  deriving (Eq,Ord)

instance Show Context where
  show Empty = "*"
  show (ExValT e g) =
    show g ++ ", " ++ show e
  show (ExCompT e g) =
    show g ++ ", " ++ show e
  show (VarBind (VarId x) vt g) =
    show g ++ ", " ++ x ++ ":" ++ show vt

emptyContext :: Context
emptyContext = Empty

isBound :: VarId -> Context -> Maybe ValT
isBound x = \case
  Empty -> Nothing
  ExValT _ g' -> isBound x g'
  ExCompT _ g' -> isBound x g'
  VarBind y t g' -> if x == y
                       then Just t
                       else isBound x g'

data ExStatus = ExNonExist | ExUnBound | ExBound ValT

data ExStatusC = ExNonExistC | ExUnBoundC | ExBoundC CompT

existStatusV :: ExTypeId -> Context -> ExStatus
existStatusV a = \case
  Empty -> ExNonExist
  ExValT (ExV a1   Nothing) _ | a == a1 -> ExUnBound
  ExValT (ExV a1 (Just vt)) _ | a == a1 -> ExBound vt
  ExValT _ g' -> existStatusV a g'
  ExCompT _ g' -> existStatusV a g'
  VarBind _ _ g' -> existStatusV a g'

existStatusC :: ExTypeId -> Context -> ExStatusC
existStatusC b = \case
  Empty -> ExNonExistC
  -- ExValT (ExV a1   Nothing) _ | a == a1 -> ExUnBound
  -- ExValT (ExV a1 (Just vt)) _ | a == a1 -> ExBound vt
  ExValT _ g' -> existStatusC b g'
  ExCompT (ExC b1   Nothing) _ | b == b1 -> ExUnBoundC
  ExCompT (ExC b1 (Just mt)) _ | b == b1 -> ExBoundC mt
  ExCompT _ g' -> existStatusC b g'
  VarBind _ _ g' -> existStatusC b g'

collectExs :: Context -> [ExTypeId]
collectExs = \case
  Empty -> []
  ExValT (ExV e _) g' -> e : collectExs g'
  ExCompT (ExC e _) g' -> e : collectExs g'
  VarBind _ _ g' -> collectExs g'

nextEx :: Context -> ExTypeId
nextEx g = case collectExs g of
  [] -> exTypeIdInit
  es -> exTypeIdNext (maximum es)

newExV :: Context -> (Context, ExTypeId)
newExV g = let e = nextEx g
           in (ExValT (ExV e Nothing) g, e)

newExC :: Context -> (Context, ExTypeId)
newExC g = let e = nextEx g
           in (ExCompT (ExC e Nothing) g, e)

bindExV :: ExTypeId -> ValT -> Context -> TErr Context
bindExV a vt g = bindExVR g a vt g

bindExVR :: Context -> ExTypeId -> ValT -> Context -> TErr Context
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

bindExC :: ExTypeId -> CompT -> Context -> TErr Context
bindExC b mt g = bindExCR g b mt g

bindExCR :: Context -> ExTypeId -> CompT -> Context -> TErr Context
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

varBind :: VarId -> ValT -> Context -> Context
varBind = VarBind

trimToVar :: VarId -> Context -> TErr (Context, ValT)
trimToVar x = \case
  Empty -> Left $ "Trim to var " ++ show x ++ " failed."
  ExValT _ g' -> trimToVar x g'
  ExCompT _ g' -> trimToVar x g'
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
  IntT -> return IntT
  PairT vt1 vt2 -> PairT <$> substV g vt1 <*> substV g vt2
  ExVar a -> case existStatusV a g of
    ExBound t -> substV g t
    ExUnBound -> return $ ExVar a
    ExNonExist -> Left $ "<" ++ show a ++ ">" 
                         ++ " doesn't exist? " ++ show g

substC :: Context -> CompT -> TErr CompT
substC g = \case
  RetT vt -> RetT <$> substV g vt
  ProdT pp -> do
    let pl = M.toList pp
    pl' <- mapM (\(i,mt) -> (,) <$> return i <*> substC g mt) pl
    return $ ProdT (M.fromList pl')
  FunT vt mt -> FunT <$> substV g vt <*> substC g mt
  ExVarC b -> case existStatusC b g of
    ExBoundC mt -> substC g mt
    ExUnBoundC -> return $ ExVarC b
    ExNonExistC -> Left $ "<|" ++ show b ++ "|>" 
                          ++ " doesn't exist? " ++ show g

onSnd :: (a -> b) -> (c1,c1,a) -> (c1,c1,b)
onSnd f (c1,c2,a) = (c1,c2,f a)

inb42 :: ExTypeId -> Context -> TErr (ExTypeId,ExTypeId,Context)
inb42 b = \case
  Empty -> Left $ show b ++ " was not in context."
  ExValT e g' -> onSnd (ExValT e) <$> inb42 b g'
  ExCompT (ExC b1 Nothing) g' | b == b1 ->
    let aNew = exTypeIdSub b1
        bNew = exTypeIdNext aNew
        gNew = ExCompT
                 (ExC b1 (Just $ FunT (ExVar aNew) (ExVarC bNew)))
                 (ExCompT
                    (ExC bNew Nothing)
                    (ExValT
                       (ExV aNew Nothing)
                       g'))
    in return (aNew, bNew, gNew)
  ExCompT e g' -> onSnd (ExCompT e) <$> inb42 b g'
  VarBind x vt g' -> onSnd (VarBind x vt) <$> inb42 b g'
