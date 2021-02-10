{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.TypeCheck.Context.Base
  ( Context
  , emptyContext
  , isBound
  , newExV
  , newExC
  , bindExV
  , bindExC
  , varBind
  , idxBind
  , exIdx
  , trimToVar
  , substV
  , substC
  , substC'
  , inb42
  , quantifyContext
  ) where

import Language.Carol.AST.Refinement (AnySym (..))
import Language.Carol.AST.Types
import Language.Carol.AST.Types.ExVars
import Language.Carol.Prelude.Internal
import Language.Carol.TypeCheck.Error

import Data.Map (Map)
import qualified Data.Map as M
import Data.SBV

-- | Existential variable declaration/binding for value types.
data (RefDomain d) => ExV d =
  ExV ExIdV (Maybe (ValT d))
  deriving (Eq,Ord)

instance (RefDomain d, Pretty d, Pretty (DRef d), Pretty (ISort d)) => Pretty (ExV d) where
  pretty (ExV e Nothing) = pretty e
  pretty (ExV e (Just vt)) = pretty e ++ "=" ++ pretty vt

-- | Existential variable declaration/binding for computation types.
data ExC d = ExC ExIdC (Maybe (CompT d)) deriving (Eq,Ord)

instance (RefDomain d, Pretty d, Pretty (DRef d), Pretty (ISort d)) => Pretty (ExC d) where
  pretty (ExC e Nothing) = pretty e
  pretty (ExC e (Just mt)) = pretty e ++ "=" ++ pretty mt

data (RefDomain d) => Context d =
    Empty
  | ExValT (ExV d) (Context d)
  | ExCompT (ExC d) (Context d)
  | VarBind VarId (ValT d) (Context d)
  | IdxBind IVarId (ISort d) (Context d)
  | ExIdx IVarId (ISort d) (Context d)
  deriving (Eq,Ord)

instance (RefDomain d, Pretty d, Pretty (DRef d), Pretty (ISort d)) => Pretty (Context d) where
  pretty Empty = "*"
  pretty (ExValT e g) =
    pretty g ++ ", " ++ pretty e
  pretty (ExCompT e g) =
    pretty g ++ ", " ++ pretty e
  pretty (VarBind x vt g) =
    pretty g ++ ", " ++ pretty x ++ ":" ++ pretty vt
  pretty (IdxBind a vt g) =
    pretty g ++ ", " ++ pretty a ++ ":" ++ pretty vt
emptyContext :: Context d
emptyContext = Empty

isBound :: (RefDomain d) => VarId -> Context d -> Maybe (ValT d)
isBound x = \case
  Empty -> Nothing
  ExValT _ g' -> isBound x g'
  ExCompT _ g' -> isBound x g'
  VarBind y t g' -> if x == y
                       then Just t
                       else isBound x g'
  IdxBind _ _ g' -> isBound x g'
  ExIdx _ _ g' -> isBound x g'

data ExStatus d = ExNonExist | ExUnBound | ExBound (ValT d)

data ExStatusC d = ExNonExistC | ExUnBoundC | ExBoundC (CompT d)

existStatusV :: (RefDomain d) => ExIdV -> Context d -> ExStatus d
existStatusV a = \case
  Empty -> ExNonExist
  ExValT (ExV a1   Nothing) _ | a == a1 -> ExUnBound
  ExValT (ExV a1 (Just vt)) _ | a == a1 -> ExBound vt
  ExValT _ g' -> existStatusV a g'
  ExCompT _ g' -> existStatusV a g'
  VarBind _ _ g' -> existStatusV a g'
  IdxBind _ _ g' -> existStatusV a g'
  ExIdx _ _ g' -> existStatusV a g'

existStatusC :: (RefDomain d) => ExIdC -> Context d -> ExStatusC d
existStatusC b = \case
  Empty -> ExNonExistC
  -- ExValT (ExV a1   Nothing) _ | a == a1 -> ExUnBound
  -- ExValT (ExV a1 (Just vt)) _ | a == a1 -> ExBound vt
  ExValT _ g' -> existStatusC b g'
  ExCompT (ExC b1   Nothing) _ | b == b1 -> ExUnBoundC
  ExCompT (ExC b1 (Just mt)) _ | b == b1 -> ExBoundC mt
  ExCompT _ g' -> existStatusC b g'
  VarBind _ _ g' -> existStatusC b g'

collectExs :: (RefDomain d) => Context d -> [ExTypeId]
collectExs = \case
  Empty -> []
  ExValT (ExV (ExIdV e) _) g' -> e : collectExs g'
  ExCompT (ExC (ExIdC e) _) g' -> e : collectExs g'
  VarBind _ _ g' -> collectExs g'

nextEx :: (RefDomain d) => Context d -> ExTypeId
nextEx g = case collectExs g of
  [] -> exTypeIdInit
  es -> exTypeIdNext (maximum es)

newExV :: (RefDomain d) => Context d -> (Context d, ExIdV)
newExV g = let a = ExIdV (nextEx g)
           in (ExValT (ExV a Nothing) g, a)

newExC :: (RefDomain d) => Context d -> (Context d, ExIdC)
newExC g = let b = ExIdC (nextEx g)
           in (ExCompT (ExC b Nothing) g, b)

bindExV :: (RefDomain d) => ExIdV -> ValT d -> Context d -> TErr d (Context d)
bindExV a vt g = bindExVR g a vt g

bindExVR :: (RefDomain d) => Context d -> ExIdV -> ValT d -> Context d -> TErr d (Context d)
bindExVR g0 a vt = \case
  Empty -> terr $ TOther "Context cannot bind; missing exvar."
  ExValT (ExV a1    Nothing) g' | a == a1 -> do
    -- Add { nu == a } constraint to vt before inserting into the
    -- context here.
    -- let vt' = addEqRef (IVarId $ show a) vt
    return $ ExValT (ExV a1 (Just vt)) g'
  ExValT (ExV a1 (Just vt1)) g' | a == a1 && vt == vt1 -> 
    return $ ExValT (ExV a1 (Just vt1)) g'
  ExValT (ExV a1 (Just vt1)) g' | a == a1 && vt /= vt1 ->
    terr $ TOther "Exvar already solved to non-match." 
  ExValT     e g' -> ExValT     e <$> bindExVR g0 a vt g'
  ExCompT    e g' -> ExCompT    e <$> bindExVR g0 a vt g'
  VarBind x t1 g' -> VarBind x t1 <$> bindExVR g0 a vt g'
  IdxBind x t1 g' -> IdxBind x t1 <$> bindExVR g0 a vt g'
  ExIdx   x t1 g' -> IdxBind x t1 <$> bindExVR g0 a vt g'

bindExC :: (RefDomain d) => ExIdC -> CompT d -> Context d -> TErr d (Context d)
bindExC b mt g = bindExCR g b mt g

bindExCR :: (RefDomain d) => Context d -> ExIdC -> CompT d -> Context d -> TErr d (Context d)
bindExCR g0 b mt = \case
  Empty -> terr $ TOther "Context cannot bind; missing comp-exvar."
  ExCompT (ExC b1    Nothing) g' | b == b1 -> 
    return $ ExCompT (ExC b1 (Just mt)) g'
  ExCompT (ExC b1 (Just mt1)) g' | b == b1 && mt == mt1 -> 
    return $ ExCompT (ExC b1 (Just mt1)) g'
  ExCompT (ExC b1 (Just mt1)) g' | b == b1 && mt /= mt1 ->
    terr $ TOther "Comp-exvar is already solved to non-match."
  ExValT     e g' -> ExValT     e <$> bindExCR g0 b mt g'
  ExCompT    e g' -> ExCompT    e <$> bindExCR g0 b mt g'
  VarBind x t1 g' -> VarBind x t1 <$> bindExCR g0 b mt g'

varBind :: (RefDomain d) => VarId -> ValT d -> Context d -> Context d
varBind = VarBind

idxBind :: (RefDomain d) => IVarId -> ISort d -> Context d -> Context d
idxBind = IdxBind

exIdx :: (RefDomain d) => IVarId -> ISort d -> Context d -> Context d
exIdx = ExIdx

trimToVar :: (RefDomain d) => VarId -> Context d -> TErr d (Context d)
trimToVar x = \case
  Empty -> terr $ TOther ("Trim to var " ++ show x ++ " failed.")
  ExValT _ g' -> trimToVar x g'
  ExCompT _ g' -> trimToVar x g'
  VarBind y t g' -> if x == y
                       then return g'
                       else trimToVar x g'

substV :: (RefDomain d) => Context d -> ValT d -> TErr d (ValT d)
substV g = \case
  ThunkT mt -> ThunkT <$> substC g mt
  SumT ss -> do
    let sl = M.toList ss
    sl' <- mapM (\(i,vt) -> (,) <$> return i <*> substV g vt) sl
    return $ SumT (M.fromList sl')
  UnitT -> return UnitT
  PairT vt1 vt2 -> PairT <$> substV g vt1 <*> substV g vt2
  DsT d r -> return (DsT d r)
  ExVT a -> case existStatusV a g of
    ExBound t -> substV g t
    ExUnBound -> return $ ExVT a
    ExNonExist -> terr $ TOther "SubstV failed; missing exvar."

substC :: (RefDomain d) => Context d -> CompT d -> TErr d (CompT d)
substC g = \case
  RetT vt -> RetT <$> substV g vt
  ProdT pp -> do
    let pl = M.toList pp
    pl' <- mapM (\(i,mt) -> (,) <$> return i <*> substC g mt) pl
    return $ ProdT (M.fromList pl')
  FunT mx vt mt -> FunT mx <$> substV g vt <*> substC g mt
  ExCT b -> case existStatusC b g of
    ExBoundC mt -> substC g mt
    ExUnBoundC -> return $ ExCT b
    ExNonExistC -> terr $ TOther "SubstC failed; missing comp-exvar."
  Idx a s m -> Idx a s <$> substC g m

substC' :: (RefDomain d)
        => Getting (Context d) s (Context d)
        -> CompT d
        -> StateT s (TErr d) (CompT d)
substC' l mt = do g <- use l
                  lift $ substC g mt

onSnd :: (a -> b) -> (c1,c2,a) -> (c1,c2,b)
onSnd f (c1,c2,a) = (c1,c2,f a)

inb42 :: (RefDomain d) => ExIdC -> Context d -> TErr d (ExIdV,ExIdC,Context d)
inb42 b = \case
  Empty -> terr $ TOther ("inb42 failed; " 
                          ++ show b 
                          ++ " was not in context.")
  ExValT e g' -> onSnd (ExValT e) <$> inb42 b g'
  ExCompT (ExC (ExIdC b1) Nothing) g' | b == ExIdC b1 ->
    let eTmp = exTypeIdSub b1
        aNew = ExIdV (eTmp)
        bNew = ExIdC (exTypeIdNext eTmp)
        gNew = ExCompT
                 (ExC (ExIdC b1) (Just $ FunT Nothing (ExVT aNew) (ExCT bNew)))
                 (ExCompT
                    (ExC bNew Nothing)
                    (ExValT
                       (ExV aNew Nothing)
                       g'))
    in return (aNew, bNew, gNew)
  ExCompT e g' -> onSnd (ExCompT e) <$> inb42 b g'
  VarBind x vt g' -> onSnd (VarBind x vt) <$> inb42 b g'

quantifyContext :: (RefDomain d) 
  => Context d 
  -> Symbolic (Map IVarId (AnySym d, Refinement d))
quantifyContext Empty = return mempty
quantifyContext (ExValT (ExV (ExIdV a) (Just vt)) g) = do
  ex <- case vt of
          DsT t _ -> mkSym (show a) t
  m <- quantifyContext g
  return $ M.insert (IVarId (show (ExIdV a))) (ex,RefTrue) m
quantifyContext (ExValT _ g) = quantifyContext g
quantifyContext (ExCompT _ g) = quantifyContext g
quantifyContext (VarBind (VarId x) vt g) = do
  (ex,r) <- case vt of
              DsT t r -> do ex <- mkSym x t
                            return (ex,r)
  m <- quantifyContext g
  return $ M.insert (IVarId x) (ex,r) m
-- quantifyContext (IdxBind (IVarId s) _ g) = do 
--   a <- forall s
--   m <- quantifyContext g
--   return $ M.insert (IVarId s) a m
-- quantifyContext (ExIdx (IVarId s) _ g) = do 
--   a <- exists s
--   m <- quantifyContext g
--   return $ M.insert (IVarId s) a m
