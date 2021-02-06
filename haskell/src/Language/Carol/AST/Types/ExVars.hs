module Language.Carol.AST.Types.ExVars
  ( ExTypeId
  , exTypeIdInit
  , exTypeIdSub
  , exTypeIdNext
  , ExIdV (..)
  , ExIdC (..)
  ) where

import Language.Carol.Prelude.Types

data ExTypeId =
  ExTypeId Int (Maybe ExTypeId)
  deriving (Eq)

instance Show ExTypeId where
  show (ExTypeId n Nothing) = show n
  show (ExTypeId n (Just e)) = show n ++ "-" ++ show e

instance Pretty ExTypeId where
  pretty = show

instance Ord ExTypeId where
  e1 <= e2 = case (e1,e2) of
    (ExTypeId n1 me1, ExTypeId n2 me2) | n1 == n2 ->
      case (me1,me2) of
        (       _,  Nothing) -> True
        ( Nothing,        _) -> False
        (Just e1', Just e2') -> e1' <= e2'
    (ExTypeId n1 _, ExTypeId n2 _) -> n1 <= n2

-- | The initial 'ExTypeId'.
exTypeIdInit :: ExTypeId
exTypeIdInit = ExTypeId 0 Nothing

-- | Make a subordinate 'ExTypeId' from a pre-existing one.
exTypeIdSub :: ExTypeId -> ExTypeId
exTypeIdSub (ExTypeId n me) = ExTypeId n (Just e')
  where e' = case me of
               Just e -> exTypeIdSub e
               Nothing -> exTypeIdInit

-- | Make a fresh sibling 'ExTypeId' from a previous one.
exTypeIdNext :: ExTypeId -> ExTypeId
exTypeIdNext (ExTypeId n Nothing) =
  ExTypeId (n + 1) Nothing
exTypeIdNext (ExTypeId n (Just e)) =
  ExTypeId n (Just $ exTypeIdNext e)

newtype ExIdV = ExIdV ExTypeId deriving (Eq,Ord)

instance Show ExIdV where
  show (ExIdV e) = "<" ++ show e ++ ">"

instance Pretty ExIdV where
  pretty = show

newtype ExIdC = ExIdC ExTypeId deriving (Eq,Ord)

instance Show ExIdC where
  show (ExIdC e) = "<|" ++ show e ++ "|>"

instance Pretty ExIdC where
  pretty = show
