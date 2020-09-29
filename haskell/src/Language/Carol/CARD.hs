module Language.Carol.CARD 
  ( CARD (..)
  ) where

import Language.Carol.AST.Types

data CARD = CARD 
  { cardStore :: ValT
  , cardEffect :: ValT
  , cardOrder :: ValT }
