module Main where

import Language.Carol.AST
import Language.Carol.Parse
import Language.Carol.TypeCheck

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [typeCase
     "Double app"
     "  5` +1`       |a|  \
     \               |x|  \
     \  mod a <- x as y|  \
     \  return y          "
     (RetT IntT)
  ,typeCase
     "Mod"
     "return +1"
     (RetT $ domModType IntD)
  ,typeCase
     "Function"
     "|asdf1234| return asdf1234 : {}"
     (FunT UnitT $ RetT UnitT)
  ,typeCase
     "Interleaved double app"
     "  +1`          |a|  \
     \  5`           |x|  \
     \  mod a <- x as y|  \
     \  return y          "
     (RetT IntT)
  ]

typeCase :: String -> String -> CompT -> TestTree
typeCase name s t =
  testCase name $ (t @=?) =<< typeOf s

typeOf :: String -> IO CompT
typeOf m = do
  prog <- pComp m
  case synthC prog emptyContext of
    Right (mt,g) -> case substC g mt of
      Right mt' -> return mt'
      Left e -> assertFailure e
    Left e -> assertFailure e

pComp :: String -> IO Comp'
pComp s = case parseComp s of
            Right m -> return m
            Left e -> assertFailure (show e)
