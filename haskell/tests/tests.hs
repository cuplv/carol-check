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
  ,typeCase
    "Bind3"
    "return {=} to x| 5` |y| return (x,y)"
    (RetT (PairT UnitT IntT))
  ,typeCase
    "Bind4"
    "return {=} to x| 5` |y| return (y,x)"
    (RetT (PairT IntT UnitT))
  ,typeCase
    "ApFunApFun"
    "{=}` |x| 5` |y| return (x,y)"
    (RetT (PairT UnitT IntT))
  ,testGroup "ApApFunFun"
     [typeCase
       "Fun2"
       "{=}` 5` |x| |y| return (x,y)"
       (RetT (PairT UnitT IntT))
     ,typeCase
       "Fun3"
       "3` {=}` 1` |x1||xU||x3| return (x1,xU)"
       (RetT (PairT IntT UnitT))
     ,typeCase -- Contrary to previous assumption, this case (and all
               -- the ApApFunFun style cases) does not work due to
               -- some logic missing from the typechecking process.
               -- These cases in fact do not depend on polymorphism in
               -- the type system.
       "Bind1"
       "5` return {=} to x| |y| return (x,y)"
       (RetT (PairT UnitT IntT))
     ,typeCase
       "Bind2"
       "5` return {=} to x| |y| return (y,x)"
       (RetT (PairT IntT UnitT))
     ]
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
