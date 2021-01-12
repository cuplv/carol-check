module Main where

import Language.Carol.AST
import Language.Carol.Parse
import Language.Carol.TypeCheck

import Control.Monad.Except
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [testCase "Double app" 
   . checks
   $ "  5` +1`       |a|  \
     \               |x|  \
     \  mod a <- x as y|  \
     \  return y          " |:- RetT intT
  ,typeCase
     "Mod"
     "return +1"
     (RetT intModT)
  ,testCase "Mod2" $ checks ("return +1" |:- RetT intModT)
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
     (RetT intT)
  ,typeCase
    "Bind3"
    "return {=} to x| 5` |y| return (x,y)"
    (RetT (PairT UnitT intT))
  ,typeCase
    "Bind4"
    "return {=} to x| 5` |y| return (y,x)"
    (RetT (PairT intT UnitT))
  ,typeCase
    "ApFunApFun"
    "{=}` |x| 5` |y| return (x,y)"
    (RetT (PairT UnitT intT))
  ,testGroup "ApApFunFun"
     [typeCase
       "Fun2"
       "{=}` 5` |x| |y| return (x,y)"
       (RetT (PairT intT UnitT))
     ,typeCase
       "Fun3"
       "3` 1` {=}` |xU||x2||x3| return (x3,xU)"
       (RetT (PairT intT UnitT))
     ,typeCase
       "Fun4"
       "3` 1` {=}` {=}` |xU0||xU||x2||x3| return (x3,xU)"
       (RetT (PairT intT UnitT))
     ,typeCase
       "Fun5"
       "3` 1` {=}` {=}` 2` |x1||xU0||xU||x2||x3| return (x3,xU)"
       (RetT (PairT intT UnitT))
     ,typeCase
       "Fun6"
       "3` 1` {=}` {=}` 2` 8` |y||z||xU0||xU||x2||x3| return (x3,xU)"
       (RetT (PairT intT UnitT))
     ,typeCase
       "Bind1"
       "5` return {=} to x| |y| return (x,y)"
       (RetT (PairT UnitT intT))
     ,typeCase
       "Bind2"
       "5` return {=} to x| |y| return (y,x)"
       (RetT (PairT intT UnitT))
     ]
  ,testGroup "Specials"
     [typeCase
        "Query"
        "query LEQ as x| 5` |y| return (y,x)"
        (RetT (PairT intT intT))
     ,typeCase
        "Issue"
        "issue +2 | return {=}"
        (RetT UnitT)
     ,typeCase
        "Produce"
        "produce +2 | return {=}"
        (RetT UnitT)
     ,typeCase
        "Consume"
        "consume +2 | return {=}"
        (RetT UnitT)
     ,typeCase
        "ModIn1"
        "mod +1 <- 5 as x| True` |x| return x"
        (RetT boolT)
     ,typeCase
        "ModIn2"
        "True` mod +1 <- 5 as x| |x| return x"
        (RetT boolT)
     ,typeCase
        "ModOut"
        "True` |x| (mod +1 <- 5 as x| return {=}) to y| return x"
        (RetT boolT)
     ,typeCase
        "String"
        "return \"Hello\\nWorld!\""
        (RetT stringT)
     ,testCase "GetString"
      . checks
      $ "strget as x| strcat x, x as y| return y" |:- RetT stringT
     ,testCase "PutString"
      . checks
      $ "\"Hi.\" ` |x| strput x | return {=}" |:- RetT UnitT
     ]
  ,testGroup "Sub" $
     let t n c m = testCase n . c $ m
     in [t "SimpleRange1" 
           checks $ "return 5" |:- RetT (intTR 5 5)
        ,t "SimpleRange2" 
           checks $ "return 5" |:- RetT (intTR 4 6)
        ,t "SimpleRange3" 
           misses $ "return 5" |:- RetT (intTR 6 6)
        ,t "SimpleRange4" -- The ref in this case is always unsat
           misses $ "return 5" |:- RetT (intTR 10 2)
        ,t "Bound1" checks $ "return 5" |:- RetT (intTGe 0)
        ,t "Bound2" misses $ "return 5" |:- RetT (intTLe 0)
        ,t "FunRange1" 
           checks $ "5` |x| return x" |:- RetT (intTR 5 5)
        ,t "FunRange2" 
           checks $ "5` |x| return x" |:- RetT (intTR 4 6)
        ]
  ]

(-:-) :: Comp' -> CompT' -> IO Comp'
(-:-) m mt = return $ AnnoC m mt

(|:-) :: String -> CompT' -> IO Comp'
(|:-) s mt = pComp s >>= (\m -> m -:- mt)

closedCompT :: Comp' -> TErr StdVD CompT'
closedCompT comp = do
  (mt,g) <- synthC comp emptyContext
  substC g mt

checks :: IO Comp' -> Assertion
checks em = do
  result <- runExceptT . closedCompT =<< em
  case result of
    Right _ -> return ()
    Left e -> assertFailure (pretty e)

misses :: IO Comp' -> Assertion
misses = misses' (const True)

misses' :: (TypeError StdVD -> Bool) -> IO Comp' -> Assertion
misses' pred em = do
  result <- runExceptT . closedCompT =<< em
  case result of
    Left e | pred e -> return ()
    Left e -> assertFailure $ "Wrong kind of check failure: " ++ pretty e
    Right t -> assertFailure $ "Should have failed, but got " ++ pretty t

typeCase :: String -> String -> CompT' -> TestTree
typeCase name s t =
  testCase name $ (t @=?) =<< (baseTypeC <$> typeOf s)

refTypeCase :: String -> String -> CompT' -> TestTree
refTypeCase name s t =
  testCase name $ (t @=?) =<< typeOf s

typeOf :: String -> IO CompT'
typeOf m = do
  prog <- pComp m
  result <- runExceptT $ synthC prog emptyContext
  case result of
    Right (mt,g) -> do
      r' <- runExceptT $ substC g mt
      case r' of
        Right mt' -> return mt'
        Left e -> assertFailure (pretty e)
    Left e -> assertFailure (pretty e)

pComp :: String -> IO Comp'
pComp s = case parseComp s of
            Right m -> return m
            Left e -> assertFailure (show e)
