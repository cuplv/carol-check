module Main where

import Language.Carol.AST
import Language.Carol.Parse
import Language.Carol.Prelude
import Language.Carol.TypeCheck

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [testGroup "Core"
     [(testCase "Double App"
       . checks
       $ "  {=1}` {=2}` |x2|  \
         \              |x1|  \
         \  return (x1,x2)    " |:- RetT (PairT unit1T unit2T))

     ,(testCase "Function"
       . checks
       $ "|asdf1234| return asdf1234 : {}" |:- FunT unitT (RetT unitT))

     ,(testCase "Interleaved Double App"
       . checks
       $ "  {=1}` |x1|      \
         \  {=2}` |x2|      \
         \  return (x1,x2)  " |:- RetT (PairT unit1T unit2T))

     ,(testCase "Bind3" . checks $
         "return {=1} to x| {=2}` |y| return (x,y)"
         |:- RetT (PairT unit1T unit2T))

     ,(testCase "Bind4" . checks $
         "return {=1} to x| {=2}` |y| return (y,x)"
         |:- RetT (PairT unit2T unit1T))

     ,(testCase "ApFunApFun" . checks $
         "{=1}` |x| {=2}` |y| return (x,y)"
         |:- RetT (PairT unit1T unit2T))
     ]
  ,testGroup "ApApFunFun"
     [(testCase "Fun2" . checks $
         "{=1}` {=2}` |x2| |x1| return (x2,x1)"
         |:- RetT (PairT unit2T unit1T))
     ,(testCase "Fun3" . checks $
         "{=3}` {=2}` {=1}` |x1||x2||x3| return (x3,x1)"
         |:- RetT (PairT unit3T unit1T))
     ,(testCase "Fun4" . checks $
         "{=3}`{=2}`{=1}`{=1}`|x1||y1||x2||x3| return (x3,x1)"
         |:- RetT (PairT unit3T unit1T))
     ,(testCase "Fun5" . checks $
         "{=3}`{=2}`{=1}`{=1}`{=2}`|x2||x1||y1||y2||x3| return (x3,y1)"
         |:- RetT (PairT unit3T unit1T))
     ,(testCase "Fun6" . checks $
         "  {=1}`{=2}`{=3}`{=1}`{=3}`{=1}`  \
         \  |x1||x3||y1||y3||x2||z1|  \
         \  return (x2,x3)"
         |:- RetT (PairT unit2T unit3T))
     ,(testCase "Bind1" . checks $
         "{=3}` return {=1} to x| |y| return (x,y)"
         |:- RetT (PairT unit1T unit3T))
     ,(testCase "Bind2" . checks $
         "{=3}` return {=1} to x| |y| return (y,x)"
         |:- RetT (PairT unit3T unit1T))
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
        ,testCase "FunType1"
         . checks
         $ "|x| return (x,{=})" |:- FunT (intTR 4 6)
                                         (RetT (PairT (intTR 4 8) UnitT))
        ,t "FunType2"
           misses $ "|x| return x" |:- FunT (intTR 0 9) (RetT (intTR 4 8))
        ,testCase "FunType3"
         . checks
         $ let t = fori "ax"
                        intSort
                        (FunT (intTEq (IVarId "ax"))
                              (RetT (PairT (intTEq (IVarId "ax")) UnitT)))
           in "|x| return (x,{=})" |:- t
        ,testCase "FunType4"
         . misses
         $ let t = fori "ax"
                        intSort
                        (FunT (intTEq (IVarId "ax"))
                              (RetT (PairT (intTEq (IVarId "ax")) UnitT)))
           in "|x| return (5,{=})" |:- t
        ]
  ,testGroup "CompSub"
     [(testCase "BaseAdd" . checks $
         "add 1, 3 as x| return x" |:- RetT intT)
     ,(testCase "BaseAddM". misses $
         "add 1, 3 as x| return x" |:- RetT unitT)
     ,(testCase "BaseAddM2" . misses $
         "add 1, {=} as x| return x" |:- RetT intT)
     ,(testCase "RefAdd" . checks $
         let t = (RetT (intTR 4 4))
         in "add 1, 3 as x| return x" |:- t)
     ,(testCase "RefAddM" . misses $
         let t = (RetT (intTR 6 6))
         in "add 1, 3 as x| return x" |:- t)
     ]
  ]

(-:-) :: Comp' -> CompT' -> IO Comp'
(-:-) m mt = return $ AnnoC m mt

(|:-) :: String -> CompT' -> IO Comp'
(|:-) s mt = pComp s >>= (\m -> m -:- mt)

checks :: IO Comp' -> Assertion
checks em = do
  result <- runTErr . closedC =<< em
  case result of
    Right _ -> return ()
    Left e -> assertFailure (pretty e)

misses :: IO Comp' -> Assertion
misses = misses' (const True)

misses' :: (TypeError StdVD -> Bool) -> IO Comp' -> Assertion
misses' pred em = do
  result <- runTErr . closedC =<< em
  case result of
    Left e | pred e -> return ()
    Left e -> assertFailure $ "Wrong kind of check failure: " ++ pretty e
    Right t -> assertFailure $ "Should have failed, but got " ++ pretty t

pComp :: String -> IO Comp'
pComp s = case parseComp s of
            Right m -> return m
            Left e -> assertFailure (show e)
