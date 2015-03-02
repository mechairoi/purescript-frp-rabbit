module Test.Main where

import Test.Unit
import Test.Unit.Console
import Data.Array
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import DOM
import FRP.Rabbit
import FRP.Rabbit.Handler
import FRP.Rabbit.Signal

record :: forall a e . RefVal [[a]] -> [a] -> Eff (ref :: Ref, dom :: DOM | e) Unit
record = \ref -> \n -> do v <- readRef ref
                          writeRef ref $ v `append` [n]

type TestEff e a = forall e . Eff (ref :: Ref, dom :: DOM, testOutput :: TestOutput | e) a

q f = do ref <- newRef []
         sigA <- createEventHandler
         sigB <- createEventHandler

         let testSig = (\a b -> [a,b]) <$> sigA.event <*> sigB.event

         runSignal testSig (record ref)

         f sigA.handler sigB.handler

         readRef ref

whenSignalsFire :: forall a e . (Eq a) =>
                   [[a]] ->
                  ( (a -> Eff (ref :: Ref, dom :: DOM | e) Unit) ->
                    (a -> Eff (ref :: Ref, dom :: DOM | e) Unit) ->
                    Eff (ref :: Ref, dom :: DOM | e) Unit
                  ) ->
                  ((Boolean -> Eff (ref :: Ref, dom :: DOM | e) Unit) -> Eff (ref :: Ref, dom :: DOM | e) Unit)
whenSignalsFire expected f = \done -> do res <- q f
                                         done (res == expected)

main = runTest do
         test "callbacks fire regardless of signal change order" do
               assertFn "signal fired in order a1, b2, a3" $ [[1,2],[3,2]] `whenSignalsFire` \a b -> do a 1
                                                                                                        b 2
                                                                                                        a 3

               assertFn "signal fired in order a1, b2, a3" $ [[1,2],[3,2]] `whenSignalsFire` \a b -> do b 2
                                                                                                        a 1
                                                                                                        a 3

         test "callbacks fire regardless of signal definition order" do
               assertFn "signal fired in order a1, b2, a3" $ [[1,2],[3,2]] `whenSignalsFire` \a b -> do a 1
                                                                                                        b 2
                                                                                                        a 3

               assertFn "signal fired in order b1, a2, b3" $ [[2,1],[2,3]] `whenSignalsFire` \a b -> do b 1
                                                                                                        a 2
                                                                                                        b 3
