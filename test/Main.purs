module Test.Main where

import Test.Definition (Example(..),runExample)
import Prelude (bind, pure,($), (-))
import Data.Unit (Unit,unit)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (range)
import Data.Traversable (for)
import Data.List (List)
import Data.Map (toUnfoldable)

import Effect (Effect)
import Effect.Class.Console (log,logShow)

import Common (Modulename)

import CounterExamples (getCex, numCex)
import StandardLib (libMap) 


runCounterExamples :: Effect Unit
runCounterExamples = do
  let cexInds :: Array Int 
      cexInds = range 0 (numCex-1)
  cexStrs <- for cexInds (\i -> pure $ Tuple (CounterExample i) (getCex i))
  ress <- for cexStrs (\(Tuple ex src) -> pure $ runExample ex src true) 
  _ <- for ress (\res -> logShow res)
  pure unit

runExamples :: Effect Unit
runExamples = do
  let libList :: List (Tuple Modulename String)
      libList = toUnfoldable libMap
  ress <- for libList (\(Tuple mn src) -> pure $ runExample (StdLib mn) src false)
  _ <- for ress (\res -> logShow res)
  pure unit


main :: Effect Unit
main = do
  _ <- log ""
  _ <- log "========================================================="
  _ <- log "=================Testing Counterexamples================="
  _ <- log "========================================================="
  _ <- log ""
  _ <- runCounterExamples 
  _ <- log ""
  _ <- log ""
  _ <- log "========================================================="
  _ <- log "==================Testing Standard Library==============="
  _ <- log "========================================================="
  _ <- log ""
  runExamples
