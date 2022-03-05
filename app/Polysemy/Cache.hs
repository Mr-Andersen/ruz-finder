{-# LANGUAGE GADTs, TemplateHaskell #-}

module Polysemy.Cache where

import Polysemy
import Polysemy.State

import Data.Map (Map)
import Data.Map qualified as M

-- Cache without expiration
data Cache k v :: Effect where
    CInsert :: k -> v -> Cache k v m ()
    CLookup :: k -> Cache k v m (Maybe v)

makeSem ''Cache

memo :: Cache k v `Member` r
     => (k -> Sem r v) -> k -> Sem r v
memo f k =
    cLookup k >>= \case
        Just v -> pure v
        Nothing -> do
            v <- f k
            cInsert k v
            pure v

runCache :: forall k v r. (Ord k, State (Map k v) `Member` r) => InterpreterFor (Cache k v) r
runCache = interpret \case
    CInsert k v -> modify' (M.insert k v)
    CLookup k -> gets (M.lookup k)

runCache' :: forall k v r. Ord k => InterpreterFor (Cache k v) r
runCache' = evalState M.empty . runCache @k @v @(State (Map k v) ': r) . raiseUnder
