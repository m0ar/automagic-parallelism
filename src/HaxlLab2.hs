{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

module HaxlLab (main, mainHaxl) where

import Haxl.Core
import Data.Hashable (Hashable (..))
import Control.Monad.Par (runPar)
import Control.Monad.Par.Combinator (parMapM)
import Control.DeepSeq (rnf, NFData)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (foldl')
import Weights

data Heavy a where
  MockA :: Heavy Integer
  MockB :: Heavy Integer
  MockC :: Heavy Integer
  MockD :: Heavy Integer
  MockE :: Heavy Integer

-- Perform all requests to the data source in parallel using
-- the Par monad. Haxl will batch all independent requests to
-- this data source together in the variable reqs.
instance DataSource () Heavy where
  fetch _ _ _ reqs = SyncFetch $ runPar $ do
    parMapM (\(BlockedFetch req var) ->
      return $ runHeavy req var) reqs
    -- SyncFetch constructor requires a result of type IO (),
    -- hence the double pure. This does not mean we cannot return
    -- data; results are passed through an IORef in runHeavy.
    pure (pure ())

-- By design of Haxl we don't care about the order our effects
-- occur, other than for the dependencies which the library
-- already does in batches. Therefore, it is not a problem to
-- perform our IO _now_, independently of the rest of the IO
-- operations.
instance NFData (IO ()) where
  rnf = unsafePerformIO


-- Defines the semantics of our two actual "data source"
-- operations. This can be any type of computation, enabling
-- concurrency of other things than regular data fetching.
runHeavy :: Heavy a -> ResultVar a -> IO ()
runHeavy MockA var = do
  putStrLn "MockA started."
  let !n = foldl' (+) 0 [1..80000000] :: Integer
  putSuccess var n
  putStrLn "MockA finished."
runHeavy MockB var = do
  putStrLn "MockB started."
  let !n = foldl' (+) 0 [1..80000000] :: Integer
  putSuccess var n
  putStrLn "MockB finished."
runHeavy MockC var = do
  putStrLn "MockC started."
  let !n = foldl' (+) 0 [1..80000000] :: Integer
  putSuccess var n
  putStrLn "MockC finished."
runHeavy MockD var = do
  putStrLn "MockD started."
  let !n = foldl' (+) 0 [1..80000000] :: Integer
  putSuccess var n
  putStrLn "MockD finished."

-- Initiates an (empty) state and runs a Haxl computation.
-- Thanks to ApplicativeDo, the two dataFetch calls desugars
-- to applicative operations because they have no dependency,
-- and are performed concurrently by the Haxl framework.
--{-# ANN dataFetchMockA (Weight 10) #-}
dataFetchMockA :: GenHaxl () Integer
dataFetchMockA = dataFetch MockA

--{-# ANN dataFetchMockB (Weight 10) #-}
dataFetchMockB :: Integer -> GenHaxl () Integer
dataFetchMockB _ = dataFetch MockB

--{-# ANN dataFetchMockC (Weight 20) #-}
dataFetchMockC :: GenHaxl () Integer
dataFetchMockC = dataFetch MockC

dataFetchMockD :: Integer -> GenHaxl () Integer
dataFetchMockD _ = dataFetch MockD


main :: IO ()
main = mainHaxl

mainHaxl :: IO ()
mainHaxl = do
  env <- initEnv initialState ()
  summed <- runHaxl env $ do
    --(x1,x2) <- (,) <$> dataFetchMockA <*> dataFetchMockB
    --(x3,x4) <- (,) <$> dataFetchMockC x1 <*> dataFetchMockD x1
    x1 <- dataFetchMockA
    x2 <- dataFetchMockB x1
    x3 <- dataFetchMockC
    --x4 <- dataFetchMockD x1
    return $ show (x1, x2, x3)
  putStrLn $ "Result = " ++ show summed


initialState :: StateStore
initialState = stateSet NoState stateEmpty

-- Required & not interesting instances below

deriving instance Show (Heavy a)
deriving instance Eq   (Heavy a)

instance StateKey Heavy where
  data State Heavy = NoState

instance Hashable (Heavy a) where
  hashWithSalt salt _ = hashWithSalt salt ()

instance DataSourceName Heavy where
  dataSourceName _ = "Heavy"

instance ShowP Heavy where
  showp _ = "Heavy"
