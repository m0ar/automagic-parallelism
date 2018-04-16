{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

module HaxlLab (mainHaxl, mainHaxl2, mainHaxl3) where

import Haxl.Core
import Data.Hashable (Hashable (..))
import Control.Parallel
import Control.Concurrent
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
  MockF :: Heavy Integer
  MockG :: Heavy Integer

-- Perform all requests to the data source in parallel using
-- the Par monad. Haxl will batch all independent requests to
-- this data source together in the variable reqs.
instance DataSource () Heavy where
  fetch _ _ _ = BackgroundFetch $ \(reqs :: [BlockedFetch Heavy]) ->
    mapM_ (\(BlockedFetch req var) ->
      forkIO $ runHeavy req var) reqs
  schedulerHint _ = SubmitImmediately

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
  let !n = foldl' (+) 0 [1..50000000] :: Integer
  putSuccess var n
  putStrLn "MockA finished."
runHeavy MockB var = do
  putStrLn "MockB started."
  let !n = foldl' (+) 0 [1..200000000] :: Integer
  putSuccess var n
  putStrLn "MockB finished."
runHeavy MockC var = do
  putStrLn "MockC started."
  let !n = foldl' (+) 0 [1..50000000] :: Integer
  putSuccess var n
  putStrLn "MockC finished."
runHeavy MockD var = do
  putStrLn "MockD started."
  let !n = foldl' (+) 0 [1..50000000] :: Integer
  putSuccess var n
  putStrLn "MockD finished."
runHeavy MockE var = do
  putStrLn "MockE started."
  let !n = foldl' (+) 0 [1..100000000] :: Integer
  putSuccess var n
  putStrLn "MockE finished."
runHeavy MockF var = do
  putStrLn "MockF started."
  let !n = foldl' (+) 0 [1..150000000] :: Integer
  putSuccess var n
  putStrLn "MockF finished."
runHeavy MockG var = do
  putStrLn "MockG started."
  let !n = foldl' (+) 0 [1..50000000] :: Integer
  putSuccess var n
  putStrLn "MockG finished."
-- Initiates an (empty) state and runs a Haxl computation.
-- Thanks to ApplicativeDo, the two dataFetch calls desugars
-- to applicative operations because they have no dependency,
-- and are performed concurrently by the Haxl framework.
dataFetchMockA :: GenHaxl () Integer
dataFetchMockA = dataFetch MockA

{-# ANN dataFetchMockB (Weight 4) #-}
dataFetchMockB :: Integer -> GenHaxl () Integer
dataFetchMockB _ = dataFetch MockB

dataFetchMockC :: GenHaxl () Integer
dataFetchMockC = dataFetch MockC

dataFetchMockD :: Integer -> GenHaxl () Integer
dataFetchMockD _ = dataFetch MockD

{-# ANN dataFetchMockE (Weight 2) #-}
dataFetchMockE :: Integer -> Integer -> GenHaxl () Integer
dataFetchMockE _ _ = dataFetch MockE

--{-# ANN dataFetchMockF (Weight 3) #-}
dataFetchMockF :: Integer -> GenHaxl () Integer
dataFetchMockF _ = dataFetch MockF

dataFetchMockG :: Integer -> Integer -> GenHaxl () Integer
dataFetchMockG _ _ = dataFetch MockG

mainHaxl :: IO ()
mainHaxl = do
  env <- initEnv initialState ()
  summed <- runHaxl env $ do
    x1 <- dataFetchMockA
    x2 <- dataFetchMockB x1
    x3 <- dataFetchMockC
    x4 <- dataFetchMockD x3
    x5 <- dataFetchMockE x1 x4
    pure $ show (x1, x2, x3, x4, x5)
  putStrLn $ "Result = " ++ show summed


mainHaxl2 :: IO ()
mainHaxl2 = do
  env <- initEnv initialState ()
  summed <- runHaxl env $ do
    x1 <- dataFetchMockA
    x2 <- dataFetchMockD x1
    x3 <- dataFetchMockB x1
    x4 <- dataFetchMockF x2
    x5 <- dataFetchMockE x3 x1
    x6 <- dataFetchMockC
    x7 <- dataFetchMockG x3 x5
    pure $ show (x1, x2, x3, x4, x5, x6)
  putStrLn $ "Result = " ++ show summed


mainHaxl3 :: IO ()
mainHaxl3 = do
  env <- initEnv initialState ()
  summed <- runHaxl env $ do
    x1 <- dataFetchMockA
    x2 <- dataFetchMockD x1
    x3 <- dataFetchMockB x1
    x4 <- dataFetchMockC
    x5 <- dataFetchMockF x2
    x6 <- dataFetchMockE x3 x5
    pure $ show (x1, x2, x3, x4, x5, x6)
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


-- TODO: Remove. This is for debugging only.
unsafePrint :: String -> ()
unsafePrint = unsafePerformIO . putStrLn
