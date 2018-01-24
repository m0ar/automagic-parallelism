{-# LANGUAGE GADTs
           , TypeFamilies
           , MultiParamTypeClasses
           , StandaloneDeriving
           , DeriveDataTypeable
           , OverloadedStrings #-}

module HaxlLab where

import Haxl.Core
import Data.Hashable
import Control.Monad

data HeavyA a where
  MockA :: HeavyA Integer

data HeavyB a where
  MockB :: HeavyB Integer

deriving instance Show (HeavyA a)
deriving instance Eq   (HeavyA a)
deriving instance Show (HeavyB a)
deriving instance Eq   (HeavyB a)

instance DataSource () HeavyA where
  fetch _ _ _ reqs = SyncFetch $
    forM_ reqs $ \(BlockedFetch req var) -> runHeavyA req var

instance DataSource () HeavyB where
  fetch _ _ _ reqs = SyncFetch $
    forM_ reqs $ \(BlockedFetch req var) -> runHeavyB req var


runHeavyA :: HeavyA a -> ResultVar a -> IO ()
runHeavyA MockA var = do
  let !n = sum [1..20000000]
  putSuccess var n
  putStrLn "MockA finished."

runHeavyB :: HeavyB a -> ResultVar a -> IO ()
runHeavyB MockB var = do
  let !n = sum [1..20000000]
  putSuccess var n
  putStrLn "MockB finished."


initialState :: StateStore
initialState = stateSet NoStateB $ stateSet NoStateA stateEmpty

main :: IO ()
main = do
  env <- initEnv initialState ()
  summed <- runHaxl env $ (+) <$> dataFetch MockA <*> dataFetch MockB
  putStrLn $ "result = " ++ show summed


instance StateKey HeavyA where
  data State HeavyA = NoStateA

instance StateKey HeavyB where
  data State HeavyB = NoStateB

instance Hashable (HeavyA a) where
    hashWithSalt salt _ =
      hashWithSalt salt ()
instance Hashable (HeavyB a) where
    hashWithSalt salt _ =
      hashWithSalt salt ()

instance DataSourceName HeavyA where
  dataSourceName _ = "HeavyA"
instance DataSourceName HeavyB where
  dataSourceName _ = "HeavyB"

instance ShowP HeavyA where
  showp _ = "HeavyA"
instance ShowP HeavyB where
  showp _ = "HeavyB"
