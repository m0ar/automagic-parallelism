{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Haxl.Core
import Data.Hashable (Hashable (..))
import Control.Monad.Par (runPar)
import Control.Monad.Par.Combinator (parMapM)
import Control.DeepSeq (rnf, NFData)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (foldl')

data Heavy a where
  MockA :: Heavy Integer
  MockB :: Heavy Integer

-- By design of Haxl we don't care about the order our effects
-- occur, other than for the dependencies which the library
-- already does in batches.
instance NFData (IO ()) where
  rnf = unsafePerformIO

instance DataSource () Heavy where
  fetch _ _ _ reqs = SyncFetch $ runPar $ do
    parMapM (\(BlockedFetch req var) ->
      return $ runHeavy req var) reqs
    pure (pure ())

runHeavy :: Heavy a -> ResultVar a -> IO ()
runHeavy MockA var = do
  let !n = foldl' (+) 0 [1..100000000]
  putSuccess var n
  putStrLn "MockA finished."
runHeavy MockB var = do
  let !n = foldl' (+) 0 [1..100000000]
  putSuccess var n
  putStrLn "MockB finished."

main :: IO ()
main = do
  env <- initEnv initialState ()
  summed <- runHaxl env $ (+) <$> dataFetch MockA
                             <*> dataFetch MockB
  putStrLn $ "result = " ++ show summed


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
