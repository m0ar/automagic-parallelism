{-# LANGUAGE DeriveDataTypeable #-}
module Weights (Weight(..)) where

import Data.Data (Data(..))
import Data.Typeable

newtype Weight = Weight Integer deriving (Data, Typeable)


