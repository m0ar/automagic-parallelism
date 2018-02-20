{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}

module Test (mainTest) where

import Weights (Weight(..))
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe

{-# ANN mainTest (Weight 2) #-}
mainTest :: IO ()
mainTest = do 
    x <- pure 5 
    y <- linje
    return ()
    
{-# ANN subTest (Weight 5) #-}
subTest :: IO ()
subTest = do
    x <- linje
    y <- pure $ x + 3
    return ()

{-# ANN linje (Weight 3) #-}   
linje :: IO Integer
linje = pure 5   
