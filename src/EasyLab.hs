{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

module EasyLab (mainIO) where
import Weights


mainIO :: IO ()
mainIO = do
    x1 <- a
    x2 <- b x1 -- Weight 4
    x3 <- c
    x4 <- d x3
    x5 <- e x1 x4 -- Weight 2
    print (x2,x5)


a :: IO Int
a = pure 3

{-# ANN b (Weight 4) #-}
b :: Int -> IO Int
b x = pure $ 2 + x

c :: IO Int
c = pure 1

d :: Int -> IO Int
d x = pure $ 4 - x

{-# ANN e (Weight 2) #-}
e :: Int -> Int -> IO Int
e x y= pure $ x * y
