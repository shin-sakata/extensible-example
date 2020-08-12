module Main where

import Data.Extensible.Effect

import Culc
import RunCulc

main :: IO ()
main = print $ leaveEff $ runCulc culculator