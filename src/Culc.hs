module Culc
    ( Culc
    , Plus
    , Minus
    , plus
    , minus
    , HasCulc
    , HasPlus
    , HasMinus
    ) where

import           Data.Extensible
import           Data.Extensible.Effect.TH

decEffectSuite [d|
    data Culc a where
        Plus :: Int -> Int -> Culc Int
        Minus :: Int -> Int -> Culc Int
    |]

type HasCulc xs = IncludeAssoc xs Culc

type HasPlus xs = IncludeAssoc xs '[Plus]
type HasMinus xs = IncludeAssoc xs '[Minus]
