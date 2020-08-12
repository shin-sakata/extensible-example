module RunCulc where

import Data.Extensible
import Data.Extensible.Effect

import Culc

-- EffectsからCulcの文脈を解釈して計算する
runCulc :: Eff (Culc ++ xs) a -> Eff xs a
runCulc = runMinus . runPlus

-- plusの解釈 (解釈は自由)
runPlus :: Eff (Plus : effs) a -> Eff effs a
runPlus eff = peelAction0 (\a b -> pure $ a + b) eff

-- minusの解釈 (解釈は自由)
runMinus :: Eff (Minus : effs) a -> Eff effs a
runMinus eff = peelAction0 (\a b -> pure $ a - b) eff

-- calc plusとminusを利用することができる文脈
culculator :: HasCulc effs => Eff effs Int
culculator = do
    two <- 1 `plus` 1
    one <- two `minus` 1
    _ <- plusCulc -- 勿論これらも使える
    _ <- minusCulc
    return one

-- plus effを利用することができる文脈
plusCulc :: HasPlus effs => Eff effs Int
plusCulc = do
    two <- 1 `plus` 1
    return two

-- minus effを利用することができる文脈
minusCulc :: HasMinus effs => Eff effs Int
minusCulc = do
    zero <- 1 `minus` 1
    return zero
