module Test.MySolutions where

import Prelude
import Math (sqrt, pi)
import Data.Int (rem)

diagonal :: Number -> Number -> Number
diagonal a b = sqrt (a * a + b * b)

circleArea :: Number -> Number
circleArea r = pi * r * r

leftoverCents :: Int -> Int
leftoverCents p = p `rem` 100
