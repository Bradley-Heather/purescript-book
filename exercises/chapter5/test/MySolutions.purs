module Test.MySolutions where

import Prelude 
import Undefined

import Data.Picture     as DP
import ChapterExamples

import Data.Maybe
import Math as Math


factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1 
binomial 0 _ = 0
binomial n k 
    | n < k = 0 
    | otherwise = f n / ((f k) * (f (n - k)))
         where f = factorial

pascal :: Int -> Int -> Int 
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k 
    | n < k = 0
    | otherwise = pascal (n-1) k + pascal (n - 1) (k - 1)

----------------------------------
type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity {address: { city: a }} {address: { city: b}} = a == b 

fromSingleton :: forall a.  a -> Array a -> a 
fromSingleton default []  = default
fromSingleton default [a] = a
fromSingleton default _   = default 

-----------------------------------

circleAtOrigin :: DP.Shape 
circleAtOrigin = DP.Circle DP.origin n 
   where 
     n :: Number 
     n = 10.0

doubleScaleAndCenter :: DP.Shape -> DP.Shape 
doubleScaleAndCenter (DP.Circle c r)      = DP.Circle DP.origin (r * 2.0)
doubleScaleAndCenter (DP.Rectangle c w h) = DP.Rectangle DP.origin (w * 2.0) (h * 2.0)
doubleScaleAndCenter line@(DP.Line s e)   = DP.Line ((s - gcl) * dbl) ((e - gcl) * dbl)
      where gcl = DP.getCenter line 
            dbl = {x: 2.0, y: 2.0}
doubleScaleAndCenter (DP.Text loc s)      = DP.Text DP.origin s
doubleScaleAndCenter (DP.Clipped _ _ _ _) = undefined

shapeText :: DP.Shape -> Maybe String 
shapeText (DP.Text loc s) = Just s 
shapeText _               = Nothing

----------------------------------
newtype Watt = Watt Number 

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

----------------------------------
area :: DP.Shape -> Number
area (DP.Circle _ r)      = Math.pi * (r * r)
area (DP.Rectangle _ w h) = w * h
area (DP.Line _ _)        = 0.0
area (DP.Text _ _)        = 0.0
area (DP.Clipped _ _ _ _) = 0.0



