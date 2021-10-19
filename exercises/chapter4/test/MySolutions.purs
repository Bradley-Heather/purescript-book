module Test.MySolutions where

import Prelude

import Data.Path
import Data.Array  
import Data.Foldable        as F 
import Control.Alternative  (guard)
import Data.Maybe           (Maybe(..))

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean 
isEven x = x `mod` 2 == 0

countEven :: Array Int -> Int 
countEven n = length $ filter isEven n

squared :: Array Number -> Array Number 
squared n = map (\x -> x * x) n

keepNonNegative :: Array Number -> Array Number 
keepNonNegative n = filter (\x -> x >= 0.0) n

infix 6 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number 
keepNonNegativeRewrite n = (\x -> x >= 0.0) <$?> n

factors' :: Int -> Array (Array Int)
factors' n = do 
   i <- 1 .. n
   j <- i .. n
   guard $ i * j == n 
   pure [i, j]

isPrime :: Int -> Boolean 
isPrime n = (length $ factors' n) == 1 && n /= 1

cartesianProduct :: forall a. Eq a => Show a => Ord a => Array a -> Array a -> Array (Array a)
cartesianProduct array1 array2 = do 
       x <- array1 
       y <- array2
       pure [x, y]

triples :: Int -> Array (Array Int) 
triples n = do 
     a <- 1 .. n 
     b <- a .. n 
     c <- 1 .. n
     guard $ a * a + b * b == c * c
     pure [a, b, c] 

primeFactors :: Int -> Array Int
primeFactors n = factor 2 n 
   where 
     factor _ 1 = []
     factor a b =
        if b `mod` a == 0 
          then a : factor a (b / a)
          else factor (a + 1) b

allTrue :: Array Boolean -> Boolean 
allTrue n = F.foldl (==) true n 

fibTailRec :: Int -> Int 
fibTailRec n = fib' n 2 0 1
   where 
     fib' num acc a1 a2 =
      if num == 0 
        then 0
      else if num == 1
        then 1
      else if num == acc 
        then a1 + a2 
      else fib' num (acc + 1) a2 (a1 + a2)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

allTheFiles :: Path -> Array Path
allTheFiles file = file : do
  child <- ls file
  allTheFiles child

onlyFiles :: Path -> Array Path 
onlyFiles d = filter (not isDirectory) $ allTheFiles d

whereIs :: Path -> String -> Maybe Path 
whereIs p fn = head $ do 
     file  <- allTheFiles p 
     child <- ls file
     guard $ filename child == filename file <> fn
     pure file 

largestSmallest :: Path -> Array Path 
largestSmallest p = foldl go [] (onlyFiles p) 
   where 
     go :: Array Path -> Path -> Array Path
     go [largest, smallest] next 
        | size next > size largest  = [next, smallest] 
        | size next < size smallest = [largest, next]
        | otherwise                 = [largest, smallest] 
     go [first] second 
        | size first > size second  = [first, second]
        | otherwise                 = [second, first]
     go empty first                    = first : empty