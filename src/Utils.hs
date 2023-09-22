module Utils
  ( quicksort
  , largestDivisible
  , factorial'
  , flip'
  , addThreeNumbers
  , numChainsWhere
  , zipWithLambda
  , twoRowComposition
  , oddSquareSumComposition
  , oddSquareSumBindings
  , reversePolishNotation
  ) where

import Control.Monad

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a -> a
largestDivisible divisor = head (filter p [100000,99999 ..])
  where
    p x = x `mod` divisor == 0

factorial' :: (Num i, Ord i) => i -> i
factorial' number
  | number <= 0 = 1
  | otherwise   = number * factorial' (number - 1)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

addThreeNumbers :: (Num a) => a -> a -> a -> a
addThreeNumbers = \x y z -> x + y + z

numChainsWhere :: (Int -> Bool) -> Int
numChainsWhere paf = length (filter predicate (map chain [1 .. 100]))
  where
    predicate xs = paf (length xs)

zipWithLambda :: (Num a) => (a -> a -> a) -> [a]
zipWithLambda w = zipWith w [1, 2, 3, 4, 5] [5, 4, 3, 2, 2]

twoRowComposition :: (Integral a) => [a]
twoRowComposition = filter (\n -> even n) . takeWhile (<= 20) $ [1,2 ..]

oddSquareSumComposition :: Integer
oddSquareSumComposition =
  sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]

oddSquareSumBindings :: Integer
oddSquareSumBindings =
  let oddSquares = filter odd $ map (^ 2) [1 ..]
      belowLimit = takeWhile (< 10000) oddSquares
   in sum belowLimit

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n  = n : chain (n * 3 + 1)

calculateBMIs :: (RealFloat a) => [(a, a)] -> [a]
calculateBMIs xs = [bmi
 | (weight, height) <- xs, let bmi = weight / height ^ 2]

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs


-- accumulated thus far -> current item -> updated accumulated
flHandler :: [Double] -> String -> Maybe [Double]
flHandler (x:y:ys) "*"   = Just ((x * y) : ys)
flHandler (x:y:ys) "+"   = Just ((x + y) : ys)
flHandler (x:y:ys) "-"   = Just ((y - x) : ys)
flHandler (x:y:ys) "/"   = Just ((y / x) : ys)
flHandler (x:y:ys) "^"   = Just ((y ** x) : ys)
flHandler (x:xs) "ln"    = Just (log x : xs)
flHandler xs "sum"       = Just ([sum xs])
flHandler xs currentItem =  fmap (:xs) (readMaybe currentItem) 

readMaybe :: (Read a) => String -> Maybe a
readMaybe str =
  case (reads str) of -- :t reads :i ReadS  | type ReadS a = String -> [(a, String)]
    [(x, "")] -> Just x
    _         -> Nothing

reversePolishNotation :: String -> Maybe Double
--reversePolishNotation items =  head (foldl flHandler [] (words items))
--reversePolishNotation = head . foldl flHandler [] . words
reversePolishNotation  st = do  
    [result] <- foldM flHandler [] (words st)  
    return result