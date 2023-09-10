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
  ) where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a -> a
largestDivisible divisor = head (filter p [100000,99998 ..])
  where
    p x = x `mod` divisor == 0

factorial' :: (Num i, Ord i) => i -> i
factorial' number
  | number <= 0 = 1
  | otherwise = number * factorial' (number - 1)

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

twoRowComposition :: (Integral a ) => [a]
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
  | odd n = n : chain (n * 3 + 1)

calculateBMIs :: (RealFloat a) => [(a, a)] -> [a]
calculateBMIs xs = [bmi | (weight, height) <- xs, let bmi = weight / height ^ 2]

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs
