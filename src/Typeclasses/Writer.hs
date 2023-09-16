module Typeclasses.Writer
  ( multiLogDo
  , multiLogBind
  , greatestCommonDenominator
  )

   where

-- flip the order of the tuple items
import           Control.Monad.Writer
 
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multiLogDo :: Writer [String] Int
multiLogDo = do
  a <- logNumber 1
  b <- logNumber (a + 1)
  c <- logNumber (b + 1)
  return (c)

multiLogBind :: Writer [String] Int
multiLogBind =
  logNumber 1 >>= (\x -> logNumber (1 + x) >>= (\y -> logNumber (1 + y)))

 
greatestCommonDenominator :: Int -> Int -> Writer [String] Int  
greatestCommonDenominator a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        greatestCommonDenominator b (a `mod` b)  