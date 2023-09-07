module Typeclasses.Custom (CustomEq(..), TrafficLight(..)) where

-- defining a new typeclass  called CustomEq
-- "equatable" is the type variable, a stand-in for the type that we will soon be making an instance of
class CustomEq equatable where
  (===) :: equatable -> equatable -> Bool
  (/==) :: equatable -> equatable -> Bool

  x === y = not (x /== y)
  x /== y = not (x === y)

-- we want all types of the form Maybe m to be part of the CustomEq typeclass
-- where what's contained inside the Maybe is also a part of CustomEq
instance (CustomEq m) => CustomEq (Maybe m) where  
    Just x === Just y = x === y  
    Nothing === Nothing = True  
    _ === _ = False  

data TrafficLight = Red | Yellow | Green  

instance CustomEq TrafficLight where  
    Red === Red = True  
    Green === Green = True  
    Yellow === Yellow = True  
    _ === _ = False  

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light" 


        