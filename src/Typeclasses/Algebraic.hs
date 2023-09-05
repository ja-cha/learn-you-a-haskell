module Typeclasses.Algebraic
  ( Point(..)
  , Shape(..)
  , surface
  , Buddy(..)
  , makeBuddy
  , Person(..)
  , makePerson
  ) where

-- Algebraic Type
data Point =
  Point Float Float
  deriving (Show)

-- Algebraic Type
data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

surface :: Shape -> Float
surface (Circle _ r)                            = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- Algebraic Type
data Buddy =
  Buddy String String Int Float
  deriving (Show, Eq)

makeBuddy :: String -> String -> Int -> Float -> Buddy
makeBuddy = Buddy

-- Record Type
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     ,phone :: String
                     } deriving (Show, Eq)

makePerson :: String -> String -> Int -> Float -> String -> Person
makePerson = Person                     