module Typeclasses.Algebraic
  ( Point(..)
  , Shape(..)
  , surface
  , Buddy(..)
  , makeBuddy
  , Person(..)
  , makePerson
  , Day(..)
  , phoneBook
  , PhoneBook
  , inPhoneBook
  , lockerLookup
  , lockers
  ) where

import qualified Data.Map as Map


-- A typeclass is a sort of interface that defines some behavior.
-- If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes.
-- Typeclass examples:  Eq, Ord, Enum, Bounded, Show, Read, Functor, Applicative
--
-- Example: the Int type is an instance of the Eq typeclass because the Eq typeclass defines behavior for stuff that can be equated.
-- And because integers can be equated, Int is a part of the Eq typeclass
--
-- The data keyword is for making your own data types. 
-- They can have as many constructors and fields as you wish and can be used to implement any algebraic data type by yourself. 
-- Everything from lists and Maybe-like types to trees.


-- Algebraic Type
data Point =
  Point Float Float
  deriving (Show)


-- Algebraic Type
data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)


--
-- When we derive the Eq instance for a type and then try to compare two values of that type with == or /=,
-- Haskell will see if the value constructors match (there's only one value constructor here though)
-- and then it will check if all the data contained inside matches by testing each pair of fields with ==.
-- The types of all the fields also have to be part of the Eq typeclass.
--
-- Algebraic Type
data Buddy =
  Buddy String String Int Float
  deriving (Eq, Show, Read)


-- Value Constructor using Record Syntax
data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  , height    :: Float
  , phone     :: String
  } deriving (Eq, Show, Read)

surface :: Shape -> Float
surface (Circle _ r)                            = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

makeBuddy :: String -> String -> Int -> Float -> Buddy
makeBuddy = Buddy

makePerson :: String -> String -> Int -> Float -> String -> Person
makePerson = Person


-- Because all the value constructors are nullary (take no parameters, i.e. fields), we can make it part of the Enum typeclass.
-- The Enum typeclass is for things that have predecessors and successors. We can also make it part of the Bounded typeclass,
-- which is for things that have a lowest possible value and highest possible value.
-- And while we're at it, let's also make it an instance of all the other derivable typeclasses and see what we can do with it.
data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


--  Type synonyms
--  Example: type String = [Char]
type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
  [ ("betty", "555-2938")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook


-- left most value is conidered the lower ranking type
data LockerState
  = Taken
  | Free
  deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map =
  case Map.lookup number map of
    Nothing -> Left $ "Locker number " ++ show number ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show number ++ " is already taken!"

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]
