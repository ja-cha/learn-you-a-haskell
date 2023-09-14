module Typeclasses.Custom
  ( CustomEq(..)
  , TrafficLight(..)
  , Option(..)
  , Try(..)
  , Status(..)
  ) where


-- defining a new typeclass  called CustomEq
-- "e" is the type variable, a stand-in for the type that we will soon be making an instance of
class CustomEq e
 where
  (===) :: e -> e -> Bool
  (/==) :: e -> e -> Bool
  x === y = not (x /== y)
  x /== y = not (x === y)


-- we want all types of the form Maybe m to be part of the CustomEq typeclass
-- where what's contained inside the Maybe is also a part of CustomEq
instance (CustomEq m) => CustomEq (Maybe m)
 where
  Just x === Just y   = x === y
  Nothing === Nothing = True
  _ === _             = False

data TrafficLight
  = Red
  | Yellow
  | Green

instance CustomEq TrafficLight
 where
  Red === Red       = True
  Green === Green   = True
  Yellow === Yellow = True
  _ === _           = False

instance Show TrafficLight
 where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"

data Option a
  = Some a
  | None
  deriving (Show, Eq)

-- A Functor f provides a function "fmap" which, given any types a and b
-- lets you apply any function from (a -> b)  to turn a given functor f a into a functor f b
instance Functor Option
 where
  fmap = optionMap

optionMap :: (a -> b) -> Option a -> Option b
optionMap f None     = None
optionMap f (Some x) = Some (f x)

-- Making "Option" an instance of the "Applicative" typeclass so that we can map over functions that are in a "Functor" context
instance Applicative Option where  
    pure = Some  
    None <*> _ = None  
    (Some f) <*> something = fmap f something  

data Try a b
  = Failure a
  | Success b
  deriving (Show, Eq)

-- lets "partially" apply the Try type constructor, just like a function
instance Functor (Try a)
 where
  fmap = tryMap

-- we made "Try a" an instance of the Functor typeclass, not "Try a b".
-- That is because the Functor typeclass wants a type constructor that takes only one type parameter.
-- When we partially apply Try by using only "Try a", we satisfy the Functor requirement.
-- If fmap was specifically for "Try a", the type signature would then be
-- (b -> c) -> (Try a) b -> (Try a) c.
tryMap :: (b -> c) -> (Try a) b -> (Try a) c
tryMap f (Failure x) = Failure x
tryMap f (Success x) = Success (f x)
 

data Status a
  = Error | OK a 
  deriving (Show, Eq)

-- A Functor f provides a function "fmap" which, given any types a and b
-- lets you apply any function from (a -> b)  to turn a given functor f a into a functor f b
instance Functor Status
 where
  fmap = statusMap

statusMap :: (a -> b) -> Status a -> Status b
statusMap f Error  = Error
statusMap f (OK x) = OK (f x)
{- Function Composition
   [OK ((*2) . (+99) $ 1) , OK (  (*2) ((+99) 1) ) , OK ( 2 * ( 99 + 1) )]
   [OK 200, OK 200, OK 200]
 -}