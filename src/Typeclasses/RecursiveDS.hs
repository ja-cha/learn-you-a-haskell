module Typeclasses.RecursiveDS( CustomList(..)
  , head'
  , tail'
  , reverse'
  , CustomTree(..)
  , treeInsert
  , treeElem
  ) where

-- Make our CustomList an instance of the Functor type class
instance Functor CustomList where
  fmap = map'

--
-- Recursive data structures
--
infixr 5 :+

data CustomList a
  = None
  | a :+ (CustomList a)
  deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: CustomList a -> CustomList a -> CustomList a
None .++ ys      = ys
(x :+ xs) .++ ys = x :+ (xs .++ ys)

reverse' :: CustomList a -> CustomList a
reverse' None      = None
reverse' (x :+ xs) = reverse' xs .++ x :+ None

head' :: CustomList a -> a
head' None     = error " cannot on empty"
head' (x :+ _) = x

tail' :: CustomList a -> CustomList a
tail' None      = error " cannot on empty"
tail' (_ :+ xs) = xs

map' :: (a -> b) -> CustomList a -> CustomList b 
map' f None = None
map' f (x :+ xs) = f x :+ map' f xs


-- CustomTree of "any" type
data CustomTree a
  = EmptyTree
  | Node (CustomTree a) a (CustomTree a)
  deriving (Show, Read, Eq)

singleton :: a -> CustomTree a
singleton value = Node EmptyTree value EmptyTree

treeInsert :: (Ord a) => a -> CustomTree a -> CustomTree a
treeInsert value EmptyTree = singleton value
treeInsert value (Node left a right)
  | value == a = Node left value right
  | value < a  = Node (treeInsert value left) a right
  | value > a  = Node left a (treeInsert value right)

treeElem :: (Ord a) => a -> CustomTree a -> Bool
treeElem value EmptyTree = False
treeElem value (Node left a right)
  | value == a = True
  | value < a  = treeElem value left
  | value > a  = treeElem value right
