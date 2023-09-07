module Typeclasses.RecursiveDS
  ( List(..)
  , head'
  , tail'
  , reverse'
  , Tree(..)
  , treeInsert
  , treeElem
  ) where


--
-- Recursive data structures
--
infixr 5 :+

data List a
  = None
  | a :+ (List a)
  deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: List a -> List a -> List a
None .++ ys      = ys
(x :+ xs) .++ ys = x :+ (xs .++ ys)

reverse' :: List a -> List a
reverse' None      = None
reverse' (x :+ xs) = reverse' xs .++ x :+ None

head' :: List a -> a
head' None     = error " cannot on empty"
head' (x :+ _) = x

tail' :: List a -> List a
tail' None      = error " cannot on empty"
tail' (_ :+ xs) = xs


-- Tree of "any" type
data Tree a
  = EmptyTree
  | Node (Tree a) a (Tree a)
  deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton value = Node EmptyTree value EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert value EmptyTree = singleton value
treeInsert value (Node left a right)
  | value == a = Node left value right
  | value < a  = Node (treeInsert value left) a right
  | value > a  = Node left a (treeInsert value right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem value EmptyTree = False
treeElem value (Node left a right)
  | value == a = True
  | value < a  = treeElem value left
  | value > a  = treeElem value right
