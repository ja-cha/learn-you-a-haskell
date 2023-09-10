module Typeclasses.BinaryTreeFunctor
  ( BinaryTree(..)
  , treeInsert
  ) where


-- Make our BinaryTree an instance of the Functor type class
instance Functor BinaryTree
 where
  fmap = treeMap


-- BinaryTree type  constructor
data BinaryTree a
  = Empty
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Read, Eq)

singleton :: a -> BinaryTree a
singleton value = Node Empty value Empty

treeInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
treeInsert value Empty = singleton value
treeInsert value (Node left a right)
  | value == a = Node left value right
  | value < a  = Node (treeInsert value left) a right
  | value > a  = Node left a (treeInsert value right)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f Empty               = Empty
treeMap f (Node left a right) = Node (treeMap f left) (f a) (treeMap f right)
