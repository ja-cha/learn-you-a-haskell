module Typeclasses.BinaryTreeFunctor
  ( BinaryTree(..)
  , treeInsert
  ) where

{-
   We can look at functors as things that output values in a context. 
   For instance, Just 3 outputs the value 3 in the context of Maybe, where it might output a value or no values at all.
   We can think of mapping over functors, using fmap, as attaching transformations to the outputs.
     fmap (+3) [1,2,3]
    (fmap (+3) (*3)) 1 == ((+3) . (*3)) 1 == (\x -> ((x*3)+3)) 1
    

   If a type obeys the functor laws, we know that calling fmap on a value of that type will only map the function over it, nothing more.

  1 Identity Law:
    Applying fmap with the identity function id to a functor should have no effect; it should return the same functor.
      
      fmap id == id

  2 Composition Law
    Applying a composed function f . g using fmap is equivalent to applying f and g separately using fmap
      fmap (f . g) == fmap f . fmap g
      
    The following statement is an instantiation of the Composition Law for a specific functor F.
      fmap (f . g) F = fmap f (fmap g F)
      example:
      fmap ((intersperse '-').(++"BT")) (Just "A") = fmap (intersperse '-') $ fmap (++"BT") (Just "A")    
      

-}

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
