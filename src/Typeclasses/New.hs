module Typeclasses.New
  ( Pair(..) )
  {-
    The newtype keyword is for taking existing types and wrapping them in new types, mostly so that it's easier to make them instances of certain type classes.
    When we use newtype to wrap an existing type, the type that we get is separate from the original type.
    In practice, you can think of newtype declarations as data declarations that can only have one constructor and one field.
    If you catch yourself writing such a data declaration, consider using newtype.

    If we make the following newtype:

      newtype CharList = CharList { getCharList :: [Char] }

    We can't use ++ to put together a "CharList" and a "list" of type [Char].
    We can't even use ++ to put together two "CharLists", because ++ works only on lists and the "CharList" type isn't a "list"
    We can however,
      * convert two "CharLists" to "lists",
      * then ++ them and
      * then convert that back to a "CharList".

    "newtpe" is lazily evaluated, while "data" is evluated eagerly.
    This can have implications if, say for example we wanted to let undefined values through:

      
-- example with eager evaluation
      data MyBool = MyBool { getValue :: Bool }

      helloMe :: MyBool -> String
      helloMe (MyBool _) = "hello to you anyway"

      ghci> helloMe undefined  -- will be evaluated right away and throw an exception
      "*** Exception: Prelude.undefined

      
-- example with lazy evaluation, via "newtype"
      newtype MyBool = MyBool { getValue :: Bool }

      ghci> helloMe undefined  -- will not be evaluated
      "hello to you anyway"
-}
   where

-- flip the order of the tuple item
newtype Pair l r = Pair
  { getPair :: (r, l)
  } deriving (Eq, Show)

instance Functor (Pair p)
 where
  fmap f (Pair (_1, _2)) = Pair (f _1, _2)


-- fmap (+3) (1,1)                      = (1, 4)
-- getPair  (fmap (+3)   (Pair(1,1)))    = (4, 1)
-- getPair  (fmap (+3) $  Pair(1,1))    = (4, 1)
-- getPair $ fmap (+3) $  Pair (1,1)     = (4, 1)
 