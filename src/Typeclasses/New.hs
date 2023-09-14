module Typeclasses.New
  ( Pair(..)
  ) where


-- flip the order of the tuple items
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
