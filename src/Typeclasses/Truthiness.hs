module Typeclasses.Truthiness (Truthiness(..)) where

import Typeclasses.BinaryTreeFunctor(BinaryTree(..))
import Typeclasses.Custom(TrafficLight(..))

-- In weakly typed languages, you can put almost anything inside an if expression. 
--   For example in Javascript you can do all of the following: 
--        if (0) alert("Truthiness!") else alert("Falsiness!")
--        if ("") alert ("Truthiness!") else alert("Falsiness!")
--        if (false) alert("Truthiness") else alert("Falsiness!")
--   All of these will throw an alert of Falsiness!. 
--    If you do 
--        if ("WHAT") alert ("Truthiness") else alert("Falsiness!")
--    It will alert "Truthiness!" because JavaScript considers non-empty strings to be a sort of true-ish value.

class Truthiness a where  
    -- Notice that from the way we use the a in the function, a has to be a concrete type. 
    evalTruthiness :: a -> Bool          

-- Make an instance of the Truthiness type class for the [] data type
-- Empty lists (and by extensions, strings) are a no-ish value, while non-empty lists are a yes-ish value.
instance Truthiness [a] where  
    evalTruthiness [] = False  
    evalTruthiness _ = True

-- Make an instance of the Truthiness type class for the Bool data type
instance Truthiness Bool where  
    -- "id" is a standard library function that takes a parameter and returns the same thing, which is what we would be writing here anyway.
    evalTruthiness = id            

-- Make an instance of the Truthiness type class for the  Maybe data type
instance Truthiness (Maybe a) where  
    evalTruthiness (Just _) = True  
    evalTruthiness Nothing = False    

-- Make make an instance of the Truthiness type class for  our BinaryTree data type
instance Truthiness (BinaryTree a) where  
    evalTruthiness Empty = False  
    evalTruthiness _ = True  

-- Make an instance of the Truthiness type class for our TrafficLight data type
instance Truthiness TrafficLight where  
    evalTruthiness Red = False  
    evalTruthiness _ = True      

-- Make an instance of the Truthiness type class for the Int data type
instance Truthiness Int where  
    evalTruthiness 0 = False  
    evalTruthiness _ = True     