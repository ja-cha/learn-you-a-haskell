module Typeclasses.Truthiness (Truthiness(..)) where

import Typeclasses.BinaryTreeFunctor(BinaryTree(..))
import Typeclasses.Custom(TrafficLight(..))

-- In JavaScript and some other weakly typed languages, you can put almost anything inside an if expression. 
--   For example you can do all of the following: 
--        if (0) alert("YEAH!") else alert("NO!")
--        if ("") alert ("YEAH!") else alert("NO!")
--        if (false) alert("YEAH") else alert("NO!")
--   All of these will throw an alert of NO!. 
--    If you do 
--        if ("WHAT") alert ("YEAH") else alert("NO!")
--    It will alert a "YEAH!" because JavaScript considers non-empty strings to be a sort of true-ish value.

class Truthiness a where  
    -- Notice that from the way we use the a in the function, a has to be a concrete type. 
    evalTruthiness :: a -> Bool          

-- Empty lists (and by extensions, strings) are a no-ish value, while non-empty lists are a yes-ish value.
instance Truthiness [a] where  
    evalTruthiness [] = False  
    evalTruthiness _ = True

instance Truthiness Bool where  
    -- "id" is a standard library function that takes a parameter and returns the same thing, which is what we would be writing here anyway.
    evalTruthiness = id            

instance Truthiness (Maybe a) where  
    evalTruthiness (Just _) = True  
    evalTruthiness Nothing = False    
 

instance Truthiness (BinaryTree a) where  
    evalTruthiness Empty = False  
    evalTruthiness _ = True  

instance Truthiness TrafficLight where  
    evalTruthiness Red = False  
    evalTruthiness _ = True      

instance Truthiness Int where  
    evalTruthiness 0 = False  
    evalTruthiness _ = True     