##### First Principles

# Keywords 
* `data` is used for declaring data types 
* `class` is used for defining type classes
* `instance` is used for implementing type class instances for specific data types

  In conjunction, they provide mechanism for creating custom data structures, as well as defining polymorphic behavior for those structures.

  ---

  ## data
    * The `data` keyword is used to declare algebraic data types. 
    * Algebraic data types allow you to define your own custom data structures. 
    * You can create data constructors and specify the possible values a data type can take.
    * Left most data constructor has a value of the lowest ranking, the right one has the highest. This is useful for comparisons

      ```haskell
         data Color = Red | Green | Blue
      ```

  ---

  ## type class
  * The `class` keyword is used to declare type classes. 
  * is a way to define a set of behaviors or functions that can be implemented by one or more data types. 
  * Provides a way to define common behavior for different data types.
 
    ```haskell
       class Eq a where
          (==) :: a -> a -> Bool
    ```

  ---

  ## instance
    * The `instance` keyword is used to declare type class instances. 
    * When you define a type class, you can create instances for specific data types to implement the functions   
      defined by the type class.

      ```haskell
        instance Eq Color where
          Red == Red = True
          Green == Green = True
          Blue == Blue = True
          _ == _ = False
      ```

  ---

  ## type
    * The `type` keyword is used to define type synonyms or type aliases. 
    * It allows you to create alternative names for existing data types, which can make your code more expressive and readable.

      ```haskell
       type Phone = String
       type Age = Int
      ```

---

# Concepts
* `concrete data type`  has a fixed structure
* `data type constructor` is a parameterized data type
* `kinds` classification system

  ---
  
  ## concrete data type
    
    * a concrete type is a type that doesn't take any type parameters
    * is a data type that represents values with a fixed, specific structure and behavior.
    * instances of concrete data types are actual values with defined characteristics.
      - **predefined** data types: `Int`, `Bool`, 
      - **custom** data types are defined using the `data` keyword, such as `data Color = Red | Green | Blue`
    * are used to create values and perform operations specific to those types

  ## data type constructor

    * is a parameterized data type that can accept type arguments to create new, parameterized types.
    * used to create more abstract or polymorphic data types that work with different types of values.
      - **predefined** data type constructors: `[]` (to create lists), `(->)` (to represent function types).<br>
        Other exmaples include `Maybe a` or `Either a b`
      - **custom** data type constructors are defined using the `data` keyword, such as 
        ```haskell
          data BinaryTree a = Empty | Node (BinaryTree a) a (BinaryTree a)
        ```
  ## kind

    * classification system for types and type constructors. 
    * analogous to types of types and are used to describe type-level properties of types and type constructors.
    * similar to how types classify values.
      - Just as values have types (e.g., Int, String), types themselves have kinds. 
      - The kind of a type or type constructor specifies how many type arguments it takes and the kinds of those arguments.


  `*` (Star Kind)
    * This is the `kind` of types that represent concrete types. 
      - For example, `Int` and `String` have kind `*`

  `* -> *` (Arrow Kind)
    * This `kind` represents type constructors that take one type argument to produce another type. 
      - For example, the `Maybe` type constructor has kind `* -> *` because it takes a concrete type (e.g., `Int`) and produces another concrete type (e.g., `Int`).

  `* -> * -> *` (Higher-Order Kind)
    * Some type constructors take multiple type arguments. 
      - For instance, the `Either` type constructor has `kind` `* -> * -> *` because it takes two types to produce a result type.

  `Constraint`
    * This `kind` represents type classes. 
      - For example, the `kind` of the `Eq` type class is `Constraint`.


## Example


  declare a `type class` named `Transformer` with a single type parameter `t`
    
  ```haskell
    class Transformer t where  ...
  ```
       
  define a function `transform` that takes an argument of type a b (where a and b are type parameters) and returns a value of type t a b. 
  In other words, it defines a method that can transform an `a b` into  `t` of `a b`.
  ```haskell
     ...   transform :: a b -> t a b  
  ```  

  define a data type named `Frank` with two type parameters `a` and `b`. 
  It has a single `constructor` called `Frank`, which has a field named `frankField` of type `a b`. 
  This data type is essentially a container that holds a value of type a b.
  ```haskell
    data Frank a b  = Frank {frankField :: a b} deriving (Show)  
  ``` 

  declare an `instance` of the `Transformer type class` for the `Frank data type`. 
  It means that we're providing an `implementation` of the `transform` function specifically for the `Frank data type`.
  ```haskell
    instance Transformer Frank where  ...
  ```
   
  implement the `transform` function for the `Frank data type`. 
  its `argument x` is of `type a b` and applies it to the `Frank constructor`, 
  effectively `transforming` it into a value of type `Frank a b`. 
  ```haskell
     ...   transform x = Frank x  
  ```  

---
### Output
```haskell
  ghci> transform (Just 'A') :: Frank Maybe Char
        Frank {frankField = Just 'A'}
  ghci> 
  ghci> transform ( Node Empty 'A' Empty) :: Frank BinaryTree Char
        Frank {frankField = Node Empty 'A' Empty}
```