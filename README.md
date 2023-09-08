# learn-you-a-haskell

#### First Principles

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

## class
 * The `class` keyword is used to declare type classes. 
 * Declares a set of functions that can be implemented by one or more data types. 
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

## concrete data type

  * is a data type that represents values with a fixed, specific structure and behavior.
  * instances of concrete data types are actual values with defined characteristics.
    - **predefined** data types: `Int`, `Bool`, 
    - **custom** data types are defined using the `data` keyword, such as `data Color = Red | Green | Blue`
  * are used to create values and perform operations specific to those types

## data type constructor

  * is a parameterized data type that can accept type arguments to create new, parameterized types.
  * used to create more abstract or polymorphic data types that work with different types of values.
     - **predefined** data type constructors: `[]` (used to create lists), `(->)` (used to represent function types)
     - **custom** data type constructors are defined using the `data` keyword, such as 
     ```haskell
      data BinaryTree a = Empty | Node (BinaryTree a) a (BinaryTree a)
     ```
