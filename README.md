# learn-you-a-haskell

## First Principles

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
