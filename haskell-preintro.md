class: center, middle

# Haskell introduction

## Erik Dominikus

### 2015-04-07

---

# Fundamental concepts

* Every value has a type.
* Every function has one argument.
* Normal-order evaluation gives programs meaning.
* Values and types have different namespaces.

---

# Value definition

The definition `x = y` means that we can replace all `x` with `y`
without changing the meaning of the program.

Examples of value definitions:

```
x :: Int
x = 1

f :: Int -> Int
f x = x + 1
```

You can omit type annotations.
If you do, the compiler will infer the *principal type* (the most general type) for the value.

---

# Functions

Every function has **one** argument.

```
f :: (Int, Int) -> Int
f (x, y) = x + y

g :: (Int, Int, Int) -> Int
g (x, y, z) = x * y + z
```

A function taking 'many arguments' is a function returning a function.

```
f :: Int -> Int -> Int
f x y = x + y

g :: Int -> Int -> Int -> Int
g x y z = x * y + z
```

---

# Functions

`(->)` associates to the right:

```none
t -> u -> v  =  t -> (u -> v)
```

That is function that takes `t` and gives a function taking `u` giving `v`.

```none
Int -> Int -> Int -> Int  =  Int -> (Int -> (Int -> Int))
```

---

# Functions are values

```
apply :: (a -> b) -> a -> b
apply f x = f x
```

```
leftToRightComposition :: (a -> b) -> (b -> c) -> (a -> c)
leftToRightComposition f g x = g (f x)
```

---

# Function application

```
(+)         :: Int -> Int -> Int
(+) 5       :: Int -> Int
(+) 5 7     :: Int
```

---

# Type inference

```
g :: Int -> Int
g x = square x + 1
    where
        square y = y * y
```

The compiler infers the type of `square` to be `Int -> Int`.

```
g x = square x + 1
    where
        square :: Int -> Int
        square y = y * y
```

The compiler infers the type of `g` to be `Int -> Int`.

---

# Type synonyms

```
type A = B
```

This means we can replace all `A` with `B`
without changing the meaning of the program:

We say that `A` is a **type synonym** for `B`.

---

# Type functions

```
type A a b = a
```

This means we can replace all `A a b` with `a`
without changing the meaning of the program,
for all `a` and `b`.

We call `a` and `b` **type parameters**.

---

# Type inhabitants

```
type A a b = a
```

`A Int Int` is inhabited by `0`, `1`, `-1`, and others.

`A Char Int` is inhabited by `'a'`, `'b'`, and others.

`A (A Int Char) Int` is the same as `Int`.

`A` is *not* a type.
`A` is a type function.
It does not make sense to talk about the inhabitants of `A`.

---

# Data types

```
data A = B | C | D
```

This defines four things:

* the type `A`.
* the value `B`,
* the value `C`, and
* the value `D`.

All those values have the same type: `A`.

We say those values _inhabit_ their type.

We say that `B`, `C`, and `D` are the **data constructors** of `A`.

---

# More data types

```
data A = B Int
```

This defines two things:

* the type `A`,
* the value `B` whose type is `Int -> A`.

If you give an `Int` to `B`, you get an `A`.

Some inhabitants of `A`:

```
B 0
B 1
B (-1)
```

---

# More data types

```
data A = B Int Int
```

Some inhabitants of `A`:

```
B 0 0
B 1 2
```

---

# Data types with parameters

```
data A a = B a a
```

This defines many things:

* the type function `A`,
* the value `B` having type `a -> a -> A a`.

If `a` is a type, then `A a` is a type.

Some inhabitants of `A (A Int)`:

```
B (B 0 0) (B 1 1)
B (B 1 1) (B 2 2)
B (B 2 (-1)) (B 3 4)
```

---

# Pattern-matching

If you have:

```
data A = B | C Int
```

then you can write:

```
fun :: A -> String
fun B = "I don't have an Int."
fun (C x) = (++) "I have an Int: " (show x)
```

which is the same as:

```
fun thing =
    case thing of
        B -> "I don't have an Int."
        C x -> "I have an Int: " ++ show x
```

---

# Evaluation strategy

Haskell uses **normal-order** evaluation strategy.

Suppose we have:

```
f :: a -> b -> a
f x y = x
```

Let's try evaluating:

```
f (1 + 2) (3 + 4)
```

---

# Evaluation strategy

## Normal order

Head first.

```
f (1 + 2) (3 + 4)
-> 1 + 2                    -- f x y = x
-> 3
```

## Applicative order

Arguments first.

```
f (1 + 2) (3 + 4)
-> f 3 (3 + 4)
-> f 3 7                    -- f x y = x
-> 3
```

---

# Normal vs applicative order

They differ when an argument diverges.

## Normal order

```
f (1 + 2) (error "foo")
-> 1 + 2
-> 3
```

## Applicative order

```
f (1 + 2) (error "foo")
-> f 3 (error "foo")
-> <error>
```

---

# Lists

```
data [] a = [] | a : [] a
    -- (:) a ([] a)
```

```
[]
0 : []  -- (:) 0 []
0 : 1 : []
0 : 1 : 2 : []
0 : (1 : (2 : []))
```

`(:)` associates to the right.

Syntactic sugar:

```
[] a  =  [a]                    -- type
[0, 1, 2]  =  0 : 1 : 2 : []    -- value

[0, 1, 2] :: [a]
```

---

# Normal-order

Normal-order evaluation enables writing this list:

```
x = 1 : x
```

```
natFrom n = n : natFrom (n + 1)
```

```
fib :: [Int]
fib = 1 : 1 : zipWith (+) fib (tail fib)
```

```
fib !! 4 = 5
[0,1,2,3] !! 2 = 2
```

```
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zipWith (+) [1,2,3] [4,5,6] = [5,7,9]
zipWith max [1,2,3] [4,3,2] = [4,3,3]
tail (x : y) = y
tail [0,1,2,3] = [1,2,3]
```

---

# Input-output

```
getLine     :: IO String
putStrLn    :: String -> IO ()
(>>=)       :: IO a -> (a -> IO b) -> IO b
(>>)        :: IO a -> IO b -> IO b
return      :: a -> IO a
fmap        :: (a -> b) -> IO a -> IO b
```

```
myRepeat 10 (putStrLn "Hi")
```

```
getInt :: IO Int
getInt = fmap read getLine

sumTwo :: IO ()
sumTwo =
    getInt >>= \ x ->
    getInt >>= \ y ->
    putStrLn (show (x + y))
```

```
data Either a b = Left a | Right b
data (,) a b = (,) a b
```

---

```
data Maybe a = Just a | Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe def (Just val) = val
fromMaybe def Nothing = def

maybe :: b -> (a -> b) -> Maybe a -> a
maybe def fun (Just val) = fun val
maybe def fun Nothing = def

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f (Just x) = Just (f x)
fmap f Nothing = Nothing
```

---

```
divide :: Int -> Int -> Maybe Int
divide x 0 = Nothing
divide x y = Just (div x y)

woo :: Maybe Int
woo = do
    x <- divide 5 0
    y <- divide x 1
    return (x + y)

woo :: Maybe Int
woo =
    divide 5 0 >>= \ x ->
    divide x 1 >>= \ y ->
    return (x + y)

return = Just

Just x >>= f = f x
Nothing >>= x = Nothing
```

---

```
data Tree a = Leaf | Node a (Tree a) (Tree a)

sumTree :: Tree Int -> Int
sumTree Leaf = 0
sumTree (Node x l r) = x + sumTree l + sumTree r
```

---

# Further reading

* [Real world Haskell](http://book.realworldhaskell.org/)
* [Learn Haskell fast and hard](http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way/)
* [A taste of Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-tutorial/)
* [The Python paradox](http://paulgraham.com/pypar.html)
* [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/)
