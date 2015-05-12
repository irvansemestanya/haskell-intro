class: center, middle

# Discover Haskell 98 in 3 hours<br>through progressive exercises<br>for strong-willed souls<br>who learn by doing

## Erik Dominikus

### 2015-04-14

#### (This deck is too big.)

---

# Audience

People new to Haskell.

Prior programming experience is optional.

---

# Goals

After attending this presentation, you should be able to:

* tell apart values and types
* figure out the types of terms
* use the interpreter
* read and write simple higher-order functions
* read and write simple functions with data types and pattern matching
* read and write equivalence relations
* read and write recursive data types
* read and write recursive functions
* deal with things never seen before

---

# Preparation

Make a file Main.hs containing this code:

```haskell
module Main where
```

Interpret that file:

```
ghci Main.hs
```

To quit ghci on Ubuntu, press Ctrl-D.

---

# ghci basics

Reload files:

```
:r
```

Figure out the type of an expression:

```
:t <expression>
```

Get information about a name:

```
:i <name>
```

Show a list of commands:

```
:h
```

---

# Resources

[Haskell wiki](http://wiki.haskell.org/) contains
guides, tutorials, explanations, information about libraries for common programming tasks.

Haskell cafe (mailing list)

[Real world Haskell](http://book.realworldhaskell.org/)

---

# Lambda calculus

Lambda calculus is the foundation of functional programming.

Lambda abstraction is binding.

Function application is substitution.

\\(\beta\\)-reduction is function application.

Example:

\\(\beta\\)-reducing \\((\lambda x. x)~0\\) substitutes \\(x\\) with \\(0\\):

\\[
\begin{align\*}
(\lambda x. x)~0 \Longrightarrow_\beta 0
\end{align\*}
\\]

---

A bigger example: \\(\beta\\)-reduction with normal-order evaluation strategy:

Replace \\(f\\), replace \\(x\\).

\\[
\begin{align\*}
&(\lambda f. (\lambda x. f~x))~(\lambda u. u)~0
\\\\ &\Longrightarrow\_\beta (\lambda x. (\lambda u. u)~x)~0
\\\\ &\Longrightarrow\_\beta (\lambda u. u)~0
\\\\ &\Longrightarrow\_\beta 0
\end{align\*}
\\]

---

An even bigger example:

\\[
\begin{align\*}
&(\lambda f. (\lambda x. f~x~x))~(\lambda u. (\lambda v. (+)~u~v))~0
\\\\ &\Longrightarrow\_\beta (\lambda x. (\lambda u. (\lambda v. (+)~u~v))~x~x)~0
\\\\ &\Longrightarrow\_\beta (\lambda u. (\lambda v. (+)~u~v))~0~0
\\\\ &\Longrightarrow\_\beta (\lambda v. (+)~0~v))~0
\\\\ &\Longrightarrow\_\beta (+)~0~0
\\\\ &\Longrightarrow\_\beta 0
\end{align\*}
\\]

---

# Scopes and shadowing

Which \\(x\\) is which?

\\[
\lambda f. (\lambda x. f~(\lambda x. f~x)~x)
\\]

---

# Check your understanding of lambda calculus

Normalize these terms using normal-order evaluation strategy:

\\[
\begin{align\*}
(\lambda x. \lambda f. f~x)~0
\\\\
(\lambda x. \lambda y. \lambda z. x~z~(y~z))~()~()~0
\\\\
(\lambda x. \lambda y. \lambda z. x~(y~z))~(\lambda x. (+)~1~x)~(\lambda x. (+)~2~x)
\\\\
(\lambda f. \lambda x. \lambda y. (+)~(f~x)~(f~y))~((+)~1)
\end{align\*}
\\]

\\(\beta\\)-reduce this term:

\\[
(\lambda x. x~x)~(\lambda x. x~x)
\\]

What do you find?

---

# Scopes and shadowing again

Which x is which?

```
\ f -> \ x -> f (\ x -> f x) x
```

```
(\ f -> (\ x -> f (x + 1))) (\ u -> u * u) 0

==>  (\ x -> (\ u -> u * u) (x + 1)) 0

==>  (\ u -> u * u) (0 + 1)

==>  (0 + 1) * (0 + 1)
```

---

# Values and types

Every value has a type.

Example:

```
'a' :: Char
```

Read: The type of 'a' is Char.

You can try that in ghci.

```
> :t 'a'
'a' :: Char
```

---

# Check your understanding

In this expression:

```
\ f -> (\ x -> f x)
```

Which terms are values?

Which terms are types?

What is the type of f?

What is the type of x?

What is the type of f x?

---

# Check your understanding

In this annotated expression:

```
(\ f -> \ x -> f x) :: (a -> b) -> a -> b
```

Which terms are types?

Which terms are values?

---

# Don't let parentheses fool you

All these are the same value:

```
((a b) c) d
```

```
(a b c) d
```

```
a b c d
```

All these are the same value:

```
\ a -> (\ b -> (\ c -> d))
```

```
\ a -> \ b -> \ c -> d
```

```
\ a b c -> d
```

---

# Don't let parentheses fool you

All these are the same type:

```
((a b) c) d
```

```
(a b c) d
```

```
a b c d
```

All these are the same type:

```
a -> b -> c -> d
```

```
a -> (b -> c -> d)
```

```
a -> (b -> (c -> d))
```

---

# Don't let parentheses fool you

But each of these is a different value:

```
a (b c d)
```

```
a (b c) d
```

```
a b (c d)
```

```
a b c d
```

---

# Dollars, parentheses, infix operators

```haskell
f $ x = f x
infixr 0 $

-- TODO penjelasan associativity

(f . g) x = f (g x)
infixr 9 .
```

All these are the same value:

```
f (g (h x))

f $ g (h x)

f $ (g $ h x)

f $ g $ h x

f . g . h $ x

(f . g . h) x
```

---

# Switching between prefix and infix

All these are the same value:

```
(+) x y

x + y
```

All these are the same value:

```
f x y

x \`f\` y
```

All these are the same type:

```
a -> b

(->) a b
```

---

# Little inconsistency

Can write this value:

```
(+) a b
```

and can write this type:

```
(->) a b
```

Can write this value:

```
x \`f\` y
```

but cannot write this type:

```
a \`Either\` b
```

---

# Many-argument functions

We often write many-argument functions like this:

```
\ a -> \ b -> \ c -> d
```

so Haskell provides this other way of writing that function:

```
\ a b c -> d
```

---

# Binding names to values

You can bind a name to a value.

You can annotate a name with a type.

```haskell
f :: (a -> b) -> a -> b
f = \ g x -> g x
```

If you don't give a type annotation, the compiler infers the most general type.

We define things all the time so Haskell has a syntax for function definitions.
This is the same as the above definition:

```haskell
f g x = g x
```

---

# Check your understanding

In this definition:

```haskell
f :: (Int -> Int) -> Int -> Int
f g x = g x
```

Which terms are values?

Which terms are types?

What is the type of f?

What is the type of g?

What is the type of x?

---

# Unique functions

Write this function.

```haskell
f :: a -> a
```

How many unique functions have that type?

---

There is only one such function.

```haskell
f :: a -> a
f x = x
```

---

But I lied.
There is this thing:

```haskell
undefined :: a
undefined = error "undefined"
```

There are only three unique ways to write the function of type `a -> a`:

```haskell
f0 :: a -> a
f0 x = x

f1 :: a -> a
f1 _ = undefined

f2 :: a -> a
f2 = undefined
```

---

However, if we only write total functions,
there is only one way to write that function.

[The Haskell Djinn](https://hackage.haskell.org/package/djinn)
can infer such unique total functions.
You can try it at #haskell-id at Freenode IRC.

```
@djinn e -> Maybe a -> Either e a
f a b =
    case b of
        Nothing -> Left a
        Just c -> Right c
```

---

# Parametricity

Write this function.

```haskell
f :: a -> b -> a
```

Write this function.

```haskell
g :: a -> a -> a
```

(Totality does not imply uniqueness.)

In the total case,
the universally-quantified type guarantees that you can only
use or not use the argument,
and not change it.

---

# Underscore for unused argument

```haskell
f :: a -> b -> a
f x _ = x
```

---

# Higher-order functions

Write this function.

```haskell
myFun :: (a -> b) -> a -> b
```

---

# Check your understanding of higher-order functions

Write this function.

```haskell
hof0 :: (b -> c) -> (a -> b) -> (a -> c)
```

Hint:
Don't let the last pair of parentheses fool you.
The type is really:

```
(b -> c) -> (a -> b) -> a -> c
```

Hint:
all you need are function applications and parentheses.

Write this function.

```haskell
hof1 :: (a -> b -> c) -> (a -> b) -> (a -> c)
```

---

# Type functions and the kind system

A value-level term has a type.

A type-level term has a kind.

```haskell
type C a = a
-- C :: * -> *
-- a :: *
```

C is not a type.

But if b is a type, then C b is a type.

```haskell
type L a b = a
-- L :: * -> * -> *
-- a :: *
-- b :: *
```

---

# Type functions

```haskell
type Apply f a = f a
-- Apply :: (* -> *) -> * -> *
-- f :: (* -> *)
-- a :: *

type Apply2 f a b = f a b
-- Apply2 :: (* -> * -> *) -> * -> * -> *
-- f :: * -> * -> *
-- a :: *
-- b :: *
```

---

# Data types and pattern-matching

```haskell
data Bool = False | True
```

Which terms are types?

Which terms are values?

What is the type of False?

Write this function.

```haskell
myNot :: Bool -> Bool
```

---

# Declaration style

```haskell
myNot False = True
myNot True = False
```

# Expression style

```haskell
myNot x =
    case x of
        False -> True
        True -> False
```

More info: [Haskell wiki: declaration vs expression style](https://wiki.haskell.org/Declaration_vs._expression_style).

---

# More pattern-matching

```haskell
data Bool = False | True
```

Write these functions.

```haskell
myAnd :: Bool -> Bool

myOr :: Bool -> Bool

ifThenElse :: Bool -> a -> a -> a
```

---

# If there were no if

Haskell provides this syntax for conditional expression:

```
if <condition> then <true-part> else <false-part>
```

but we can live just fine without that syntax.

---

# Data types with parameters

```haskell
data Pair a b = MkPair a b
```

Which terms are types?

Which terms are values?

Give an example inhabitant of Pair Char Char.

What is the type of MkPair?

Write this function.

```haskell
mkPair :: a -> b -> Pair a b
```

---

# η is Greek small letter eta

## η-reduction

```
mkPair x y = MkPair x y
mkPair x = MkPair x
mkPair = MkPair
```

## η-expansion

```
mkPair = MkPair
mkPair x = MkPair x
mkPair x y = MkPair x y
```

More info: [Haskell wiki: pointfree](https://wiki.haskell.org/Pointfree).

---

# Getters and setters

```haskell
data Pair a b = MkPair a b
```

Write these functions.

```haskell
first :: Pair a b -> a

second :: Pair a b -> b

setFirst :: c -> Pair a b -> Pair c b

setSecond :: c -> Pair a b -> Pair a c
```

---

# Records

## Record syntax

```haskell
data Pair a b = MkPair { first :: a, second :: b }
```

## Creating a value of type Pair a b

```
MkPair { first = x, second = y }
```

## Record update syntax

```
p { first = x }

p { second = y }

p { first = x, second = y }
```

---

# Check your understanding of records

```haskell
data T = MkT { x :: Int, y :: String }

foo a = a { x = 0 }
```

What is the type of MkT?

What is the type of x?

What is the type of y?

What is the type of foo?

---

# Example of reuse

```haskell
data Pair a b = MkPair a b

id :: a -> a
id x = x
```

Write this function.

```haskell
mapBoth :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
```

Write these functions using mapBoth and id.

```haskell
mapFirst :: (a -> c) -> Pair a b -> Pair c b

mapSecond :: (a -> c) -> Pair a b -> Pair c b
```

---

# Data types with many parameters and constructors

```haskell
data Either a b = Left a | Right b
```

Which terms are types?
Which terms are values?

Give several example inhabitants of Either Int Char.

What is the type of Left?

What is the type of Right?

Write this function.

```haskell
bimapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
```

---

# Recursive data types

```haskell
data List a = Nil | Cons a (List a)
```

and mutually recursive data types

```haskell
data A = A0 | A1 B
data B = B0 | B1 A
```

A data type can:

* have many parameters,
* have many constructors,
* be recursive,
* be mutually recursive.

---

# Educational reimplementation of Data.List

```haskell
data [] a
    = a : [] a
```

Implement these correctly without worrying about performance.
See the Haddock of Data.List module for what these functions do.

```haskell
map :: (a -> b) -> [a] -> [b]

filter :: (a -> Bool) -> [a] -> [a]

sort :: (a -> Bool) -> [a] -> [a]

(++) :: [a] -> [a] -> [a]

reverse :: [a] -> [a]
```

(There are more in the next slide.)

---

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a

concat :: [[a]] -> [a]

iterate :: (a -> a) -> a -> [a]

repeat :: a -> [a]

replicate :: Int -> a -> [a]

cycle :: [a] -> [a]

take :: Int -> [a] -> [a]

drop :: Int -> [a] -> [a]

takeWhile :: (a -> Bool) -> [a] -> [a]

dropWhile :: (a -> Bool) -> [a] -> [a]

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
```

---

# Some patterns

```haskell
data Identity a = MkIdentity a

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data List a = Nil | Cons a (List a)
```

Write these functions.

```haskell
mapIdentity :: (a -> b) -> Identity a -> Identity b

mapMaybe :: (a -> b) -> Maybe a -> Maybe b

mapEither :: (a -> b) -> Either e a -> Either e b

mapList :: (a -> b) -> List a -> List b
```

---

```haskell
data Maybe a = Nothing | Just a

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
```

Actually there are two possible total implementations of mapMaybe.

```haskell
mapMaybe0 :: (a -> b) -> Maybe a -> Maybe b
mapMaybe0 _ _ = Nothing

mapMaybe1 :: (a -> b) -> Maybe a -> Maybe b
mapMaybe1 _ Nothing = Nothing
mapMaybe1 f (Just x) = Just (f x)
```

However, there is only one satisfying these:

```
mapMaybe id = id
mapMaybe f . mapMaybe g = mapMaybe (f . g)
```

Which mapMaybe is that?

---

# Ad-hoc (for-this) polymorphism

```haskell
mapIdentity         :: (a -> b) -> Identity a -> Identity b
mapMaybe            :: (a -> b) ->    Maybe a ->    Maybe b
mapEither           :: (a -> b) -> Either e a -> Either e b
mapList             :: (a -> b) ->     List a ->     List b
```

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Identity   where fmap = mapIdentity
instance Functor Maybe      where fmap = mapMaybe
instance Functor (Either e) where fmap = mapEither
instance Functor List       where fmap = mapList
```

```haskell
fmap :: (Functor f) => (a -> b) ->        f a ->        f b
```

---

# Parametric polymorphism

Same name, different type, same definition.

```haskell
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

foo = length [0,1,2] + length ['a','b']
```

# Ad-hoc polymorphism

Same name, different type, different definition.

```haskell
instance Functor Maybe where fmap = mapMaybe
instance Functor (Either e) where fmap = mapEither
```

---

# Functors

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

A Functor instance should obey these laws:

```haskell
fmap id = id
```

```haskell
fmap (f . g) = fmap f . fmap g
```

---

# Classes are not object-oriented

This looks bad:

```haskell
class Animal a where
    sound :: a -> String

data Dog = MkDog

instance Animal Dog where
    sound _ = "woof"
```

because a data type seems to do it just fine:

```haskell
data Animal = MkAnimal { _sound :: String }

dog :: Animal
dog = MkAnimal "woof"
```

---

Besides, the data-type approach fares better with composition.

```haskell
lively :: Animal -> Animal
lively x = x { _sound = _sound x ++ "!" }

loud :: Animal -> Animal
loud x = x { _sound = map toUpper $ _sound x }

blackie :: Animal
blackie = lively dog

doggie :: Animal
doggie = loud dog
```

```
> putStrLn $ _sound blackie
woof!

> putStrLn $ _sound doggie
WOOF

> putStrLn $ _sound $ lively $ loud dog
WOOF!
```

---

# More info

[Scrap your type classes](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)

[Haskell antipattern: existential typeclass](https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/)

---

# Future-proofing records

```haskell
data Animal = MkAnimal { _happySound :: String }

dog :: Animal
dog = MkAnimal "woof"

cat :: Animal
cat = MkAnimal "meow"
```

This breaks if we add a field to MkAnimal.

---

# Future-proofing by indirection

```haskell
data Animal = MkAnimal { _happySound :: String, _angrySound :: String }

defAnimal :: Animal
defAnimal = MkAnimal
    {
        _happySound = ""
        , _angrySound = ""
    }

dog :: Animal
dog = defAnimal { _happySound = "woof", _angrySound = "bark" }

cat :: Animal
cat = defAnimal { _happySound = "meow", _angrySound = "hiss" }
```

But compile error is much better than nonsensical default.

---

# Equivalence relations

```haskell
data Bool = False | True
```

Write this function
that determines whether two Bools are the same.

```haskell
eqBool :: Bool -> Bool -> Bool
```

---

# More equivalence relations

```haskell
data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data Pair a b = MkPair a b
```

Write these functions.

```haskell
eqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool

eqEither :: (a -> a -> Bool) -> (b -> b -> Bool) -> Either a b -> Either a b -> Bool

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> Pair a b -> Pair a b -> Bool
```

---

# Default and minimum implementations

A class member can have a default implementation.

```haskell
class Eq a where

    (==) :: a -> a -> Bool
    (==) x y = not (x /= y)

    (/=) :: a -> a -> Bool
    (/=) x y = not (x == y)
```

Minimum implementation: At least you have to write (==) or (/=).

---

# Instance constraints

```haskell
instance Eq Bool where
    (==) = eqBool

instance (Eq a) => Eq (Maybe a) where
    (==) = eqMaybe (==)

instance (Eq a, Eq b) => Eq (Either a b) where
    (==) = eqEither (==) (==)

instance (Eq a, Eq b) => Eq (Pair a b) where
    (==) = eqPair (==) (==)
```

What are the type of each (==) above?

---

# Class constraints

A class can have many constraints

```haskell
class (Num a, Ord a) => Real a
```

in the same way an instance can have many constraints:

```haskell
instance (Eq a, Eq b) => Eq (Either a b)
```

---

# Even more equivalence relations

An equivalence relation eq is:

* reflexive: eq x x,
* symmetric: if eq x y then eq y x,
* transitive: if eq x y and eq y z then eq x z.

Is f an equivalence relation?

```haskell
f False False = False
f False True = True
f True False = False
f True True = False
```

---

# So many equivalence relations

Assume that both eqFirst and eqSecond are equivalence relations.

Which of these are equivalence relations?

```haskell
f eqFirst eqSecond (MkPair x0 y0) (MkPair x1 y1) =
    eqFirst x0 x1
```

```haskell
g eqFirst eqSecond (MkPair x0 y0) (MkPair x1 y1) =
    eqSecond y0 y1
```

```haskell
h eqFirst eqSecond (MkPair x0 y0) (MkPair x1 y1) =
    eqFirst x0 x1 && eqSecond y0 y1
```

```haskell
i eqFirst eqSecond (MkPair x0 y0) (MkPair x1 y1) =
    eqFirst x0 x1 || eqSecond y0 y1
```

---

# But usually these equivalence relations

```haskell
eqPair eqFirst eqSecond (MkPair x0 y0) (MkPair x1 y1) =
    eqFirst x0 x1 && eqSecond y0 y1
```

The 'deriving' mechanism generates the equivalence relation we usually expect
(involving as many fields as possible).

```haskell
data Bool = False | True deriving (Eq)

data Maybe a = Nothing | Just a deriving (Eq)

data Either a b = Left a | Right b deriving (Eq)
```

More info: [specification of derived instances in Haskell 98 report](https://www.haskell.org/onlinereport/derived.html)

The 'deriving' mechanism only works for
Eq, Ord, Enum, Bounded, Show, Read.

---

# Example usage of equivalence relations

Write these functions that determine whether a list contains a certain thing.

```haskell
myElemBy :: (a -> a -> Bool) -> a -> [a] -> Bool

myElem :: (Eq a) => a -> [a] -> Bool
```

---

# Total orders

```haskell
data Ordering = LT | EQ | GT

class (Eq a) => Ord a where
    compare :: a -> a -> Ordering
    (<=)    :: a -> a -> Bool
    (<)     :: a -> a -> Bool
    (>=)    :: a -> a -> Bool
    (>)     :: a -> a -> Bool
    max     :: a -> a -> a
    min     :: a -> a -> a

instance Ord Int
instance Ord Integer
instance Ord Float
instance Ord Double
...
instance (Ord a) => Ord [a]
instance (Ord a, Ord b) => Ord (a, b)
instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
...
```

---

# Numbers

```haskell
class Num a where
    (+)         :: a -> a -> a
    (*)         :: a -> a -> a
    (-)         :: a -> a -> a
    negate      :: a -> a
    abs         :: a -> a
    signum      :: a -> a
    fromInteger :: Integer -> a

instance Num Int
instance Num Integer
instance Num Float
instance Num Double
...
```

What is the type of (+)?

---

# Numeric literals

ghci reveals:

```
> :t 0
0 :: (Num a) => a

> :t 0.0
0.0 :: (Fractional a) => a
```

---

# Some patterns

```haskell
data Maybe a = Nothing | Just a

data Either a b = Left a | Right b
```

Write these functions.

```haskell
pureMaybe :: a -> Maybe a

pureEither :: a -> Either e a

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b

applyEither :: Either e (a -> b) -> Either e a -> Either e b
```

---

# Applicative functors

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

```haskell
instance Applicative Maybe where
    pure = pureMaybe
    (<*>) = applyMaybe
```

---

# Applicative functors

`<*>` is lifted function application.
It associates to the left like function application.

```
a <*> b <*> c = (a <*> b) <*> c
```

An Applicative instance should obey these laws:

```
pure f <*> x = fmap f x

pure f <*> pure x = pure (f x)

pure f <*> pure x = pure ($ x) <*> pure f
```

A common pattern:

```
       f <$> a <*> b <*> c <*> ...
= pure f <*> a <*> b <*> c <*> ...
```

---

# Lifting function applications

```haskell
import Control.Applicative

(<$>) :: (Functor f) => (a -> b) -> (f a -> f b)
(<$>) = fmap

class (Functor f) => Applicative f
```

Write these functions.

```haskell
myLiftA2 :: (Applicative f) => (a -> b -> c) -> (f a -> f b -> f c)

myLiftA3 :: (Applicative f) => (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
```

---

# Still function applications

What does this evaluate to?

```
(+) 1 2
```

What does this evaluate to?

```
(+) <$> Just 1 <*> Just 2
```

What does this evaluate to?

```
(+) <$> Just 1 <*> Nothing
```

What does this evaluate to?

```
liftA2 (+) Nothing (Just 2)
```

---

# Example usage of Applicative

FIXME wrong example; this needs Monad

Programming is unfortunately a puzzle.

```haskell
instance Functor IO
instance Applicative IO

type String = [Char]

getLine     :: IO String
putStrLn    :: String -> IO ()
(++)        :: [a] -> [a] -> [a]
(<$>)       :: (Functor f) => (a -> b) -> (f a -> f b)
(<*>)       :: (Applicative f) => f (a -> b) -> f a -> f b
```

Write a program that asks two lines and then prints them concatenated.

```haskell
main :: IO ()
```

---

# Some patterns

```haskell
data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data List a = Nil | Cons a (List a)
```

Write these functions.

```haskell
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b

bindEither :: Either e a -> (a -> Either e b) -> Either e b

bindList :: List a -> (a -> List b) -> List b
```

---

# Monads

```haskell
class (Applicative m) => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

```haskell
instance Monad Maybe where
    return = pure
    (>>=) = bindMaybe
```

```haskell
instance Monad (Either e) where
    return = pure
    (>>=) = bindEither
```

```haskell
instance Monad List where
    return = pure
    (>>=) = bindList
```

---

# Breaking changes in GHC 7.10

## Functor-Applicative-Monad proposal

Before:

```haskell
class (Functor m) => Monad m
```

After:

```haskell
class (Applicative m) => Monad m
```

---

# Meanwhile in China

```haskell
data Person
    = MkPerson
    {
        child :: Maybe Person
    }
```

Write this function.

```haskell
grandchild :: Person -> Maybe Person
```

---

# Kleisli composition

Write this function.

```haskell
(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
```

---

# From Beijing with Kleisli composition

```haskell
data Person
    = MkPerson
    {
        child :: Maybe Person
    }
```

Write this function using Kleisli composition.

```haskell
grandchild :: Person -> Maybe Person
```

---

# If Mao Zedong wrote Haskell programs

(Mao Zedong died in 1976; Haskell 98 report began in 1997.)

```haskell
data Person
    = MkPerson
    {
        children :: [Person]
    }
```

Write this function.

```haskell
grandchildren :: Person -> [Person]
```

---

# Generalizing by parametrization

Another example of indirection.

```haskell
data Person m
    = MkPerson
    {
        children :: m Person
    }

type ListPerson = Person []
type MaybePerson = Person Maybe

grandchildren :: (Monad m) => Person m -> m (Person m)
```

If the requirement may change,
using a [Person] with one element
is more prudent than using Maybe Person.

---

# Some higher-order functions

Write these functions.

```haskell
flip :: (a -> b -> c) -> (b -> a -> c)
```

```haskell
curry :: ((a, b) -> c) -> (a -> b -> c)
```

```haskell
uncurry :: (a -> b -> c) -> ((a, b) -> c)
```

---

# Compositions

```haskell
import Control.Category ((>>>))
import Control.Applicative (liftA2)
import Control.Monad (liftM2, (>=>))
```

```haskell
(>>>)        ::                      (a ->   b) ->   (b ->   c) ->   (a ->   c)
liftA2 (>>>) :: (Applicative m) => m (a ->   b) -> m (b ->   c) -> m (a ->   c)
liftM2 (>>>) :: (Monad       m) => m (a ->   b) -> m (b ->   c) -> m (a ->   c)
(>=>)        :: (Monad       m) =>   (a -> m b) ->   (b -> m c) ->   (a -> m c)
```

Actually left-to-right composition (>>>) is a bit more general:

```haskell
(>>>) :: (Category cat) => cat a b -> cat b c -> cat a c

instance Category (->)
```

---

# Applications

```haskell
import Control.Applicative (liftA, (<$>), (<*>))
import Control.Monad (liftM, (=<<))
```

```haskell
($)   ::                      (a ->   b) -> (  a ->   b)
(<$>) :: (Functor     m) =>   (a ->   b) -> (m a -> m b)
liftA :: (Applicative m) =>   (a ->   b) -> (m a -> m b)
liftM :: (Monad       m) =>   (a ->   b) -> (m a -> m b)
(<*>) :: (Applicative m) => m (a ->   b) -> (m a -> m b)
ap    :: (Monad       m) => m (a ->   b) -> (m a -> m b)
(=<<) :: (Monad       m) =>   (a -> m b) -> (m a -> m b)
```

```haskell
(<$>) = fmap
```

```haskell
import Control.Monad ((>>=))

flip ($) ::                a -> (a ->   b) ->   b
(>>=)    :: (Monad m) => m a -> (a -> m b) -> m b
```

---

# Result-ignoring applications

```haskell
import Control.Applicative ((<$), (<*), (*>))
import Control.Monad ((>>))
```

```haskell
(<$)  :: (Functor     m) =>   b -> m a -> m b
(<*)  :: (Applicative m) => m a -> m b -> m a
(*>)  :: (Applicative m) => m a -> m b -> m b
(>>)  :: (Monad       m) => m a -> m b -> m b
```

---

# Do-notation for Monad instances

```
do
    a <- b
    c <- d
    let
        e = f
        g = h
    i
```

translates to

```
a >>= \ b ->
c >>= \ d ->
let
    e = f
    g = h
in
    i
```

---

# Side effects

What does main print?

```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse c t f = if c then t else f

main :: IO ()
main = liftM3 ifThenElse (return True) (putStrLn "true") (putStrLn "false")
```

Write oldIfThenElse that does the same as `liftM3 ifThenElse` but without using liftM3.

```haskell
oldIfThenElse :: (Monad m) => m Bool -> m a -> m a -> m a
```

Write newIfThenElse
that executes the side-effect of the taken branch
and does not execute the side-effect ot the branch not taken:

```haskell
newIfThenElse :: (Monad m) => m Bool -> m a -> m a -> m a
```

---

# Binary trees

```haskell
data Tree a = Leaf | Node a (Tree a) (Tree a)
```

Write this function that inserts a value into a binary search tree.

```haskell
insert :: (Ord a) => a -> Tree a -> Tree a
```

---

# Folds

Write these functions.

```haskell
foldrMaybe :: (e -> a -> a) -> a -> Maybe e -> a

foldrList  :: (e -> a -> a) -> a ->  List e -> a
```

```
foldrList f a [x0, x1, x2, ..., xm] = f xm ( ... (f x2 (f x1 (f x0 a))))
```

---

# You can write your own control structures

```haskell
instance Monad IO

return :: (Monad m) => a -> m a

if <condition> then <true-part> else <false-part>
```

Write these functions.

```haskell
when :: (Monad m) => Bool -> m () -> m ()

unless :: (Monad m) => Bool -> m () -> m ()

forever :: (Monad m) => m a -> m b
```

---

# More custom control structures

```haskell
module MyControlStructures
where

import qualified Control.Concurrent as C
import qualified Control.Monad as M
import qualified System.IO.Error as E

runEverySecond :: IO a -> IO b
runEverySecond action =
    M.forever (C.threadDelay 1000000 >> action)

retry :: Int -> IO a -> IO a
retry maxRetry action
    | maxRetry <= 0 = action
    | otherwise = E.catchIOError action (\ _err -> retry_)
    where
        retry_ = retry (maxRetry - 1) action
```

---

# You can have many pattern guards

```haskell
f x y z | cond0 = expr0
        | cond1 = expr1
        | cond2 = expr2
        | cond3 = expr3

g x
    | cond0 = expr0
    | cond1 = expr1
```

where each cond can be any expression of type Bool.

```haskell
-- Defined in GHC.Base
otherwise :: Bool
otherwise = True
```

Make sure you have considered all cases. Use 'ghc -Wall' to compile with warnings.

---

# Libraries on [Hackage](http://hackage.haskell.org/)

* CSV parsing, input, and output: cassava
* parallelism for functions: parallel
* parallelism for IO stuffs: parallel-io, async
* database access: HDBC, HDBC-postgresql, mongoDB, bson
* JSON input and output: aeson, aeson-pretty
* byte input and output: binary, cereal, bytestring
* textual input, output, encoding, and decoding: text
* monad transformers: transformers, mtl
* general parsing: parsec, attoparsec
* low-level networking: network
* HTTP servers: warp
* web application interface: wai

---

# Even more libraries

[Haskell wiki: applications and libraries](https://wiki.haskell.org/Applications_and_libraries)

---

class: center, middle

# That's all

## Thank you 
