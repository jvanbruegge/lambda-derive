module Recursion (Fix (..), Algebra, cata) where

newtype Fix f = In {out :: f (Fix f)}

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . out
