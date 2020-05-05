module Recursion (Fix (..), Algebra, RAlgebra, cata, para) where

newtype Fix f = In {out :: f (Fix f)}

type Algebra f a = f a -> a

type RAlgebra f a = Fix f -> f a -> a

para :: Functor f => RAlgebra f a -> Fix f -> a
para alg t = alg t $ para alg <$> out t

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = para (const f)
