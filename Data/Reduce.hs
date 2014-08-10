module Data.Reduce where

import qualified Data.Foldable as Foldable
import Data.Monoid
import Data.Maybe

inc :: (a -> a -> a) -> [ Maybe a ] -> a -> [ Maybe a ]
inc _ [] x = [ Just x ]
inc _ (Nothing : ys) x = Just x : ys
inc f (Just y : ys) x = Nothing : inc f ys (f y x)

fini :: (a -> a -> a) -> [ Maybe a ] -> Maybe a
fini _ [] = Nothing
fini f (Nothing : xs) = fini f xs
fini _ [ Just x ] = Just x
fini f (Just x : xs) = loop x xs
    where
        loop y [] = Just y
        loop y (Nothing : zs) = loop y zs
        loop y (Just z : zs) = loop (f z y) zs

add :: (a -> a -> a) -> [ Maybe a ] -> [ Maybe a ] -> [ Maybe a ]
add f = loop0
    where
        loop0 xs [] = xs
        loop0 [] ys = ys
        loop0 (Nothing : xs) (y : ys) = y : loop0 xs ys
        loop0 (Just x : xs) (y : ys) = y : loop1 x xs ys
        loop1 x [] [] = [ Just x ]
        loop1 x [] (y : ys) = y : loop1 x [] ys
        loop1 x (Nothing : xs) [] = Just x : xs
        loop1 x (Just x' : xs) [] = Nothing : loop1 (f x' x) xs []
        loop1 x (Nothing : xs) (y : ys) = y : loop1 x xs ys
        loop1 x (Just x' : xs) (y : ys) = y : loop1 (f x' x) xs ys


reduce :: Foldable.Foldable t => (a -> a -> a) -> t a -> Maybe a
reduce f xs = fini f $ Foldable.foldl (inc f) [] xs

data Reduce a = Reduce [ Maybe a ]

empty :: Reduce a
empty = Reduce []

singleton :: a -> Reduce a
singleton x = Reduce [ Just x ]

append :: (a -> a -> a) -> Reduce a -> a -> Reduce a
append f (Reduce xs) y = Reduce $ inc f xs y

toMaybe :: (a -> a -> a) -> Reduce a -> Maybe a
toMaybe f (Reduce xs) = fini f xs

concat :: (a -> a -> a) -> Reduce a -> Reduce a -> Reduce a
concat f (Reduce xs) (Reduce ys) = Reduce $ add f xs ys

instance Monoid a => Monoid (Reduce a) where
    mempty = Reduce []
    mappend (Reduce xs) (Reduce ys) = Reduce $ add mappend xs ys

reduceSort :: Foldable.Foldable t => (a -> a -> Ordering) -> t a -> [a]
reduceSort cmp xs = fromMaybe [] $ fini merge $ Foldable.foldl' f [] xs
    where
        f d x = inc merge d [ x ]
        merge [] qs = qs
        merge ps [] = ps
        merge ps@(p:_) (q:qs) | cmp p q == GT = q : merge ps qs
        merge (p:ps) qs = p : merge ps qs


