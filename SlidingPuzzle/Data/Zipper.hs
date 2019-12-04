{-# LANGUAGE DeriveFunctor #-}

module Data.Zipper where

data Z a = Z ![a] !a ![a] deriving (Show, Functor)

focusL :: Z a -> Z a
focusL (Z (l:ls) f rs) = Z ls l (f:rs)

focusR :: Z a -> Z a
focusR (Z ls f (r:rs)) = Z (f:ls) r rs

extractz :: Z a -> a
extractz (Z _ f _) = f

toList :: Z a -> [a]
toList (Z ls f rs) = go ls (f:rs) where
  go [] ys     = ys
  go (x:xs) ys = go xs (x:ys)

fromList :: [a] -> Z a
fromList (x:xs) = Z [] x xs
