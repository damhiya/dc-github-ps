{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE DataKinds      #-}

{-# LANGUAGE UndecidableInstances #-}

module QSort where

import GHC.TypeLits

type family If (p :: Bool) (x :: k) (y :: k) :: k
type instance If 'True  x y = x
type instance If 'False x y = y

type family FilterLE (t :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance FilterLE t '[]    ys = ys
type instance FilterLE t (x:xs) ys =
  If (x <=? t)
    (FilterLE t xs (x:ys))
    (FilterLE t xs ys)

type family FilterGT (t :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance FilterGT t '[]    ys = ys
type instance FilterGT t (x:xs) ys =
  If (x <=? t)
    (FilterGT t xs ys)
    (FilterGT t xs (x:ys))

type family Null (xs :: [a]) :: Bool
type instance Null '[]    = 'True
type instance Null (x:xs) = 'False

type family QSortD (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance QSortD '[]  ys = ys
type instance QSortD (x:xs) ys =
  If (Null xs)
    (x:ys)
    (QSortD (FilterLE x xs '[])
      (x :
        (QSortD (FilterGT x xs '[])
          ys)))

type family QSort (xs :: [Nat]) :: [Nat]
type instance QSort xs = QSortD xs '[]
