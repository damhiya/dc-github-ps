{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE DataKinds      #-}

{-# LANGUAGE UndecidableInstances #-}

module QSort where

import GHC.TypeLits

type family FilterLE (t :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance FilterLE t '[]    ys = ys
type instance FilterLE t (x:xs) ys = FilterLE_If (x <=? t) t x xs ys

type family FilterLE_If (p :: Bool) (t :: Nat) (x :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance FilterLE_If 'True  t x xs ys = FilterLE t xs (x:ys)
type instance FilterLE_If 'False t x xs ys = FilterLE t xs ys

type family FilterGT (t :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance FilterGT t '[]    ys = ys
type instance FilterGT t (x:xs) ys = FilterGT_If (x <=? t) t x xs ys

type family FilterGT_If (p :: Bool) (t :: Nat) (x :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance FilterGT_If 'True  t x xs ys = FilterGT t xs ys
type instance FilterGT_If 'False t x xs ys = FilterGT t xs (x:ys)

type family QSortD (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance QSortD '[]  ys = ys
type instance QSortD (x:xs) ys = CaseNonNull x xs ys

type family CaseNonNull (x :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat]
type instance CaseNonNull x '[] ys = x:ys
type instance CaseNonNull x (x':xs) ys =
  QSortD
    (FilterLE x (x':xs) '[])
    (x : QSortD (FilterGT x (x':xs) '[]) ys)

type family QSort (xs :: [Nat]) :: [Nat]
type instance QSort xs = QSortD xs '[]
