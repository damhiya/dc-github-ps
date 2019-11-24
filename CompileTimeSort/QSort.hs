{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE DataKinds      #-}

{-# LANGUAGE UndecidableInstances #-}

module QSort where

import GHC.TypeLits

type family FilterLE (t :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  FilterLE t '[]    ys = ys
  FilterLE t (x:xs) ys = FilterLE_If (x <=? t) t x xs ys

type family FilterLE_If (p :: Bool) (t :: Nat) (x :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  FilterLE_If 'True  t x xs ys = FilterLE t xs (x:ys)
  FilterLE_If 'False t x xs ys = FilterLE t xs ys

type family FilterGT (t :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  FilterGT t '[]    ys = ys
  FilterGT t (x:xs) ys = FilterGT_If (x <=? t) t x xs ys

type family FilterGT_If (p :: Bool) (t :: Nat) (x :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  FilterGT_If 'True  t x xs ys = FilterGT t xs ys
  FilterGT_If 'False t x xs ys = FilterGT t xs (x:ys)

type family QSortD (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  QSortD '[]  ys = ys
  QSortD (x:xs) ys = CaseNonNull x xs ys

type family CaseNonNull (x :: Nat) (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  CaseNonNull x '[] ys = x:ys
  CaseNonNull x (x':xs) ys =
    QSortD
      (FilterLE x (x':xs) '[])
      (x : QSortD (FilterGT x (x':xs) '[]) ys)

type family QSort (xs :: [Nat]) :: [Nat] where
  QSort xs = QSortD xs '[]
