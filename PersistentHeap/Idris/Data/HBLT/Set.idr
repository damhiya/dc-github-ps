module Data.HBLT.Set

import Decidable.Order
import Decidable.Order.Maybe

%default total

namespace Min
  data Leftist : (a : Type) -> (o : Ordered a to) -> (s : Nat) -> Maybe a -> Type where
    Node  : .{o : Ordered a to} ->
            (x : a) ->
            (l : Leftist a o sl pl) ->
            (r : Leftist a o sr pr) ->
            .(leftist : GTE sl sr) ->
            .(priorl : OrderMaybe to (Just x) pl) ->
            .(priorr : OrderMaybe to (Just x) pr) ->
            Leftist a o (S sr) (Just x)
    Nil : Leftist a o 0 Nothing

  data Heap : (a : Type) -> (o : Ordered a to) -> Type where
    MkHeap : Leftist a o s p -> Heap a o

  LTEM : (to : a -> a -> Type) -> Maybe a -> Maybe a -> Type
  LTEM = OrderMaybe

  onat : Ordered Nat LTE
  onat = %implementation

  om : Ordered a to -> Ordered (Maybe a) (OrderMaybe to)
  om o = %implementation

  transEither : {o : Ordered a po} -> po x y1 -> po x y2 -> Either (po y1 z) (po y2 z) -> po x z
  transEither {x=x} {y1=y} {z=z} oxy _ (Left oyz)  = transitive x y z oxy oyz
  transEither {x=x} {y2=y} {z=z} _ oxy (Right oyz) = transitive x y z oxy oyz

  swapEither : Either a b -> Either b a
  swapEither (Left  x) = Right x
  swapEither (Right x) = Left  x

  mergeLeftist :  {o : Ordered a to} ->
                  Leftist a o s1 p1 ->
                  Leftist a o s2 p2 ->
                  (s ** p ** prf : Either (LTEM to p1 p) (LTEM to p2 p) ** Leftist a o s p)
  mergeLeftist {p2=p} Nil h   = (_ ** _ ** Right (reflexive p) ** h)
  mergeLeftist {p1=p} h   Nil = (_ ** _ ** Left  (reflexive p) ** h)
  mergeLeftist {a=a} {o=o} {to=to} (Node {sl=sl1} x1 l1 r1 lft1 prl1 prr1) (Node {sl=sl2} x2 l2 r2 lft2 prl2 prr2) =
    case (order @{o} x1 x2) of
      Left  o12 {- x1 <= x2 -} => case (let h2 = Node x2 l2 r2 lft2 prl2 prr2 in mergeLeftist r1 h2) of
        (sb ** pb ** prfb ** b) => case (order @{onat} sl1 sb) of
          Left  lft {- sb >= sl1 -} =>
            let { lprf = reflexive (Just x1)
                ; prl  = transEither {o=om o} prr1 (OrderJJ o12) prfb
                ; prr  = prl1
                } in (_ ** _ ** Left lprf ** Node x1 b l1 lft prl prr)
          Right lft {- sl1 >= sb -} =>
            let { lprf = reflexive (Just x1)
                ; prl  = prl1
                ; prr  = transEither {o=om o} prr1 (OrderJJ o12) prfb
                } in (_ ** _ ** Left lprf ** Node x1 l1 b lft prl prr)
      Right o21 {- x2 <= x1 -} => case mergeLeftist (Node x2 l2 r2 lft2 prl2 prr2) (Node x1 l1 r1 lft1 prl1 prr1) of
        (sb ** pb ** prfb ** b) => (sb ** pb ** swapEither prfb ** b)

      -- case (let h1 = Node x1 l1 r1 lft1 prl1 prr1 in mergeLeftist r2 h1) of
      --   (sb ** pb ** prfb ** b) => case (order @{onat} sl2 sb) of
      --     Left  lft {- sb >= sl2 -} => ?hole1
      --     -- let { rprf = reflexive (Just x2)
      --     --     ; prl  = ?prl
      --     --     ; prr  = ?prr
      --     --     } in ( _ ** _ ** Right rprf ** Node x2 b l2 lft prl prr)
      --     Right lft {- sl1 >= sb -} => ?hole2

  empty : Heap a o
  empty = MkHeap Nil

  singleton : (x : a) -> Heap a o
  singleton x = MkHeap (Node x Nil Nil LTEZero OrderJN OrderJN)

  merge : Heap a o -> Heap a o -> Heap a o
  merge (MkHeap h1) (MkHeap h2) with (mergeLeftist h1 h2)
    | (_ ** _ ** _ ** h) = MkHeap h
