module Decidable.Order.Maybe

import Decidable.Order

%access public export
%default total

data OrderMaybe : (a -> a -> Type) -> Maybe a -> Maybe a -> Type where
  OrderNN : OrderMaybe to Nothing Nothing
  OrderJN : OrderMaybe to (Just x) Nothing
  OrderJJ : o x y -> OrderMaybe o (Just x) (Just y)

implementation (Preorder a po) => Preorder (Maybe a) (OrderMaybe po) where
  transitive Nothing  Nothing  Nothing  _ _ = OrderNN
  transitive (Just x) Nothing  Nothing  _ _ = OrderJN
  transitive (Just x) (Just y) Nothing  _ _ = OrderJN
  transitive (Just x) (Just y) (Just z) (OrderJJ oxy) (OrderJJ oyz) = OrderJJ (transitive x y z oxy oyz)
  reflexive Nothing  = OrderNN
  reflexive (Just x) = OrderJJ (reflexive x)

implementation (Poset a po) => Poset (Maybe a) (OrderMaybe po) where
  antisymmetric Nothing Nothing _ _ = Refl
  antisymmetric (Just x) (Just y) (OrderJJ oxy) (OrderJJ oyx) = cong (antisymmetric x y oxy oyx)

implementation (Ordered a to) => Ordered (Maybe a) (OrderMaybe to) where
  order Nothing  Nothing  = Left OrderNN
  order (Just x) Nothing  = Left OrderJN
  order Nothing  (Just y) = Right OrderJN
  order {to=to} (Just x) (Just y) with (the (Either (to x y) (to y x)) (order x y))
    | Left  oxy = Left  (OrderJJ oxy)
    | Right oyx = Right (OrderJJ oyx)

ordInfinity : {x : Maybe a} -> OrderMaybe to x Nothing
ordInfinity {x=Nothing} = OrderNN
ordInfinity {x=Just x } = OrderJN