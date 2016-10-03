public export
data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

Functor InfList where
    map func (value :: xs) = func value :: Delay (map func xs)

%name InfList xs, ys, zs
