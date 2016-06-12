AdderType : (numType : Type) -> (argc : Nat) -> Type
AdderType numType Z = numType
AdderType numType (S k) = (next : numType) -> AdderType numType k

adder : Num numType =>
  (argc : Nat) -> (acc : numType) -> AdderType numType argc
adder Z acc = acc
adder (S k) acc = \next => adder k (acc + next)
