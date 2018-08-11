AdderType : (numargs : Nat) -> Type -> Type
AdderType Z a = a
AdderType (S k) a = (next: a) -> AdderType k a

adder : Num a => (numargs : Nat) -> (acc : a) -> AdderType numargs a
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)
