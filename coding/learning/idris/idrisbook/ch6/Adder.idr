AdderType : (numargs : Nat) -> Type -> Type
AdderType Z t = t
AdderType (S k) t = (next : t) -> AdderType k t

adder : Num n => (numargs : Nat) -> (acc : n) -> AdderType numargs n
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)
