public export
data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

export
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just eq => Just (sameS _ _ eq)

checkEqNatMay : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNatMay Z Z         = Just Refl
checkEqNatMay Z (S k)     = Nothing
checkEqNatMay (S k) Z     = Nothing
checkEqNatMay (S k) (S j) = case checkEqNatMay k j of
                                 Nothing => Nothing
                                 Just eq => Just (cong eq)
