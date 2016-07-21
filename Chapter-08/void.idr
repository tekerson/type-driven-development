
twoplustwo_not_five : 2 + 2 = 5 -> Void
twoplustwo_not_five Refl impossible

value_not_suc : (x : Nat) -> x = S x -> Void
value_not_suc _ Refl impossible

zero_not_succ : (0 = S k) -> Void
zero_not_succ Refl impossible

succ_not_zero : (S k = 0) -> Void
succ_not_zero Refl impossible

no_rec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
no_rec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zero_not_succ
checkEqNat (S k) Z = No succ_not_zero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              (Yes prf) => Yes (cong prf)
                              (No contra) => No (no_rec contra)
