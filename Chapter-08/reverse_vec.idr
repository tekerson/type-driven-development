import Data.Vect

my_plusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
my_plusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
my_plusCommutes (S k) m = rewrite my_plusCommutes k m in
                          rewrite plusSuccRightSucc m k in
                          Refl

my_reverse : Vect n elem -> Vect n elem
my_reverse [] = []
my_reverse (x :: xs) = reverseProof (my_reverse xs ++ [x])
  where
    reverseProof : Vect (plus k 1) elem -> Vect (S k) elem
    reverseProof {k} result = rewrite my_plusCommutes 1 k in result

my_reverse' : Vect n elem -> Vect n elem
my_reverse' xs = reverse' [] xs
  where
    reverseProof_nil : Vect n1 a -> Vect (plus n1 0) a
    reverseProof_nil {n1} acc = rewrite plusZeroRightNeutral n1 in acc

    reverseProof_xs : Vect ((S n) + k) a -> Vect (plus n (S k)) a
    reverseProof_xs {n} {k} xs = rewrite sym (plusSuccRightSucc n k) in xs

    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc [] = reverseProof_nil acc
    reverse' acc (x::xs) = reverseProof_xs (reverse' (x::acc) xs)

