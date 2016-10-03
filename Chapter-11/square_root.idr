square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx =
  let next = (approx + (number / approx)) / 2
  in approx :: Delay (square_root_approx number next)

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
square_root_bound Z number bound (approx :: approxs) = approx
square_root_bound (S k) number bound (approx :: approxs)
  = if (within_bound number bound approx)
    then approx
    else square_root_bound k number bound approxs
  where
    within_bound : (number : Double) -> (bound : Double) -> (approx : Double) -> Bool
    within_bound number bound approx = abs((approx * approx) - number) < bound

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.0000000001
                                       (square_root_approx number number)
