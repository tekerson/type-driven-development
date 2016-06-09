import Data.Vect

add_row : Num num => (xs : Vect cols num) -> (ys : Vect cols num) -> Vect cols num
add_row [] [] = []
add_row (x :: xs) (y :: ys) = (x + y) :: add_row xs ys

add : Num num =>
      Vect rows (Vect cols num) -> Vect rows (Vect cols num) ->
      Vect rows (Vect cols num)
add [] [] = []
add (x :: xs) (y :: ys) = let rest = add xs ys
                          in add_row x y :: rest

create_empties : Vect n (Vect 0 el)
create_empties {n} = replicate n []

transpose_mat : Vect n (Vect m el) -> Vect m (Vect n el)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = zipWith (::) x (transpose_mat xs)

calc_row : Num num => (xs : Vect m num) -> (rgt : Vect p (Vect m num)) -> Vect p num
calc_row xs ys = map (\x => sum (zipWith (*) xs x)) ys

mult : Num num =>
       Vect n (Vect m num) -> Vect m (Vect p num) ->
       Vect n (Vect p num)
mult lft rgt = let rgt' = transpose_mat rgt in
               map (\r => calc_row r rgt') lft
