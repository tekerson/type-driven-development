data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

Eq Shape where
    (==) (Triangle x z) (Triangle x' z') = x == x' && z == z'
    (==) (Rectangle x z) (Rectangle x' z') = x == x' && z == z'
    (==) (Circle x) (Circle x') = x == x'
    (==) _ _ = False

Ord Shape where
  compare x y = compare (area x) (area y)
