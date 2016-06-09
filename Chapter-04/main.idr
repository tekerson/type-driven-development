data Direction = North
               | East
               | South
               | West

turn_clockwise : Direction -> Direction
turn_clockwise North = East
turn_clockwise East = South
turn_clockwise South = West
turn_clockwise West = North

data Shape = ||| A triangle, with base length and height
             Triangle Double Double
           | ||| A rectangle with length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

test_pic : Picture
test_pic = Combine (Translate 5 5 rectangle)
                   (Combine (Translate 35 5 circle)
                            (Translate 15 25 triangle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

picture_area : Picture -> Double
picture_area (Primitive shape) = area shape
picture_area (Combine pic pic1) = picture_area pic + picture_area pic1
picture_area (Rotate x pic) = picture_area pic
picture_area (Translate x y pic) = picture_area pic

data Tree a = Empty
            | Node (Tree a) a (Tree a)

%name Tree tree, tree1

insert : Ord a =>
         a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left y right) = case compare x y of
                                    LT => Node (insert x left) y right
                                    EQ => orig
                                    GT => Node left y (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree xs = foldr insert Empty xs

foldrTree : (a -> as -> as) -> as -> Tree a -> as
foldrTree f x Empty = x
foldrTree f x (Node lft y rgt) = let rgt' = (foldrTree f x rgt) in
                                 foldrTree f (f y rgt') lft

treeToList : Tree a -> List a
treeToList tree = foldrTree (::) [] tree

data Expr = PrimInt Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (PrimInt x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing x = x
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just (max x y)

triangleSize : Shape -> Maybe Double
triangleSize tri@(Triangle _ _) = Just (area tri)
triangleSize _ = Nothing

biggestFound : Maybe Double -> Picture -> Maybe Double
biggestFound x (Primitive p) = maxMaybe x (triangleSize p)
biggestFound x (Combine pic pic1) = biggestFound (biggestFound x pic) pic1
biggestFound x (Rotate y pic) = biggestFound x pic
biggestFound x (Translate y z pic) = biggestFound x pic

biggestTriangle : Picture -> Maybe Double
biggestTriangle pic = biggestFound Nothing pic


data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle p -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Car 200
