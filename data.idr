data Direction = North | East | South | West

turnClockwise: Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

||| Represents shapes
data Shape = ||| triangle, base and height
             Triangle Double Double 
           | ||| rectangle, length and height
             Rectangle Double Double 
           | ||| circle, radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

testPicture : Picture
testPicture = let rectangle = Rectangle 20 10
                  rectanglePos = Translate 5 5 (Primitive rectangle)
                  circle = Circle 5
                  circlePos = Translate 35 5 (Primitive circle)
                  triangle = Triangle 10 10 
                  trianglePos = Translate 15 25 (Primitive triangle) in
                  Combine rectanglePos (Combine circlePos trianglePos) 

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive (Triangle x y)) = Just (area (Triangle x y))
biggestTriangle (Primitive (Rectangle x y)) = Nothing
biggestTriangle (Primitive (Circle x)) = Nothing
biggestTriangle (Combine x y) = let left = biggestTriangle x
                                    right = biggestTriangle y in 
                                bigger left right
                                where
                                  bigger : Maybe Double -> Maybe Double -> Maybe Double
                                  bigger Nothing x = x
                                  bigger lh@ (Just x) Nothing = lh 
                                  bigger (Just x) (Just y) = Just (if x > y then x else y)
biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z
