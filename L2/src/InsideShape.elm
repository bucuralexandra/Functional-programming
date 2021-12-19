module InsideShape exposing(..)

type alias Point = {x: Float, y: Float}

type Shape2D
 = Circle {center: Point, radius: Float}
   | Rectangle {topLeftCorner: Point, bottomRightCorner: Point}
   | Triangle {pointA: Point, pointB: Point, pointC: Point}

--Circle definition
centerCircle = Point 2.0 5.0
circle = Circle { center = centerCircle, radius = 2.5}

--Rectangle definition
top = Point 4.0 6.0
bottom = Point 5.0 1.0
rectangle = Rectangle { topLeftCorner = top, bottomRightCorner = bottom}

--Triangle definition
pa = Point 4.0 6.0
pb = Point 5.0 1.0
pc = Point 0.0 1.0
triangle = Triangle {pointA = pa, pointB = pb, pointC = pc} 

--Function that checks if a point is inside a circle
insideCircle : Point -> Point -> Float -> Bool
insideCircle p c r =
    let
       dist = distance p c
    in
     dist < r

--Function that checks if a point is inside a rectangle
insideRectangle : Point -> Point -> Point -> Bool
insideRectangle p topLeftCorner bottomRightCorner = 
         if topLeftCorner.x < p.x && p.x < bottomRightCorner.x
            && bottomRightCorner.y < p.y && p.y < topLeftCorner.y
         then True
         else False

--Function that checks if a point is inside a triangle
insideTriangle : Point -> Point -> Point -> Point -> Bool
insideTriangle p a b c = 
         let 
            heronA = heronShort ( distance p b) (distance p c) (distance b c)
            heronB = heronShort ( distance p a) (distance a c) (distance a c)
            heronC = heronShort ( distance p b) (distance p a) (distance b a)
            heron  = heronShort ( distance a b) (distance a c) (distance b c)
        in 
             if heron == heronA + heronB + heronC then True 
            else False

pointInShape : Shape2D -> Point -> Bool
pointInShape shape point = 
    case shape of
        Circle { center, radius } -> insideCircle point center radius
        Rectangle { topLeftCorner, bottomRightCorner} -> insideRectangle point topLeftCorner bottomRightCorner
        Triangle { pointA, pointB, pointC} -> insideTriangle point pointA pointB pointC

distance a b = 
   let 
      d = ((a.x - b.x) * (a.x - b.x)) + ((a.y - b.y) * (a.y - b.y))
   in 
      sqrt(d)
   
heronShort a b c =
    let
       s = (a + b+ c) / 2
    in
      sqrt ( s * ( s - a) * (s - b) * (s - c))