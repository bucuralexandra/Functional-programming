module Problem2 exposing(..)

--problem2 

type Shape
  = Circle Float
  | Rectangle Float Float
  | Triangle Float Float Float

heron a b c =
  let
    s = (a + b + c) / 2
  in
    sqrt (s * (s - a) * (s - b) * (s - c))

type InvalidShapeError
  = InvalidCircle
  | InvalidRectangle InvalidRectangleError
  | InvalidTriangle InvalidTriangleError

type InvalidRectangleError
  = InvalidWidth
  | InvalidHeight

type InvalidTriangleError
 = NegativeSide TriangleSide
 | ImpossibleTriangle

type TriangleSide = A | B | C

safeAreaEnum : Shape -> Result InvalidShapeError Float
safeAreaEnum shape =
  case shape of
    Circle radius ->
      if radius < 0 then
        Err InvalidCircle
      else
        Ok (pi * radius * radius)
    Rectangle width height ->
      if (width < 0) then
        Err (InvalidRectangle InvalidWidth)
      else if (height < 0) then
        Err (InvalidRectangle InvalidHeight)
      else
        Ok (width * height)
    Triangle a b c -> 
      case safeHeronEnum a b c of
        Ok area -> Ok area
        Err err -> Err (InvalidTriangle err)

safeHeronEnum : Float -> Float -> Float -> Result InvalidTriangleError Float
safeHeronEnum a b c =
  if (a < 0) then
    Err (NegativeSide A)
  else if (b < 0) then
    Err (NegativeSide B)
  else if (c < 0) then
    Err (NegativeSide C)
  else if ((a + b < c) || (a + c < b) || (b + c < a)) then
    Err ImpossibleTriangle
  else Ok (heron a b c)


totalArea : List Shape -> Result (Int, InvalidShapeError) Float
totalArea l = 
   let totalAreaIndex sum index lx = 
        case lx of
        [] -> Ok (sum)
        x::xs -> 
         let 
            current_area = safeAreaEnum x
         in 
            case current_area of
                Err err -> Err(index, err)
                Ok a -> totalAreaIndex (sum + a) (index+1) xs
    in 
    totalAreaIndex 0 0 l




