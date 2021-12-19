module ThreeStar exposing (..)

--problem 1

type Shape
  = Circle Float
  | Rectangle Float Float
  | Triangle Float Float Float

heron a b c =
  let
    s = (a + b + c) / 2
  in
    sqrt (s * (s - a) * (s - b) * (s - c))

safeArea : Shape -> Result String Float
safeArea shape =
  case shape of
    Circle radius ->
      if radius < 0 then
        Err "Negative circle radius"
      else
        Ok (pi * radius * radius)
    Rectangle width height ->
      if (width < 0) || (height < 0) then
        Err "Negative rectangle width or height"
      else
        Ok (width * height)
    Triangle a b c ->
      case safeHeron a b c of
        Just area -> Ok area
        Nothing -> Err "Sides can't form a triangle"

validTriangle a b c =
  ((a > 0) && (b > 0) && (c > 0)) &&
  ((a + b >= c) && (a + c >= b) && (b + c >= a))

safeHeron : Float -> Float -> Float -> Maybe Float
safeHeron a b c =
  if not (validTriangle a b c) then
    Nothing
  else
    Just (heron a b c)

cmpShapes : Shape -> Shape -> Result String Order
cmpShapes shape1 shape2 = 
    let 
        a1 = safeArea shape1 
        a2 = safeArea shape2
    in 
    case (a1, a2) of
        (Ok r1, Ok r2) -> if ( r1 == r2) then Ok (EQ) else 
                          if (r1 < r2 ) then Ok (LT)
                          else Ok (GT) 
        (Err err, _) -> Err ("Shape1 cannot have the area calculated :" ++ err)
        (_, Err errr) -> Err ("Shape2 cannot have the area calculated:"  ++ errr)   

-- pb2 

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