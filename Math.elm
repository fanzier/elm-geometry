module Math where

import Basics (..)
import Maybe

import Types (..)

-- * Vector operations

plus : Vector -> Vector -> Vector
plus p1 p2 = { x = p1.x + p2.x, y = p1.y + p2.y }

minus : Vector -> Vector -> Vector
minus p1 p2 = { x = p1.x - p2.x, y = p1.y - p2.y }

times : number -> Vector -> Vector
times number { x, y } = { x = number * x, y = number * y }

dot : Vector -> Vector -> Float
dot p q = p.x * q.x + p.y * q.y

norm : Vector -> Float
norm v = sqrt (dot v v)

normalize : Vector -> Vector
normalize v = let n = norm v in { x = v.x / n, y = v.y / n }

rotate90 : Vector -> Vector
rotate90 { x, y } = { x = -y, y = x }

-- * Constructing lines and circles

pointsToLineDescription : Vector -> Vector -> Maybe LineDescription
pointsToLineDescription p q =
  let diff = q `minus` p
      normal = normalize <| rotate90 diff
  in
    if diff == { x = 0, y = 0 }
    then Nothing
    else Just { normal = normal, offset = p `dot` normal }

-- | Finds two points on a line that are at least 2 * `distance` apart.
findRemotePointsOnLine : number -> LineDescription -> (Vector, Vector)
findRemotePointsOnLine distance { normal, offset } =
  let point = offset `times` normal
      tangent = rotate90 normal
      shift = distance `times` tangent
  in
    (point `minus` shift, point `plus` shift)

pointsToCircleDescription : Vector -> Vector -> Maybe CircleDescription
pointsToCircleDescription p q =
  let radius = norm (p `minus` q)
  in
    if radius == 0
    then Nothing
    else Just { center = p, radius = radius }

-- * Find out whether points are on lines or circles

threshold : number
threshold = 10

closeEnough : Vector -> Vector -> Bool
closeEnough p q = norm (p `minus` q) < threshold

isOnLine : Vector -> Maybe LineDescription -> Bool
isOnLine p desc = case desc of
  Just { offset, normal } -> abs ((normal `dot` p) - offset) < threshold
  Nothing -> False

isOnCircle : Vector -> Maybe CircleDescription -> Bool
isOnCircle p desc = case desc of
  Just { center, radius } -> abs (norm (center `minus` p) - radius) < threshold
  Nothing -> False

-- * Compute Intersections

lineLineIntersection : LineDescription -> LineDescription -> Maybe Vector
lineLineIntersection line1 line2 =
  let determinant = rotate90 line1.normal `dot` line2.normal in
  if determinant == 0
  then Nothing
  else Just
    { x = (line1.offset * line2.normal.y - line1.normal.y * line2.offset)
        / determinant
    , y = (line1.normal.x * line2.offset - line1.offset * line2.normal.x)
        / determinant
    }

lineCircleIntersection : WhichOne -> LineDescription -> CircleDescription -> Maybe Vector
lineCircleIntersection whichOne line circle =
  let
    sign = getSign whichOne
    centerDistance = line.offset - circle.center `dot` line.normal
    centerToProjection = centerDistance `times` line.normal
    projectionOfCenter = centerToProjection `plus` circle.center
  in if abs centerDistance > circle.radius then Nothing else
    let projectionToIntersectionDistance
          = sqrt (circle.radius ^ 2 - centerDistance ^ 2)
    in Just <| projectionOfCenter `plus`
      ((projectionToIntersectionDistance * sign) `times` rotate90 line.normal)
  
getSign : WhichOne -> number
getSign whichOne = case whichOne of
  PositiveOne -> 1
  NegativeOne -> -1

circleCircleIntersection : WhichOne -> CircleDescription -> CircleDescription -> Maybe Vector
circleCircleIntersection whichOne circle1 circle2 = if circle1.center == circle2.center then Nothing else
  let 
    sign = getSign whichOne
    distance = norm centerVector
    centerVector = circle2.center `minus` circle1.center
    normalizedCenterVector = normalize centerVector
    commonSecantMiddleDistanceFromCircle1
      = (circle1.radius ^ 2 - circle2.radius ^ 2 + distance ^ 2)
      / (2 * distance)
    commonSecantMiddle = (commonSecantMiddleDistanceFromCircle1 `times` normalizedCenterVector) `plus` circle1.center
    intersectionOffsetSquare = circle1.radius ^ 2 - commonSecantMiddleDistanceFromCircle1 ^ 2
  in if intersectionOffsetSquare < 0 then Nothing else
  let intersectionOffset = sign * sqrt intersectionOffsetSquare
  in Just <| commonSecantMiddle `plus` (intersectionOffset `times` rotate90 normalizedCenterVector)
    
-- | When a line and a circle intersect, there can be two intersection points.
-- This function computes which one is closer to a given point.
chooseCloserPointLC : Model -> LineDescription -> CircleDescription -> Vector -> { first : ID, second : ID } -> Intersection
chooseCloserPointLC m ld cd closePoint inter =
  if rotate90 ld.normal `dot` closePoint < rotate90 ld.normal `dot` cd.center
  then { inter | whichOne = NegativeOne }
  else { inter | whichOne = PositiveOne }

-- | When two circles intersect, there can be two intersection points.
-- This function computes which one is closer to a given point.
chooseCloserPointCC : Model -> CircleDescription -> CircleDescription -> Vector -> { first : ID, second : ID } -> Intersection
chooseCloserPointCC m c1 c2 closePoint inter =
  let centerVector = (c2.center `minus` c1.center)
  in if rotate90 centerVector `dot` (closePoint `minus` c1.center) < 0
  then { inter | whichOne = NegativeOne }
  else { inter | whichOne = PositiveOne }

