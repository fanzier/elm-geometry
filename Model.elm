module Model where

import Basics (..)
import Dict
import List
import Maybe (..)
import Maybe
import Set

import Math (..)
import Types (..)

-- * Adding objects to the model

-- ** Adding free points

newFreePoint : Vector -> Model -> Model
newFreePoint v m =
  let id = m.newID
  in { m
     | objects <- Dict.insert m.newID (freePoint id v) m.objects
     , newID <- id + 1
     }

freePoint : ID -> Vector -> GeoObject
freePoint id v = { id = id, name = "P" ++ toString id, revDeps = Set.empty, deps = Set.empty, object = FreePoint v }

-- ** Adding intersection points

newIntersection : ID -> ID -> Vector -> Model -> Model
newIntersection id1 id2 closePoint m =
  let obj1 = fromJust <| Dict.get id1 m.objects
      obj2 = fromJust <| Dict.get id2 m.objects
      inter = case (obj1.object, obj2.object) of
        (Straight line1, Straight line2) -> { first = id1, second = id2, whichOne = PositiveOne }
        (Straight line, Circle circle) -> chooseCloserPointLC m (fromJust <| line.desc) (fromJust <| circle.desc) closePoint { first = id1, second = id2}
        (Circle circle, Straight line) -> chooseCloserPointLC m (fromJust <| line.desc) (fromJust <| circle.desc) closePoint { first = id1, second = id2}
        (Circle circle1, Circle circle2) -> chooseCloserPointCC m (fromJust <| circle1.desc) (fromJust <| circle2.desc) closePoint { first = id1, second = id2 }
      id = m.newID
      v = computeIntersection m inter
  in addRevDep id inter.first <| addRevDep id inter.second <| { m
     | objects <- Dict.insert m.newID (intersectPoint id inter v) m.objects
     , newID <- id + 1
     }

computeIntersection : Model -> Intersection -> Maybe Vector
computeIntersection model inter = 
  Dict.get inter.first model.objects `andThen` \obj1 ->
  Dict.get inter.second model.objects `andThen` \obj2 ->
  case (obj1.object, obj2.object) of
    (Straight line1, Straight line2) -> line1.desc `andThen` \l1 -> line2.desc `andThen` \l2 -> lineLineIntersection l1 l2
    (Straight line, Circle circle) -> line.desc `andThen` \l -> circle.desc `andThen` \c -> lineCircleIntersection inter.whichOne l c
    (Circle circle, Straight line) -> line.desc `andThen` \l -> circle.desc `andThen` \c -> lineCircleIntersection inter.whichOne l c
    (Circle circle1, Circle circle2) -> circle1.desc `andThen` \c1 -> circle2.desc `andThen` \c2 -> circleCircleIntersection inter.whichOne c1 c2

intersectPoint : ID -> Intersection -> Maybe Vector -> GeoObject
intersectPoint id inter v = { id = id, name = "S" ++ toString id, revDeps = Set.empty, deps = Set.fromList [inter.first, inter.second], object = Intersect { intersection = inter, point = v } }

-- ** Adding lines

newStraightLine : ID -> ID -> Model -> Model
newStraightLine p1 p2 m =
  let id = m.newID
      desc = computeLineDescription m p1 p2
  in addRevDep id p1 <| addRevDep id p2 <|
     { m
     | objects <- Dict.insert m.newID (straightLine id p1 p2 desc) m.objects
     , newID <- id + 1
     }
     
computeLineDescription : Model -> ID -> ID -> Maybe LineDescription
computeLineDescription m p1 p2 =
  Dict.get p1 m.objects `andThen` \point1 ->
  Dict.get p2 m.objects `andThen` \point2 ->
  (case point1.object of
    FreePoint v -> Just v
    Intersect { point } -> point) `andThen` \x ->
  (case point2.object of
    FreePoint v -> Just v
    Intersect { point } -> point) `andThen` \y ->
    pointsToLineDescription x y 

straightLine : ID -> ID -> ID -> Maybe LineDescription -> GeoObject
straightLine id p1 p2 desc = { id = id, name = "l" ++ toString id, revDeps = Set.empty, deps = Set.fromList [p1, p2], object = Straight { first = p1, second = p2, desc = desc } }

-- ** Adding circles

newCircle : ID -> ID -> Model -> Model
newCircle center edgePoint m =
  let id = m.newID
      desc = computeCircleDescription m center edgePoint
  in addRevDep id center <| addRevDep id edgePoint <| { m
     | objects <- Dict.insert m.newID (circleObject id center edgePoint desc) m.objects
     , newID <- id + 1
     }

circleObject : ID -> ID -> ID -> Maybe CircleDescription -> GeoObject
circleObject id center edgePoint desc = { id = id, name = "c" ++ toString id, revDeps = Set.empty, deps = Set.fromList [center, edgePoint], object = Circle { center = center, edgePoint = edgePoint, desc = desc } }

computeCircleDescription : Model -> ID -> ID -> Maybe CircleDescription
computeCircleDescription m p1 p2 =
  Dict.get p1 m.objects `andThen` \point1 ->
  Dict.get p2 m.objects `andThen` \point2 ->
  (case point1.object of
    FreePoint v -> Just v
    Intersect { point } -> point) `andThen` \x ->
  (case point2.object of
    FreePoint v -> Just v
    Intersect { point } -> point) `andThen` \y ->
    pointsToCircleDescription x y 

-- * Recomputing the dependency tree

recomputeObject : Model -> Object -> Object 
recomputeObject model obj = case obj of
  FreePoint v -> FreePoint v
  Intersect { intersection, point } -> Intersect { intersection = intersection, point = computeIntersection model intersection }
  Straight { first, second, desc } -> Straight  { first = first, second = second, desc = computeLineDescription model first second }
  Circle { center, edgePoint, desc } -> Circle { center = center, edgePoint = edgePoint, desc = computeCircleDescription model center edgePoint }

recomputeGeoObject : Model -> GeoObject -> GeoObject
recomputeGeoObject model geo = { geo | object <- recomputeObject model geo.object }

updateRevDeps : ID -> Model -> Model
updateRevDeps id model =
  let obj = fromJust <| Dict.get id model.objects
      objects' = Dict.update id (Maybe.map (recomputeGeoObject model)) model.objects
      model' = { model | objects <- objects' }
  in Set.foldr updateRevDeps model' obj.revDeps

addRevDep : ID -> ID -> Model -> Model
addRevDep what wher m =
  let addDep geo = { geo | revDeps <- Set.insert what geo.revDeps }
  in { m | objects <- Dict.update wher (map addDep) m.objects }

removeRevDep : ID -> ID -> Model -> Model
removeRevDep what wher m =
  let removeDep geo = { geo | revDeps <- Set.remove what geo.revDeps }
  in { m | objects <- Dict.update wher (map removeDep) m.objects }

removeRevDeps : ID -> Model -> Model
removeRevDeps what m = List.foldr (removeRevDep what) m (Dict.keys m.objects)

deleteObject : ID -> Model -> Model
deleteObject id model =
  let obj = fromJust <| Dict.get id model.objects
      objects' = Dict.remove id model.objects
      model' = removeRevDeps id { model | objects <- objects' }
  in Set.foldr deleteObject model' obj.revDeps

-- * Reacting to user input and updating the model

update : UserInput -> Model -> Model
update action m = case action of
  MouseEvent (x',y') down ->
    let (x,y) = transformMouseCoords m.dimensions (x',y')
        pos = { x = x, y = y }
    in case (m.mouseDown, down) of
      (True, False) -> (if m.mouseDragged then processEndDrag else processMouseUp) pos { m | mouseDragged <- False, mouseDown <- False }
      (False, True) -> processMouseDown pos { m | mouseDown <- True, mouseDragged <- False }
      (True, True) -> processDrag m.mousePosition pos { m | mouseDragged <- True, mousePosition <- pos }
      (False, False) -> processMove m.mousePosition pos { m | mouseDragged <- False, mousePosition <- pos }
  ChangeMode mode -> case (m.mode, mode) of
    (Selected id, Delete) -> deleteObject id { m | mode <- DefaultMode }
    _ -> { m | mode <- mode }
  _ -> m

processMouseDown : Vector -> Model -> Model
processMouseDown { x, y } m = case m.mode of
  DefaultMode ->
    case findClosestPointAt (x,y) m of
      Just p -> { m | mode <- Dragging p False m.mode }
      Nothing -> m
  _ -> m

processDrag : Vector -> Vector -> Model -> Model
processDrag from to m = case m.mode of
  Dragging p _ mode ->
    let objects' = Dict.update p modCoords m.objects
        modCoords = Maybe.map (\p -> case p.object of
          FreePoint v -> { p | object <- FreePoint { x = to.x - from.x + v.x, y = to.y - from.y + v.y } }
          _ -> p
          )
    in updateRevDeps p { m | objects <- objects', mode <- Dragging p True mode }
  _ -> m

processMove : Vector -> Vector -> Model -> Model
processMove _ pos m = { m | hovered <- findObjectIDsAt pos m }

processEndDrag : Vector -> Model -> Model
processEndDrag { x, y } m = case m.mode of
  Dragging _ _ mode -> { m | mode <- mode }
  _ -> m

processMouseUp : Vector -> Model -> Model
processMouseUp { x, y } m = case m.mode of
  DefaultMode -> newFreePoint { x = x, y = y } m
  Selected _ -> {m | mode <- DefaultMode }
  DrawLine0 -> case findClosestPointAt (x,y) m of
    Just p -> { m | mode <- DrawLine1 p }
    Nothing -> { m | mode <- DefaultMode }
  DrawLine1 p -> case findClosestPointAt (x,y) m of
    Just q -> newStraightLine p q { m | mode <- DefaultMode }
    Nothing -> { m | mode <- DefaultMode }
  DrawCircle0 -> case findClosestPointAt (x,y) m of
    Just p -> { m | mode <- DrawCircle1 p }
    Nothing -> { m | mode <- DefaultMode }
  DrawCircle1 p -> case findClosestPointAt (x,y) m of
    Just q -> newCircle p q { m | mode <- DefaultMode }
    Nothing -> { m | mode <- DefaultMode }
  Intersect0 -> case Set.toList <| findOneDimObjectIDsAt { x = x, y = y } m of
    [] -> { m | mode <- DefaultMode }
    [id] -> { m | mode <- Intersect1 id }
    _ -> { m | mode <- DefaultMode }
  Intersect1 id1 -> case Set.toList <| findOneDimObjectIDsAt { x = x, y = y } m of
    [] -> { m | mode <- DefaultMode }
    [id2] -> newIntersection id1 id2 { x = x, y = y } { m | mode <- DefaultMode }
    _ -> { m | mode <- DefaultMode }
  Delete -> case Set.toList <| findObjectIDsAt { x = x, y = y } m of
    [id] -> deleteObject id { m | mode <- DefaultMode }
    _ -> { m | mode <- DefaultMode }
  Dragging _ _ mode -> case findClosestPointAt (x, y) m of
    Just p -> case mode of
      _ -> { m | mode <- Selected p }
    Nothing ->  case mode of
      Selected _ -> { m | mode <- DefaultMode }
      _ -> newFreePoint { x = x, y = y } { m | mode <- mode }

-- * Helpers

isHighlighted : Model -> ID -> Bool
isHighlighted m p = isSelected m p || isHovered m p

isSelected : Model -> ID -> Bool
isSelected m p = case m.mode of
  Selected q -> if q == p then True else False
  Dragging q _ _ -> if q == p then True else False
  _ -> False

isHovered : Model -> ID -> Bool
isHovered m p = Set.member p m.hovered

findObjectIDsAt : Vector -> Model -> Set.Set ID
findObjectIDsAt v = Set.fromList << List.map (\obj -> obj.id) << findObjectsAt v

findObjectsAt : Vector -> Model -> List GeoObject
findObjectsAt pos m = List.filter (isObjectAt pos) <| Dict.values m.objects

findOneDimObjectIDsAt : Vector -> Model -> Set.Set ID
findOneDimObjectIDsAt v = Set.fromList << List.map (\obj -> obj.id) << List.filter isOneDim << findObjectsAt v

isOneDim : GeoObject -> Bool
isOneDim geo = case geo.object of
  Straight _ -> True
  Circle _ -> True
  _ -> False

isObjectAt : Vector -> GeoObject -> Bool
isObjectAt p obj = case obj.object of
  FreePoint q -> closeEnough p q
  Straight { desc } -> isOnLine p desc
  Circle { desc } -> isOnCircle p desc
  _ -> False

findClosestPointAt : (Float, Float) -> Model -> Maybe ID
findClosestPointAt (x,y) m =
  let
    step val cur v = let d = dist v {x = x, y = y} in case cur of
      Just (curP, curMin) -> if d < curMin then Just (val.id, d) else cur
      Nothing -> if d < threshold then Just (val.id, d) else cur
    incMin = \_ val cur -> case val.object of
      FreePoint v -> step val cur v
      Intersect { point } -> case point of
        Just v -> step val cur v
        Nothing -> cur
      _ -> cur
    dist p1 p2 = (p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2
    threshold = 100
  in Maybe.map fst <| Dict.foldr incMin Nothing m.objects

transformMouseCoords : (Int, Int) -> (Int, Int) -> (Float, Float)
transformMouseCoords (w, h) (x, y) = (toFloat <| x - w // 2, toFloat <| h // 2 - y)

emptyModel : Model
emptyModel =
  { objects = Dict.empty
  , newID = 0
  , dimensions = (1200, 600)
  , mode = DefaultMode
  , mouseDown = False
  , mouseDragged = False
  , mousePosition = { x = 0, y = 0 }
  , hovered = Set.empty
  }

