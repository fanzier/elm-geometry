module Draw where

import Basics (..)
import Color (..)
import Dict
import Graphics.Element (..)
import Graphics.Collage (..)
import List

import Math (..)
import Model (..)
import Types (..)

-- * Display the model graphically

view : Model -> Element
view model = let (w,h) = model.dimensions in collage w h <|
  (List.map (drawGeoObject model) (Dict.values model.objects))

drawGeoObject : Model -> GeoObject -> Form
drawGeoObject m geo = case geo.object of
  FreePoint v -> drawFreePoint (isHighlighted m geo.id) v
  Intersect { point } -> case point of
    Just point -> drawIntersectPoint (isHighlighted m geo.id) point
    Nothing -> emptyForm
  Straight { desc } -> case desc of
    Just d -> drawLine (isHighlighted m geo.id) m.dimensions d
    Nothing -> emptyForm
  Circle { desc } -> case desc of
    Just d -> drawCircle (isHighlighted m geo.id) d
    Nothing -> emptyForm

drawCircle : Bool -> CircleDescription -> Form
drawCircle highlighted { center, radius } = move (center.x, center.y) <| 
  outlined (let style = solid green in { style | width <- if highlighted then 4 else 2 }) <|
    circle radius

drawLine : Bool -> (Int, Int) -> LineDescription -> Form
drawLine highlighted (width, height) desc =
  let (start, end) = findRemotePointsOnLine (toFloat <| max width height) desc
  in traced (let style = solid blue in { style | width <- if highlighted then 4 else 2 }) <|
    segment (start.x, start.y) (end.x, end.y)  

drawFreePoint : Bool -> Vector -> Form
drawFreePoint highlighted p = let point = filled red <| circle (if highlighted then 6 else 4)
  in move (p.x, p.y) point

drawIntersectPoint : Bool -> Vector -> Form
drawIntersectPoint highlighted p = let point = filled black <| circle (if highlighted then 6 else 4)
  in move (p.x, p.y) point

emptyForm : Form
emptyForm = group []
  
