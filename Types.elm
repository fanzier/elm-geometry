module Types where

import Basics (..)
import Debug
import Dict
import Maybe
import Set

-- * Geometric Objects

-- | Every geometric object has a unique ID.
type alias ID = Int

-- | A geometric object stores its direct and reverse dependencies and
-- the actual object information.
type alias GeoObject = { id : ID, name : String, revDeps : Set.Set ID, deps : Set.Set ID,  object : Object }

-- | The actual object type.
type Object
  = FreePoint Vector
  | Intersect { intersection : Intersection, point : Maybe.Maybe Vector }
  | Straight { first : ID, second : ID, desc : Maybe.Maybe LineDescription }
  | Circle { center : ID, edgePoint : ID, desc : Maybe.Maybe CircleDescription }

type alias CircleDescription = { center : Vector, radius : Float }

-- | The line consists of all points p satisfying
-- p * normal = offset
type alias LineDescription = { offset : Float, normal : Vector }

type alias Vector = { x : Float, y : Float }

-- | Intersection of two objects. If circles are involved, it also has to
-- store which one of the two intersections is meant.
type alias Intersection = { first : ID, second : ID, whichOne : WhichOne }

-- | Allows to distinguish between two intersection points.
type WhichOne = PositiveOne | NegativeOne

-- * The model/state of the program

-- | The whole state of the program.
type alias Model =
  { objects : Dict.Dict ID GeoObject
  , newID : ID
  , dimensions : (Int, Int)
  , mode : Mode
  , mouseDown : Bool
  , mouseDragged : Bool
  , mousePosition : Vector
  , hovered : Maybe ID
  }

-- | The mode stores which command is currently being executed.
-- The reaction to user input depends on the mode.
type Mode
  = DefaultMode
  | Selected ID
  | DrawLine0
  | DrawLine1 ID
  | DrawCircle0
  | DrawCircle1 ID
  | Intersect0
  | Intersect1 ID
  | Delete
  | Dragging ID

-- * User interaction

-- | User inputs consist of mouse clicks, moves and drags
-- or button clicks which change the mode
type UserInput = MouseEvent (Int, Int) Bool | ChangeMode Mode

-- * Utilities

fromJust : Maybe a -> a
fromJust x = case x of
  Nothing -> Debug.crash "fromJust: Nothing"
  Just a -> a

