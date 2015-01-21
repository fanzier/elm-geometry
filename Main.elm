module Main where

import Graphics.Element (..)
import Mouse
import Signal (..)

import Draw (..)
import Types (..)
import Model (..)

main : Signal Element
main =  view <~ foldp update emptyModel (mergeMany 
                  [ MouseEvent <~ Mouse.position ~ Mouse.isDown
                  , ChangeMode <~ modeSignal
                  ])

port lineButtonClick : Signal ()
port circleButtonClick : Signal ()
port deleteButtonClick : Signal ()
port intersectButtonClick : Signal ()

modeSignal : Signal Mode
modeSignal = mergeMany
  [ constant DefaultMode
  , map (always DrawLine0) lineButtonClick
  , map (always DrawCircle0) circleButtonClick
  , map (always Delete) deleteButtonClick
  , map (always Intersect0) intersectButtonClick
  ]

