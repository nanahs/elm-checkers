module Helpers.CheckersInit exposing (..)

import List exposing (member)
import Helpers.TypeDec exposing (..)

initSquare : Int -> Int -> Square
initSquare x y =
  Square x y False (doesInitHaveChecker x y) (getInitColor x y) False

initRow : Int -> Row
initRow x =
  List.map (initSquare x)
  (List.indexedMap (\x -> (+) 0) (List.range 0 7))

initBoard : Board
initBoard =
  List.map initRow (List.indexedMap (\x -> (+) 0) (List.range 0 7))

--The game state of a new game with a score of 0 - 0
initCheckers : (Int, Int) -> Model
initCheckers score = 
  Model initBoard Black [] score cleanMessage

--determines the intial placement of checkers for a new game
doesInitHaveChecker : Int -> Int -> Bool
doesInitHaveChecker x y =
  if member y [0,1,2,5,6,7] then
    if y % 2 == 0 then
      if x % 2 == 0 then
        True
      else
        False
    else
      if x % 2 == 0 then
        False
      else
        True
  else
    False

--determines the color of the checker based upon a new game state for a given location
getInitColor : Int -> Int -> Player
getInitColor x y =
  if (  member y [0,2] && x%2 == 0)
      ||  y == 1 && x%2 == 1 then
    Black
  else if ( member y [5,7] && x%2 == 1)
      ||  y == 6 && x%2 == 0 then
    Red
  else
    Neither

cleanMessage : Message
cleanMessage =
  Message False "" ""

invalidMessage : Message
invalidMessage =
  Message True "invalid" "Invalid Move"