module Helpers.TypeDec exposing (..)

type alias Model = 
  {
    board: Board,
    playerTurn: Player,
    prevBoardState: List Board,
    score: (Int,Int),
    message : Message
  }

type Msg =
  SelectChecker Square |
  MoveChecker Square |
  Undo |
  Win Player

type alias Message =
  {
    show : Bool,
    class : String,
    text : String
  }

type Player = Black | Red | Neither

type alias Square =
  { 
    x : Int,
    y : Int,
    isSelected : Bool,
    hasChecker : Bool,
    checkerColor : Player,
    isKing : Bool
  }

type alias Row =
  List Square

type alias Board =
  List Row