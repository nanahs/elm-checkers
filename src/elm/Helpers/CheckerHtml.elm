module Helpers.CheckerHtml exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Tuple exposing (first, second)

import Helpers.TypeDec exposing (..)
import Helpers.CheckerMovement exposing (..)


--HTML PIECES
undoHtml :  Html Msg
undoHtml =
    button [ class "undoButton", onClick (Undo) ] [ text "Undo Previous Action" ]

turnHtml : Model -> Html Msg
turnHtml model =
  h1 [ class "container" ] [
    case isaWinner model.board of
      Nothing ->
        text ((toString model.playerTurn ) ++ " Turn")
      Just player ->
        case player of
          Red -> text "Red Wins"
          Black -> text "Black Wins"
          Neither -> text "DRAW"
  ]

messageHtml : Model -> Html Msg
messageHtml model =
  if model.message.show then
    h2 [ class model.message.class ] [
      text model.message.text
    ]
  else
    h2 [] []

scoreHtml : Model -> Html Msg
scoreHtml model =
  div [ class "container"] [
    text ( "Black: " ++ toString (first model.score) ++ " vs  Red: " ++ toString (second model.score) )
  ]

boardHtml : Board -> Html Msg
boardHtml board =
  div [ class "board" ]
  ( List.map ( div [ class "row" ] << List.map squareHtml ) board )

squareHtml : Square -> Html Msg
squareHtml square =
  div 
  [ 
    class ("square x:" ++ toString(square.x) ++ " y:" ++ toString(square.y) ),
    onClick (if square.hasChecker then 
                    (SelectChecker square)
                  else (MoveChecker square))
  ] [
    div
    [ 
      class (checkerClassName square) 
    ] [ 
      div [ class (if square.isKing then "king" else "man") ] [ text "" ]
      ]
  ]

winScreenHtml : Model -> Html Msg
winScreenHtml model =
  case isaWinner model.board of
    Nothing ->
      div [] []
    Just val ->
      div [ class "scorescreen"] [
        div [] [
          button [ onClick (Win Neither) ] [ text "RESET" ], --Reset
          button [ onClick (Win val) ] [ text "REMATCH" ] --Rematch
        ]
      ]

checkerClassName : Square -> String
checkerClassName square =
  let
      checker = "checker "
      checkerColor = toString( square.checkerColor )
      selected = if square.isSelected then
        " selected"
        else
          ""
      state = if square.isKing then
        " king"
      else
        " man"
  in
    if square.hasChecker then
      checker ++ checkerColor ++ state ++ selected
    else
      ""