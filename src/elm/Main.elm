module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)
import Tuple exposing (first, second, mapFirst, mapSecond)

import Helpers.TypeDec exposing (..)
import Helpers.CheckersInit exposing (..)
import Helpers.CheckerMovement exposing (..)
import Helpers.CheckerHtml exposing (..)

-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


selectionHelper : Square -> Bool -> Square
selectionHelper square val =
  { square | isSelected = val }

selectSquareWithChecker : Square -> Board -> Board
selectSquareWithChecker square board =
  List.map ( \r -> List.map (\s -> if square == s then 
                                    selectionHelper s True else
                                    selectionHelper s False )
                                    r ) board

-- MODEL
model : Model
model =
  initCheckers (0, 0)

-- UPDATE
update : Msg -> Model -> Model
update msg model =
  case msg of

    SelectChecker square ->
      case isaWinner model.board of
        Nothing ->
          { 
            model | 
              board = selectSquareWithChecker square model.board,
              message = cleanMessage
          }
        _ ->
          model
    
    MoveChecker square ->
      let
          maybePrevBoard = head model.prevBoardState
          maybeNewBoard = tryToMoveToSquare model.playerTurn square model.board
      in
        case isaWinner model.board of
          Nothing ->
            case maybeNewBoard of
              Nothing ->
                { model | message = invalidMessage }
              Just newBoard ->
                case maybePrevBoard of
                  Just prevBoard ->
                    {
                      model |
                        board = newBoard,
                        prevBoardState = model.board :: model.prevBoardState,
                        message = cleanMessage,
                        playerTurn =  case model.playerTurn of
                                        Red ->
                                          Black
                                        Black ->
                                          Red
                                        Neither ->
                                          Neither
                    }
                  Nothing ->
                    {
                      model |
                        board = newBoard,
                        message = cleanMessage,
                        prevBoardState = model.board :: model.prevBoardState,
                        playerTurn =  case model.playerTurn of
                                        Red ->
                                          Black
                                        Black ->
                                          Red
                                        Neither ->
                                          Neither
                    }
          _ ->
            model
    
    Undo ->
      let
          maybePrevBoard = head model.prevBoardState
      in
        case maybePrevBoard of
          Nothing ->
            model
          Just b ->
            case (tail model.prevBoardState) of
              Just val ->
                { 
                  model | 
                    board = b,
                    prevBoardState = val,
                    playerTurn =  case model.playerTurn of
                                        Red ->
                                          Black
                                        Black ->
                                          Red
                                        Neither ->
                                          Neither
                }
              Nothing ->
                model

    Win player ->
      if player == Black then
        initCheckers ( first model.score + 1, second model.score )
      else if player == Red then
        initCheckers ( first model.score, second model.score + 1 )
      else
        initCheckers (0, 0)


-- VIEW
view: Model -> Html Msg
view model =
  div [ class "container center" ] [
    turnHtml model,
    scoreHtml model,
    boardHtml model.board,
    messageHtml model,
    winScreenHtml model,
    undoHtml
  ]