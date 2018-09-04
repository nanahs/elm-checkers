module Helpers.CheckerMovement exposing (..)

import Helpers.TypeDec exposing (..)
import List exposing (head, concat, filter, any)

tryToMoveToSquare : Player -> Square -> Board -> Maybe Board
tryToMoveToSquare player destSquare board =
  let
      selectedSquare = head (filter ((\square -> square.isSelected)) <| concat board)
  in

    case selectedSquare of
      Just val ->

        if destSquare.checkerColor /= Neither
          || player /= val.checkerColor
          || (destSquare.x + destSquare.y)%2 == 1 then
          Nothing
        else
          if isValidMove val destSquare board then
            Just <| moveCheckerTo val destSquare board
          else if isValidJump val destSquare board then
            Just <| jumpCheckerTo val destSquare board
          else
            Nothing
      Nothing ->
        Nothing

isValidMove : Square -> Square -> Board -> Bool
isValidMove startSquare destSquare board =
  case startSquare.checkerColor of
    Neither ->
      False
    _ ->
      ( destSquare.hasChecker /= True) &&
        canMove startSquare.checkerColor startSquare destSquare board

isValidJump : Square -> Square -> Board -> Bool
isValidJump startSquare destSquare board =
  case startSquare.checkerColor of
    Neither ->
      False
    _ ->
    let
      midX = (startSquare.x + destSquare.x) // 2
      midY = (startSquare.y + destSquare.y) // 2
      midSquare = getSquareByXposYpos midX midY board
    in
      case midSquare of
        Just val ->
          ( destSquare.hasChecker /= True) && 
          ( isSquareBetween startSquare destSquare val) &&
          ( canJump startSquare.checkerColor startSquare destSquare board || 
            canJump startSquare.checkerColor startSquare destSquare board )
        Nothing ->
          False

getSquareByXposYpos : Int -> Int -> Board -> Maybe Square
getSquareByXposYpos x y board =
  head (filter (\square -> square.x == x && square.y == y) <| concat board)

--Player type determines the direction that a man is allowed to move
canMove : Player -> Square -> Square -> Board -> Bool
canMove player startSquare destSquare board =

  if startSquare.isKing
    && ( abs ( destSquare.y - startSquare.y ) ) == 1
    && ( abs ( destSquare.x - startSquare.x ) ) == 1 then
      True
  else
    case player of
      Black ->
        if ( destSquare.y - startSquare.y ) == 1
            && ( abs ( destSquare.x - startSquare.x ) ) == 1 then
              True
        else
          False
      Red ->
        if (startSquare.y - destSquare.y) == 1
            && ( abs ( startSquare.y - destSquare.y ) ) == 1 then
              True
        else
          False
      Neither ->
        False

canJump : Player -> Square -> Square -> Board -> Bool
canJump player startSquare destSquare board =

  if startSquare.isKing
    && ( abs ( destSquare.y - startSquare.y ) ) == 2
    && ( abs ( destSquare.x - startSquare.x ) ) == 2 then
      True
  else
    case player of
      Black ->
        if ( destSquare.y - startSquare.y ) == 2
        && ( abs ( destSquare.x - startSquare.x ) ) == 2 then
          True
        else
          False
      Red ->
        if (startSquare.y - destSquare.y) == 2
        && ( abs ( startSquare.y - destSquare.y ) ) == 2 then
          True
        else
          False
      Neither ->
        False

willBeKing : Square -> Square -> Bool
willBeKing startSquare destSquare =
  startSquare.isKing || ( startSquare.checkerColor == Black && destSquare.y == 7 )
                      || ( startSquare.checkerColor == Red && destSquare.y == 0 )

moveCheckerTo : Square -> Square -> Board -> Board
moveCheckerTo startSquare destSquare board =
    List.map 
    ( \row ->  
      List.map 
      ( \square -> 
        if square == startSquare then
          { square |
            checkerColor = Neither,
            isSelected = False,
            hasChecker = False,
            isKing = False
          }
        else if square == destSquare then
          { square |
            checkerColor = startSquare.checkerColor,
            isSelected = False,
            hasChecker = True,
            isKing = willBeKing startSquare destSquare
          }
        else
          square
      ) row
    ) board

jumpCheckerTo : Square -> Square -> Board -> Board
jumpCheckerTo startSquare destSquare board =
  List.map 
  ( \row ->  
    List.map 
    ( \square -> 
      if square == startSquare then
        { square |
          checkerColor = Neither,
          isSelected = False,
          hasChecker = False,
          isKing = False
        }
      else if square == destSquare then
        { square |
          checkerColor = startSquare.checkerColor,
          isSelected = False,
          hasChecker = True,
          isKing = willBeKing startSquare destSquare
        }
      --Remove the piece inbetween the start and destination
      else if isSquareBetween startSquare destSquare square then
        { square |
          checkerColor = Neither,
          isSelected = False,
          hasChecker = False,
          isKing = False
        }
      else
        square
    ) row
  ) board



  
isSquareBetween : Square -> Square -> Square -> Bool
isSquareBetween startSquare destSquare checkSquare =
  if abs ( startSquare.x - checkSquare.x ) == 1 && abs ( checkSquare.x - destSquare.x ) == 1
    && abs ( startSquare.y - checkSquare.y ) == 1 && abs ( checkSquare.y - destSquare.y ) == 1
    && areDifferentPlayers startSquare.checkerColor checkSquare.checkerColor then
      True
  else
    False

areDifferentPlayers : Player -> Player -> Bool
areDifferentPlayers player1 player2 =
  case player1 of
    Red ->
      player2 == Black
    Black ->
      player2 == Red
    Neither ->
      False

getAllPlayerCheckerSquares : Player -> Board -> List Square
getAllPlayerCheckerSquares player board =
  filter (\s -> s.hasChecker && s.checkerColor == player ) <| concat board

canSquareMove : Board -> Square -> Bool
canSquareMove board square =
  let
    tl = getSquareByXposYpos (square.x - 1) (square.y - 1) board
    tr = getSquareByXposYpos (square.x - 1) (square.y + 1) board
    bl = getSquareByXposYpos (square.x + 1) (square.y - 1) board
    br = getSquareByXposYpos (square.x + 1) (square.y + 1) board
    theSquares = tl :: tr :: bl :: br :: []
  in
    any (\s -> case s of
                  Nothing ->
                    False
                  _ ->
                    True
        ) theSquares

isaWinner : Board -> Maybe Player
isaWinner board =
  let
    redCanMove = List.any (canSquareMove board) <| getAllPlayerCheckerSquares Red board
    blackCanMove = List.any (canSquareMove board) <| getAllPlayerCheckerSquares Black board
    draw = not redCanMove && not blackCanMove
  in
    if draw then
      Just Neither
    else if not blackCanMove then
      Just Red
    else if not redCanMove then
      Just Black
    else
      Nothing