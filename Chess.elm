module Chess (..) where

import String exposing (lines, join, toList, fromList)


type alias Board =
  List (List Square)


type alias Square =
  Maybe Piece


type Piece
  = Piece PColor PType


type PColor
  = White
  | Black


type PType
  = Queen
  | King
  | Bishop
  | Rook
  | Knight
  | Pawn


showPiece : Piece -> Char
showPiece piece =
  case piece of
    Piece White Queen ->
      'Q'

    Piece White King ->
      'K'

    Piece White Bishop ->
      'B'

    Piece White Rook ->
      'R'

    Piece White Knight ->
      'N'

    Piece White Pawn ->
      'P'

    Piece Black Queen ->
      'q'

    Piece Black King ->
      'k'

    Piece Black Bishop ->
      'b'

    Piece Black Rook ->
      'r'

    Piece Black Knight ->
      'n'

    Piece Black Pawn ->
      'p'


readPiece : Char -> Maybe Piece
readPiece char =
  case char of
    'Q' ->
      Just (Piece White Queen)

    'K' ->
      Just (Piece White King)

    'B' ->
      Just (Piece White Bishop)

    'R' ->
      Just (Piece White Rook)

    'N' ->
      Just (Piece White Knight)

    'P' ->
      Just (Piece White Pawn)

    'q' ->
      Just (Piece Black Queen)

    'k' ->
      Just (Piece Black King)

    'b' ->
      Just (Piece Black Bishop)

    'r' ->
      Just (Piece Black Rook)

    'n' ->
      Just (Piece Black Knight)

    'p' ->
      Just (Piece Black Pawn)

    _ ->
      Nothing


showSquare : Square -> Char
showSquare square =
  case square of
    Nothing ->
      ' '

    Just piece ->
      showPiece piece


readSquare : Char -> Square
readSquare =
  readPiece


readBoard : String -> Board
readBoard str =
  let
    readRow =
      List.map readSquare
  in
    lines str
      |> List.map toList
      |> List.map readRow


showBoard : Board -> String
showBoard board =
  let
    showRow =
      List.map showSquare
  in
    List.map showRow board
      |> List.map fromList
      |> join "\n"


initialBoardStr : String
initialBoardStr =
  join
    "\n"
    [ "rnbqkbnr"
    , "pppppppp"
    , "        "
    , "        "
    , "        "
    , "        "
    , "PPPPPPPP"
    , "RNBQKBNR"
    ]
