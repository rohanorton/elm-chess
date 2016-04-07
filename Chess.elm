module Chess (..) where

import String
import Maybe.Extra
import Dict
import Char


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


typeList : Dict.Dict Char PType
typeList =
  Dict.fromList
    [ ( 'p', Pawn )
    , ( 'n', Knight )
    , ( 'r', Rook )
    , ( 'b', Bishop )
    , ( 'k', King )
    , ( 'q', Queen )
    ]


readPiece : Char -> Maybe Piece
readPiece char =
  let
    color =
      if Char.isUpper char then
        White
      else
        Black

    pieceType =
      Dict.get (Char.toLower char) typeList

    makePiece =
      Piece color
  in
    Maybe.map makePiece pieceType


showSquare : Square -> Char
showSquare square =
  case square of
    Nothing ->
      '.'

    Just piece ->
      showPiece piece


readSquare : Char -> Maybe Square
readSquare c =
  case c of
    '.' ->
      Just Nothing

    _ ->
      Maybe.map Just (readPiece c)


readBoard : String -> Maybe Board
readBoard str =
  let
    readRow =
      Maybe.Extra.traverse readSquare
  in
    String.lines str
      |> List.map String.toList
      |> Maybe.Extra.traverse readRow


showBoard : Board -> String
showBoard board =
  let
    showRow =
      List.map showSquare
  in
    List.map showRow board
      |> List.map String.fromList
      |> String.join "\n"


initialBoardStr : String
initialBoardStr =
  String.join
    "\n"
    [ "rnbqkbnr"
    , "pppppppp"
    , "........"
    , "........"
    , "........"
    , "........"
    , "PPPPPPPP"
    , "RNBQKBNR"
    ]
