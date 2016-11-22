module Chess exposing (..)

import Dict
import Char
import Html exposing (Html, table, tbody, tr, td, span, text)
import Html.Attributes as Attr


main : Html a
main =
    toHtml startingBoard



-- Board Types


type alias Board =
    List (List Square)


type alias Square =
    Maybe Piece


type Piece
    = Piece Colour Figure


type Colour
    = White
    | Black


type Figure
    = Queen
    | King
    | Bishop
    | Rook
    | Knight
    | Pawn



-- Read Board


figureList : Dict.Dict Char Figure
figureList =
    Dict.fromList
        [ ( 'p', Pawn )
        , ( 'n', Knight )
        , ( 'r', Rook )
        , ( 'b', Bishop )
        , ( 'k', King )
        , ( 'q', Queen )
        ]


readSquare : Char -> Square
readSquare char =
    let
        colour =
            if (Char.isUpper char) then
                Black
            else
                White
    in
        figureList
            |> Dict.get (Char.toLower char)
            |> Maybe.map (Piece colour)


readRow : String -> List Square
readRow =
    List.map readSquare << String.toList


readBoard : String -> Board
readBoard =
    List.map readRow << String.split "\n"


startingBoardString : String
startingBoardString =
    String.join "\n"
        [ "rnbqkbnr"
        , "pppppppp"
        , "........"
        , "........"
        , "........"
        , "........"
        , "PPPPPPPP"
        , "RNBQKBNR"
        ]


startingBoard : Board
startingBoard =
    readBoard startingBoardString



-- View


toHtml : Board -> Html a
toHtml board =
    let
        children =
            board
                |> List.indexedMap rowView
    in
        table
            [ Attr.style
                [ "text-align" => "center"
                , "border-spacing" => "0pt"
                , "border-collapse" => "collapse"
                , "border-color" => "silver"
                , "border-style" => "solid"
                , "border-width" => "0pt 0pt 0pt 0pt"
                ]
            ]
            [ tbody [] children
            ]


rowView : Int -> List Square -> Html a
rowView x squares =
    let
        children =
            squares
                |> List.indexedMap (squareView x)
    in
        tr
            [ Attr.style
                [ "vertical-align" => "bottom"
                ]
            ]
            children


squareView : Int -> Int -> Square -> Html a
squareView x y square =
    td
        [ Attr.style
            [ "width" => "40pt"
            , "height" => "40pt"
            , "background-color" => colourByPosition ( x, y )
            ]
        ]
        [ pieceView square ]


pieceView : Square -> Html a
pieceView square =
    let
        maybeString =
            square
                |> Maybe.map pieceToUnicode
    in
        case maybeString of
            Nothing ->
                text ""

            Just piece ->
                span [ Attr.style [ "font-size" => "250%" ] ]
                    [ text piece
                    ]


pieceToUnicode : Piece -> String
pieceToUnicode piece =
    case piece of
        Piece Black King ->
            "♔"

        Piece White King ->
            "♚"

        Piece Black Queen ->
            "♕"

        Piece White Queen ->
            "♛"

        Piece Black Rook ->
            "♖ "

        Piece White Rook ->
            "♜"

        Piece Black Bishop ->
            "♗"

        Piece White Bishop ->
            "♝"

        Piece Black Knight ->
            "♘"

        Piece White Knight ->
            "♞"

        Piece Black Pawn ->
            "♙"

        Piece White Pawn ->
            "♟"


colourByPosition : ( Int, Int ) -> String
colourByPosition ( x, y ) =
    let
        silver =
            x + y |> isOdd
    in
        if silver then
            "silver"
        else
            "white"



-- Utilities


(=>) : String -> String -> ( String, String )
(=>) =
    (,)


isOdd : Int -> Bool
isOdd x =
    x % 2 /= 0
