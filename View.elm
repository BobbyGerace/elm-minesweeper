module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Types exposing (..)
import Array
import Json.Decode as D
import Char

view : Model -> Html Msg
view model = 
    table [class "game-container"] [
        tbody [] [
            tr [class "game-header"] [
                td [class "bomb-count-cell"] [mineDisplay model],
                td [class "face-button-cell"] [
                  button [onClick ResetGame, class "face-button"] [buttonFace model.state]
                ],
                td [class "timer-cell"] [timer model.state]
            ],
            tr [] [
                td [class "field-container-cell", colspan 3] [
                    minefield model
                ]
            ]
        ]
    ]
                                                        
minefield : Model -> Html Msg
minefield model = 
    table [class "field-table"] [
        fieldView model.state model.field
    ]

fieldView : GameState -> Minefield -> Html Msg
fieldView state field = 
        let arr = Array.indexedMap (rowView state) field
        in tbody [] (Array.toList arr)

rowView : GameState -> Int -> MineRow -> Html Msg
rowView state i row = 
        let arr = Array.indexedMap (cellView state i) row
        in tr [class "field-row"] (Array.toList arr)

cellView : GameState -> Int -> Int -> Cell -> Html Msg
cellView state i j cell =
        let getContent = case state of
                            Won _ -> wonContents
                            Lost _ -> lostContents
                            _ -> cellContents
            content = getContent i j cell
            classes = case (cell.contents, cell.state) of
                          (Bomb, Clicked)   -> "field-cell clicked clicked-bomb"
                          (_ , Clicked)     -> "field-cell clicked" 
                          _                 -> "field-cell"
        in td [class classes] [content]

lostContents : Int -> Int -> Cell -> Html Msg
lostContents i j cell = 
    case (cell.state, cell.contents) of
        (Unclicked Flagged, Empty) -> text "❌"
        (Unclicked Flagged, Number _) -> text "❌"
        _ -> cellContents i j cell

wonContents : Int -> Int -> Cell -> Html Msg
wonContents i j cell = 
    case (cell.state, cell.contents) of
        (Unclicked _, Bomb) -> cellContents i j { cell | state = Unclicked Flagged }
        _ -> cellContents i j cell


cellContents : Int -> Int -> Cell -> Html Msg
cellContents i j cell = 
    case (cell.state, cell.contents) of
        (Unclicked flag, _) -> button 
                                [ 
                                  class "field-button",
                                  onClick (CellClicked (i,j)), 
                                  onRightClick (i, j)
                                ] 
                                [flagToStr flag |> text]
        (Clicked, Bomb)     -> text "💣" 
        (Clicked, Number n) -> span [class ("number-cell " ++ numClass n)] [n |> toString |> text]
        _                   -> text ""

numClass : Int -> String
numClass n =
    case n of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        _ -> ""

flagToStr : FlagState -> String
flagToStr f =
    case f of
        Flagged  -> "🚩" 
        Question -> "❔" 
        None     -> Char.fromCode 160 |> String.fromChar

buttonFace : GameState -> Html Msg
buttonFace state = 
    case state of
        --Won _  -> text "😯"
        Won _  -> text "😎"
        Lost _ -> text "💀"
        _      -> text "🙂"

timer : GameState -> Html Msg
timer state =
    let num = case state of
                Playing n -> toString n
                Won n     -> toString n
                Lost n    -> toString n
                _         -> "0"
    in num |> leftPad '0' 3 |> text

mineDisplay : Model -> Html Msg
mineDisplay model =
    let num = case model.state of
                Ready -> model.options.bombs |> toString
                _     -> mineCount model.field |> toString
    in num |> leftPad '0' 3 |> text

onRightClick : (Int, Int) -> Attribute Msg
onRightClick coords = 
    let options = { preventDefault = True
                  , stopPropagation = True
                  }
        decoder = D.maybe D.bool |> D.map (\_ -> CellRightClicked coords)
    in onWithOptions "contextmenu" options decoder

leftPad : Char -> Int -> String -> String
leftPad ch n str = 
    if String.length str < n
    then leftPad ch n <| String.cons ch str
    else str
