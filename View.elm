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
        fieldView model.field
    ]

fieldView : Minefield -> Html Msg
fieldView field = 
        let arr = Array.indexedMap rowView field
        in tbody [] (Array.toList arr)

rowView : Int -> MineRow -> Html Msg
rowView i row = 
        let arr = Array.indexedMap (cellView i) row
        in tr [class "field-row"] (Array.toList arr)

cellView : Int -> Int -> Cell -> Html Msg
cellView i j cell =
        let content = cellContents i j cell
            classes = case (cell.contents, cell.state) of
                          (Bomb, Clicked)   -> "field-cell clicked clicked-bomb"
                          (_ , Clicked)     -> "field-cell clicked" 
                          _                 -> "field-cell"
        in td [class classes] [content]


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
    case state of
        Playing n -> text <| toString n
        Won n     -> text <| toString n
        Lost n    -> text <| toString n
        _         -> text "0"

mineDisplay : Model -> Html Msg
mineDisplay model =
    case model.state of
        Ready -> model.options.bombs |> toString |> text
        _     -> mineCount model.field |> toString |> text

onRightClick : (Int, Int) -> Attribute Msg
onRightClick coords = 
    let options = { preventDefault = True
                  , stopPropagation = True
                  }
        decoder = D.maybe D.bool |> D.map (\_ -> CellRightClicked coords)
    in onWithOptions "contextmenu" options decoder
