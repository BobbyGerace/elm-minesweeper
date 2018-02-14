module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)
import Array

view : Model -> Html Msg
view model = 
        div [] [
          span [] [mineCount model.field |> toString |> text],
          span [] [timer model.state],
          table [] [
            minefield model
          ]
        ]
                                                        
minefield : Model -> Html Msg
minefield model = fieldView model.field

fieldView : Minefield -> Html Msg
fieldView field = 
        let arr = Array.indexedMap rowView field
        in tbody [] (Array.toList arr)

rowView : Int -> MineRow -> Html Msg
rowView i row = 
        let arr = Array.indexedMap (cellView i) row
        in tr [] (Array.toList arr)

cellView : Int -> Int -> Cell -> Html Msg
cellView i j cell =
        let content = case cell.contents of
                        Bomb -> text "b"
                        Number n -> n |> toString |> text
                        _ -> text ""
            classes = "mine" ++ if cell.state == Clicked 
                                then " clicked" 
                                else " unclicked"
        in td [onClick (CellClicked (i,j))] [button [class classes] [content]]

timer : GameState -> Html Msg
timer state =
    case state of
        Playing n -> text <| toString n
        Won _     -> text "Won"
        Lost _     -> text "Lost"
        _         -> text "0"
