module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Types exposing (..)
import Array
import Json.Decode as D

view : Model -> Html Msg
view model = 
        div [] [
          span [] [mineDisplay model],
          span [] [
              button [onClick ResetGame] [buttonFace model.state]
          ],
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
            classes = "mine" ++ case cell.state of
                                  Clicked   -> " clicked"
                                  Unclicked Flagged -> " flagged"
                                  Unclicked Question -> " question"
                                  Unclicked None     -> " unclicked"
                                     
        in td [onClick (CellClicked (i,j)), onRightClick (i, j)] [button [class classes] [content]]

buttonFace : GameState -> Html Msg
buttonFace state = 
    case state of
        Won _  -> text "B-)"
        Lost _ -> text ":-("
        _      -> text ":-)" 

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
