module Menu.View exposing (..)

import Menu.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)

view : Options -> Html Msg
view opts = 
    let classes optType = class ("menu-item" ++ maybeActive optType)
        maybeActive optType = if optType == opts.optionType then " active" else ""
    in div [class "menu-bar"] [
         div [onClick BeginnerClick, classes Beginner] [text "Beginner"],
         div [onClick IntermediateClick, classes Intermediate] [text "Intermediate"],
         div [onClick ExpertClick, classes Expert] [text "Expert"]
    ]
