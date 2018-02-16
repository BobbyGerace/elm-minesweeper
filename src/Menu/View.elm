module Menu.View exposing (..)

import Menu.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)

view : Options -> Html MenuMsg
view opts = 
    div [class "menu-bar"] [
         div [class "menu-item active"] [text "Beginner"],
         div [class "menu-item"] [text "Intermediate"],
         div [class "menu-item"] [text "Expert"]
    ]
