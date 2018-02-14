module Main exposing (main)

import Html exposing (program)
import View exposing (view)
import State exposing (update, init)
import Types exposing (..)
import Time exposing (every, second)

main =
      program
          { init = init
          , view = view
          , update = update
          , subscriptions = subscriptions
          }
                              
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Playing _ -> every second Tick
        _         -> Sub.none
