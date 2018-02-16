module Main exposing (main)
             
import Html  exposing (program)
import Game.View exposing (view) 
import Game.State exposing (update, init)
import Game.Types exposing (..)
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
