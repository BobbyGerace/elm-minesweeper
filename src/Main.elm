module Main exposing (main)
             
import Game.View exposing (view) 
import Game.State exposing (update, init)
import Browser exposing (element)
import Game.Types exposing (..)
import Time exposing (every)

init_ : () ->  (Model, Cmd Msg)
init_ _ = init 

main =
      element
          { init = init_
          , view = view
          , update = update
          , subscriptions = subscriptions
          }
                              
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Playing _ -> every 1000 Tick
        _         -> Sub.none
