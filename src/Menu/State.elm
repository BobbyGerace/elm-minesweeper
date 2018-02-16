module Menu.State exposing (update, beginnerOpts, intermediateOpts, expertOpts)

import Menu.Types exposing(..)

update : Msg -> Options -> (Options, Cmd Msg)
update msg opts =
    case msg of 
        BeginnerClick     -> (beginnerOpts, Cmd.none)
        IntermediateClick -> (intermediateOpts, Cmd.none)
        ExpertClick       -> (expertOpts, Cmd.none)

beginnerOpts : Options
beginnerOpts = { rows = 8
               , cols = 8
               , bombs = 10
               , optionType = Beginner
               }

intermediateOpts : Options
intermediateOpts = { rows = 16
                   , cols = 16
                   , bombs = 40
                   , optionType = Intermediate
                   }

expertOpts : Options
expertOpts = { rows = 16
             , cols = 31
             , bombs = 99
             , optionType = Expert
             }
