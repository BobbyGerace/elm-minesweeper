module Menu.Types exposing (..)

type alias Options = { rows : Int
                     , cols : Int
                     , bombs : Int
                     }

type MenuMsg = 
    BeginnerClick
    | IntermediateClick
    | ExpertClick
