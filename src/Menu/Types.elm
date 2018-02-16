module Menu.Types exposing (..)

type alias Options = { rows : Int
                     , cols : Int
                     , bombs : Int
                     , optionType : OptionType
                     }

type OptionType = Beginner | Intermediate | Expert

type Msg = 
    BeginnerClick
    | IntermediateClick
    | ExpertClick
