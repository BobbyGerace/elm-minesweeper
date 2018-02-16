module Game.Types exposing (..)

import Array
import Time exposing (Time)
import Menu.Types exposing (..)

type CellState = Clicked | Unclicked FlagState

type CellContents = Bomb | Number Int | Empty

type GameState = Ready | Playing Int | Won Int| Lost Int

type alias Cell = { contents : CellContents
                  , state : CellState
                  }

type FlagState = Flagged | Question | None
                                                      
type alias MineRow = Array.Array Cell
type alias Minefield = Array.Array MineRow
type alias Model = { field : Minefield
                   , options : Options
                   , state : GameState
                   }

type Msg = 
        CellClicked (Int, Int)
        | CellRightClicked (Int, Int)
        | ResetGame
        | MineList (Int, Int) (List (Int, Int))
        | Tick Time
        | MenuAction MenuMsg

                    
mineCount : Minefield -> Int
mineCount field =
      let isBomb cell = cell.contents == Bomb
          isFlag cell = cell.state == Unclicked Flagged
          rowCount row = row |> Array.filter isBomb |> Array.length
          rowCountFlags row = row |> Array.filter isFlag |> Array.length
      in Array.foldl (\row sum -> sum + (rowCount row) - (rowCountFlags row)) 0 field
                       
setCell : Int -> Int -> (Cell -> Cell) -> Minefield -> Minefield
setCell r c fn field = 
  Array.get r field
    |> Maybe.andThen (\row -> 
        Array.get c row
            |> Maybe.map (\cell -> fn cell)
            |> Maybe.map (\cell -> Array.set c cell row)
            |> Maybe.map (\row -> Array.set r row field)
    )
    |> Maybe.withDefault field 

setMine : (Int, Int) -> Minefield -> Minefield
setMine (r, c) field = 
    let setMine cell = { cell | contents = Bomb }
    in setCell r c setMine field
                                               
mapField : (Int -> Int -> Cell -> Cell) -> Minefield -> Minefield
mapField fn field =
        Array.indexedMap (\r row ->
          Array.indexedMap (\c cell ->
            fn r c cell
          ) row
        ) field

getCell : Int -> Int -> Minefield -> Maybe Cell
getCell r c field = 
        Array.get r field
            |> Maybe.andThen (Array.get c)
                                
toggleFlag : FlagState -> FlagState
toggleFlag flag = 
    case flag of
        None     -> Flagged
        Flagged  -> Question
        Question -> None
