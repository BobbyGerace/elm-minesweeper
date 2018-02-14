module State exposing (update, init)

import Array
import List
import Random
import Types exposing (..)

blankCell = { contents = Empty
            , state = Unclicked 0
            }
                            
blankField : Int -> Int -> Minefield
blankField rows cols = 
        let mineRow = Array.repeat cols blankCell
        in Array.repeat rows mineRow

defaultOpts : Options
defaultOpts = { rows = 8
              , cols = 8
              , bombs = 10
              }
                                                        
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
      let blank = blankField model.options.rows model.options.cols
      in case msg of
            CellClicked coords -> handleCellClick model coords
            MineList coords lst -> ({ model |
                                field = updateFieldWithMines coords lst blank,
                                state = Playing 0
                            }, Cmd.none)
            StopGame  -> init
            Tick _ -> case model.state of
                        Playing n -> ({ model | state = Playing (n + 1)}, Cmd.none)
                        _         -> (model, Cmd.none)
            
handleCellClick : Model -> (Int, Int) -> (Model, Cmd Msg)
handleCellClick model coords =
    let rpList = randomPairList 
                     model.options.rows 
                     model.options.cols 
                     model.options.bombs
    in case model.state of
        Playing _ -> handlePlayingClick model coords
        Ready -> (model, Random.generate (MineList coords) <| rpList coords)
        _ -> (model, Cmd.none)

handlePlayingClick : Model -> (Int, Int) -> (Model, Cmd Msg)
handlePlayingClick model coords =
    let newModel = {model | field = clickCell coords model.field }
    in (newModel |> checkWin |> checkLoss, Cmd.none)

checkWin : Model -> Model
checkWin model = 
    let isWin = Array.foldl (\row rowWin ->
                  rowWin && Array.foldl (\cell acc ->
                    acc && isWinningCell cell
                  ) True row
                ) True model.field
    in case (model.state, isWin) of
          (Playing n, True) -> { model | state = Won n }
          _                 -> model

checkLoss : Model -> Model
checkLoss model =
    let isLoss = Array.foldl (\row rowWin ->
                  rowWin || Array.foldl (\cell acc ->
                    acc || isLosingCell cell
                  ) False row
                ) False model.field
    in case (model.state, isLoss) of
          (Playing n, True) -> { model | state = Lost n }
          _                 -> model

isLosingCell : Cell -> Bool
isLosingCell cell = 
    case (cell.state, cell.contents) of
        (Clicked, Bomb) -> True
        _               -> False

isWinningCell : Cell -> Bool
isWinningCell cell = 
    case (cell.state, cell.contents) of
        (Unclicked _, Bomb) -> True
        (Clicked, Number _) -> True
        (Clicked, Empty)    -> True
        _                   -> False
    
clickCell : (Int, Int) -> Minefield -> Minefield
clickCell (r, c) field = 
    let click cell = { cell | state = Clicked }
        cell = getCell r c field
        newField = setCell r c click field
    in case Maybe.map (\c -> (c.contents, c.state)) cell of
        Just (Empty, Unclicked _) -> clickSurrounding (r, c) newField
        _                  -> newField

clickSurrounding : (Int, Int) -> Minefield -> Minefield
clickSurrounding (r, c) field = 
    let offsets = [-1, 0, 1]
        offsetPairs = crossProduct offsets offsets
                        |> List.filter (\p -> p /= (0,0))
        click (a, b) f = clickCell (r + a, c + b) f
    in List.foldl click field offsetPairs

updateFieldWithMines : (Int, Int) -> List (Int, Int) -> Minefield -> Minefield
updateFieldWithMines coords mines field = 
        addMinesToField mines field 
            |> updateNumbers
            |> clickCell coords

addMinesToField : List (Int, Int) -> Minefield -> Minefield
addMinesToField mines field =
        List.foldl setMine field mines
            
updateNumbers : Minefield -> Minefield
updateNumbers field = 
        let surr r c = getSurrounding r c field
            count r c = surr r c 
                          |> List.filter (\c -> c.contents == Bomb)
                          |> List.length
            update num cell = case cell.contents of
                                Bomb -> cell
                                _    -> if num == 0
                                        then { cell | contents = Empty }
                                        else { cell | contents = Number num }
        in mapField (\r c cell -> update (count r c) cell) field 
                             
getSurrounding : Int -> Int -> Minefield -> List Cell
getSurrounding r c field =
        let offsets = [-1, 0, 1]
            offsetPairs = crossProduct offsets offsets
                            |> List.filter (\p -> p /= (0,0))
            get (r, c) = getCell r c field
        in List.map (\(a, b) -> get (r + a, c + b)) offsetPairs
           |> listFromMaybes
                                           
crossProduct : List a -> List b -> List (a, b)
crossProduct a b = 
        let pairWith x list = List.map (\y -> (x, y)) list
        in List.map (\x -> pairWith x b) a |> List.concat
                           
areAdjacent : (Int, Int) -> (Int, Int) -> Bool
areAdjacent (r1, c1) (r2, c2) =
    let xDist = r1 - r2 |> abs
        yDist = c1 - c2 |> abs
    in xDist <= 1 && yDist <= 1

randomPairList : Int -> Int -> Int -> (Int, Int) -> Random.Generator(List (Int, Int))
randomPairList rows cols len coords = 
        let rowGen = (Random.int 0 (rows - 1))
            colGen = (Random.int 0 (cols - 1))
            inLst p lst = List.member p lst
            maybeAppend p lst = if inLst p lst || areAdjacent p coords 
                                then lst 
                                else p :: lst
            pair = Random.pair rowGen colGen
            next lst = if List.length lst < len
                       then Random.andThen (\p -> next (maybeAppend p lst)) pair
                       else Random.map (\_ -> lst) Random.bool
        in next []
           
init : (Model, Cmd Msg)
init = 
    let model = { field = blankField defaultOpts.rows defaultOpts.cols
                , options = defaultOpts
                , state = Ready
                }
    in (model, Cmd.none)

listFromMaybes : List (Maybe a) -> List a
listFromMaybes lst =
    case lst of
        [] -> []
        (Just x::xs) -> x :: listFromMaybes xs
        (Nothing::xs) -> listFromMaybes xs
