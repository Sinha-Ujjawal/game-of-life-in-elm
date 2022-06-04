module Game exposing (..)

import Set as S


type alias Cell =
    { x : Int, y : Int }


cellFromTuple : ( Int, Int ) -> Cell
cellFromTuple ( x, y ) =
    { x = x, y = y }


cellToTuple : Cell -> ( Int, Int )
cellToTuple { x, y } =
    ( x, y )


type alias BoardSize =
    { width : Int, height : Int }


cellOnBoard : BoardSize -> Cell -> Bool
cellOnBoard { width, height } { x, y } =
    (x >= 0)
        && (x < width)
        && (y >= 0)
        && (y < height)


neighborsOfACell : BoardSize -> Cell -> List Cell
neighborsOfACell boardSize { x, y } =
    let
        neighborsList =
            [ ( x - 1, y - 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x - 1, y + 1 )
            , ( x, y + 1 )
            , ( x + 1, y + 1 )
            ]
    in
    neighborsList
        |> List.map cellFromTuple
        |> List.filter (cellOnBoard boardSize)


neighborsOfCells : BoardSize -> S.Set ( Int, Int ) -> S.Set ( Int, Int )
neighborsOfCells boardSize cells =
    cells
        |> S.toList
        |> List.concatMap (neighborsOfACell boardSize << cellFromTuple)
        |> List.map cellToTuple
        |> S.fromList


liveNeighbors : BoardSize -> S.Set ( Int, Int ) -> Cell -> S.Set ( Int, Int )
liveNeighbors boardSize liveCells cell =
    cell
        |> neighborsOfACell boardSize
        |> List.filter (isLiveCell liveCells)
        |> List.map cellToTuple
        |> S.fromList


isLiveCell : S.Set ( Int, Int ) -> Cell -> Bool
isLiveCell liveCells { x, y } =
    S.member ( x, y ) liveCells


isEmptyCell : S.Set ( Int, Int ) -> Cell -> Bool
isEmptyCell liveCells cell =
    not (isLiveCell liveCells cell)


isGoingToBornOrSurvive : BoardSize -> S.Set ( Int, Int ) -> Cell -> Bool
isGoingToBornOrSurvive boardSize liveCells cell =
    let
        neighborsCount =
            S.size (liveNeighbors boardSize liveCells cell)
    in
    (isEmptyCell liveCells cell && (neighborsCount == 3))
        || (isLiveCell liveCells cell && (neighborsCount == 2 || neighborsCount == 3))


type alias GameState =
    { liveCells : S.Set ( Int, Int )
    , boardSize : BoardSize
    }


initGameState : BoardSize -> GameState
initGameState boardSize =
    { liveCells = S.empty, boardSize = boardSize }


insertLiveCell : Cell -> GameState -> GameState
insertLiveCell cell game =
    if cellOnBoard game.boardSize cell then
        { game | liveCells = S.insert (cellToTuple cell) game.liveCells }

    else
        game


insertLiveCells : List Cell -> GameState -> GameState
insertLiveCells cells game =
    List.foldl insertLiveCell game cells


nextGameState : GameState -> GameState
nextGameState game =
    let
        neighbors =
            neighborsOfCells game.boardSize game.liveCells

        bornOrSurvivedCells =
            neighbors
                |> S.filter (isGoingToBornOrSurvive game.boardSize game.liveCells << cellFromTuple)
    in
    { game | liveCells = bornOrSurvivedCells }
