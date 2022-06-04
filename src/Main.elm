module Main exposing (main)

import Browser
import Game
import Html exposing (Html)
import Random
import Set as S
import Time



-- Model


type alias Model =
    Game.GameState


initModel : () -> ( Model, Cmd Msg )
initModel _ =
    ( Game.initGameState { width = 20, height = 20 }, Cmd.none )



-- Update


type Msg
    = NextState
    | InsertRandomPoints Int
    | InsertCells (List Game.Cell)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextState ->
            ( Game.nextGameState model, Cmd.none )

        InsertRandomPoints many ->
            ( model, randomSpawns model.boardSize many )

        InsertCells cells ->
            ( Game.insertLiveCells cells model, Cmd.none )


randomSpawns : Game.BoardSize -> Int -> Cmd Msg
randomSpawns { width, height } many =
    let
        xPosGen =
            Random.uniform 0 (List.range 1 (width - 1))

        yPosGen =
            Random.uniform 0 (List.range 1 (height - 1))

        generator =
            Random.pair xPosGen yPosGen
    in
    Random.generate (InsertCells << List.map Game.cellFromTuple) (Random.list many generator)



-- View


renderCell : S.Set ( Int, Int ) -> Int -> Int -> Html Msg
renderCell liveCells row col =
    if Game.isLiveCell liveCells { x = col, y = row } then
        Html.text "# "

    else
        Html.text "_ "


renderRow : Game.BoardSize -> S.Set ( Int, Int ) -> Int -> Html Msg
renderRow boardSize liveCells row =
    Html.div []
        (List.range 0 (boardSize.width - 1)
            |> List.map (renderCell liveCells row)
        )


renderGrid : Game.BoardSize -> S.Set ( Int, Int ) -> List (Html Msg)
renderGrid boardSize liveCells =
    List.range 0 (boardSize.height - 1)
        |> List.map (renderRow boardSize liveCells)


view : Model -> Html Msg
view { boardSize, liveCells } =
    Html.div [] (renderGrid boardSize liveCells)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 (\_ -> NextState)
        , Time.every 100 (\_ -> InsertRandomPoints 100)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
