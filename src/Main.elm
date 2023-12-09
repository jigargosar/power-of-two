module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes as HA exposing (attribute, class, style)
import Html.Events as HE exposing (onClick)
import List.Extra as LE
import List.Nonempty as NEL
import Maybe.Extra as ME
import Svg exposing (Svg)
import Svg.Attributes as SA


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { game : Game }


type alias Cells =
    List Cell


type alias Cell =
    TileData


type alias Tiles =
    List Tile


type alias Tile =
    TileVM


type alias ConnectionCells =
    NEL Cell


type Game
    = Start Cells
    | Connecting ConnectionTiles Cells
    | Connected Tiles


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialGame =
            Start initialCells
                -- [ 15, 14, 9, 5, 6, 11 ]
                -- [ 13, 9 ]
                |> withRollback (connectAll (NEL.map idxToGP ( 15, [ 14, 9, 5, 6, 11 ] )))
                |> withRollback completeConnection
    in
    ( { game = initialGame }, Cmd.none )


connectAll ( h, t ) g =
    connect h g
        |> Maybe.andThen
            (case NEL.fromList t of
                Just nel ->
                    connectAll nel

                Nothing ->
                    Just
            )


withRollback fn a =
    fn a |> Maybe.withDefault a


findCellAt : GP -> Cells -> Maybe Cell
findCellAt gp cells =
    LE.find (cellGP >> eq gp) cells


cellGP : Cell -> GP
cellGP =
    Tuple.first


initConnectionCells : Cell -> ConnectionCells
initConnectionCells cell =
    NEL.singleton cell


addConnectionCell : Cell -> ConnectionCells -> Maybe ConnectionCells
addConnectionCell cell connectionCells =
    Just (NEL.prependElem cell connectionCells)


completeConnection : Game -> Maybe Game
completeConnection game =
    case game of
        Connecting connectionCells cells ->
            updateTilesWithConnections (connectionCells |> NEL.toList |> List.map cellGP) cells
                |> Just
                |> Maybe.map (\tiles -> Connected tiles)

        _ ->
            Nothing


connect : GP -> Game -> Maybe Game
connect gp game =
    case game of
        Start cells ->
            findCellAt gp cells
                |> Maybe.map
                    (\cell ->
                        Connecting (initConnectionCells cell) cells
                    )

        Connecting connectionCells cells ->
            findCellAt gp cells
                |> Maybe.andThen (\cell -> addConnectionCell cell connectionCells)
                |> Maybe.map
                    (\newConnectionCells ->
                        Connecting newConnectionCells cells
                    )

        _ ->
            Nothing


type Msg
    = Msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "padding" "2rem"
        , style "gap" "1rem"
        ]
        [ globalStyles
        , text "V9 Implementing game from scratch"
        , div [ style "display" "flex", style "gap" "1rem" ]
            [ case model.game of
                Start cells ->
                    viewGrid (cells |> List.map StaticTile)

                Connecting connectionCells cells ->
                    viewGrid (cells |> List.map StaticTile)

                Connected tiles ->
                    viewGrid tiles
            , let
                tiles =
                    updateTilesWithConnections initialCGPs initialTileDataList
              in
              viewGrid tiles
            ]
        ]


initialCGPs =
    [ 15, 14, 9, 5, 6, 11 ]
        -- [ 13, 9 ]
        |> List.map idxToGP
        |> List.reverse


initialCells =
    initialTileDataList


initialTileDataList =
    List.range 1 16
        |> List.map (\i -> ( idxToGP i, i ))


idxToGP i =
    ( modBy 4 (i - 1), (i - 1) // 4 )


type alias GP =
    ( Int, Int )


type alias Val =
    Int


type alias TileData =
    ( GP, Val )


type TileVM
    = StaticTile TileData
    | MergedTile TileData Int ConnectionTiles
    | DroppedTile TileData { dy : Int, dropTileDelay : Int }


tileVMGP tvm =
    case tvm of
        StaticTile ( gp, _ ) ->
            gp

        MergedTile ( gp, _ ) _ _ ->
            gp

        DroppedTile ( gp, _ ) _ ->
            gp


type alias ConnectionTiles =
    NEL TileData


countHolesBelow : GP -> ConnectionTiles -> Int
countHolesBelow ( x, y ) ( _, prevConnectionTiles ) =
    LE.count (\( ( hx, hy ), _ ) -> x == hx && y < hy) prevConnectionTiles


lastConnectionTileGP : ConnectionTiles -> GP
lastConnectionTileGP ( ( gp, _ ), _ ) =
    gp


partitionConnectedTiles : List GP -> List TileData -> Maybe ( ConnectionTiles, List TileData )
partitionConnectedTiles connectionGPs tiles =
    let
        maybeConnectionTiles : Maybe ConnectionTiles
        maybeConnectionTiles =
            connectionGPs
                |> List.map (\gp -> tiles |> LE.find (Tuple.first >> eq gp))
                |> ME.combine
                |> Maybe.andThen LE.uncons
    in
    maybeConnectionTiles
        |> Maybe.map
            (\connectionTiles ->
                ( connectionTiles
                , tiles |> reject (Tuple.first >> memberOf connectionGPs)
                )
            )


createNewDroppedTileVMs dropTileDelay emptyGPs =
    let
        maxYOfEmptyGPs =
            emptyGPs
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0
    in
    emptyGPs
        |> List.map
            (\gp ->
                DroppedTile ( gp, -99 ) { dy = maxYOfEmptyGPs + 1, dropTileDelay = dropTileDelay }
            )


updateTilesWithConnections : List GP -> List TileData -> List TileVM
updateTilesWithConnections connectionGPs tiles =
    case partitionConnectedTiles connectionGPs tiles of
        Nothing ->
            []

        Just ( connectionTiles, notConnectedTiles ) ->
            let
                dropTileDelay =
                    NEL.length connectionTiles

                droppedAndStaticTileVMs =
                    notConnectedTiles
                        |> List.map
                            (\( ( x, y ), v ) ->
                                let
                                    ct =
                                        countHolesBelow ( x, y ) connectionTiles
                                in
                                if ct > 0 then
                                    DroppedTile ( ( x, y + ct ), v ) { dy = ct, dropTileDelay = dropTileDelay }

                                else
                                    StaticTile ( ( x, y ), v )
                            )

                mergedTileVM =
                    let
                        ( x, y ) =
                            lastConnectionTileGP connectionTiles

                        ct =
                            countHolesBelow ( x, y ) connectionTiles
                    in
                    MergedTile ( ( x, y + ct ), 99 ) ct connectionTiles

                tileVMExistsAt gp =
                    List.any (tileVMGP >> eq gp) (mergedTileVM :: droppedAndStaticTileVMs)

                emptyGPs =
                    tiles
                        |> List.map Tuple.first
                        |> reject tileVMExistsAt
            in
            mergedTileVM
                :: droppedAndStaticTileVMs
                ++ createNewDroppedTileVMs dropTileDelay emptyGPs
                ++ []


viewGrid tiles =
    div
        [ style "display" "inline-block"
        , style "align-self" "start"
        , style "overflow" "hidden"
        ]
        [ div
            [ style "background-color" "#333"
            , style "border-radius" "0.5rem"
            , style "position" "relative"
            ]
            [ text ""
            , div
                [ style "" ""
                , style "display" "grid"
                , style "grid-template" "repeat(4, 100px)/ repeat(4,100px)"
                , style "padding" "0.5rem"
                , style "gap" "0.5rem"
                , style "font-size" "2rem"
                ]
                (tiles |> List.map viewTile)
            ]
        ]


viewTile tile =
    case tile of
        StaticTile ( gp, val ) ->
            div
                [ gridAreaFromGP gp
                , style "display" "grid"
                , style "background-color" "#111"
                , style "place-content" "center"
                , style "border-radius" "0.5rem"
                ]
                [ text (String.fromInt val)
                ]

        MergedTile td mdy reverseCollapseTDs ->
            let
                viewCollapseTiles len i ( ( gp, val ), ( dx, dy ) ) =
                    div
                        [ let
                            slideDuration =
                                "calc(var(--unit-time))"

                            slideDelay =
                                "calc($slideDuration * $idx)"
                                    |> String.replace "$slideDuration" slideDuration
                                    |> String.replace "$idx" (String.fromInt i)
                          in
                          replaceStyles
                            [ "--diff-x:" ++ String.fromInt dx
                            , "--diff-y:" ++ String.fromInt dy
                            , "--duration:" ++ slideDuration
                            , "--delay:" ++ slideDelay
                            ]
                        , style "animation" "var(--duration) ease-out var(--delay) 1 normal both running slide-to-diff-and-vanish"
                        , gridAreaFromGP gp
                        , style "display" "grid"
                        , style "background-color" "#111"
                        , style "place-content" "center"
                        , style "border-radius" "0.5rem"
                        ]
                        [ text (String.fromInt val)
                        , div [ style "font-size" "0.5rem" ] [ text ("cidx = " ++ String.fromInt i) ]
                        ]

                viewNewMergedTile ( gp, val ) =
                    div
                        [ replaceStyles
                            [ "--diff-x:" ++ String.fromInt 0
                            , "--diff-y:" ++ String.fromInt mdy
                            , "--drop-tile-delay: calc($len * var(--unit-time))"
                                |> String.replace "$len" (String.fromInt (NEL.length reverseCollapseTDs))
                            , "--merge-appear-delay: calc(var(--drop-tile-delay) - var(--unit-time))"
                            ]
                        , style "animation"
                            ([ "var(--unit-time) ease-out var(--merge-appear-delay) 1 normal both running merged-appear"
                             , "calc(var(--unit-time) * var(--diff-y)) ease-out var(--drop-tile-delay) 1 normal both running slide-from-diff"
                             ]
                                |> String.join ","
                            )
                        , gridAreaFromGP gp
                        , style "display" "grid"
                        , style "background-color" "#111"
                        , style "place-content" "center"
                        , style "border-radius" "0.5rem"
                        ]
                        [ text (String.fromInt val)
                        , div [ style "font-size" "0.5rem" ] [ text ("merged dy = " ++ String.fromInt mdy) ]
                        ]
            in
            div [ style "display" "contents" ]
                ([]
                    ++ (let
                            collapseTDs =
                                List.reverse (NEL.toList reverseCollapseTDs)

                            collapseTDWithDiffs =
                                List.map2
                                    (\( gp, v ) ( ngp, _ ) ->
                                        ( ( gp, v ), tmap2 sub ngp gp )
                                    )
                                    collapseTDs
                                    (List.drop 1 collapseTDs ++ (LE.last collapseTDs |> Maybe.map List.singleton |> Maybe.withDefault []))
                        in
                        List.indexedMap (viewCollapseTiles (-1 + List.length collapseTDWithDiffs)) collapseTDWithDiffs
                       )
                    ++ [ viewNewMergedTile td ]
                )

        DroppedTile ( gp, val ) { dy, dropTileDelay } ->
            div
                [ replaceStyles
                    [ "--diff-x:" ++ String.fromInt 0
                    , "--diff-y:" ++ String.fromInt dy
                    , "--drop-tile-delay: calc($len * var(--unit-time))"
                        |> String.replace "$len" (String.fromInt dropTileDelay)
                    ]
                , style "animation" "calc(var(--unit-time) * var(--diff-y)) ease-out var(--drop-tile-delay) 1 normal both running slide-from-diff"
                , gridAreaFromGP gp
                , style "display" "grid"
                , style "background-color" "#111"
                , style "place-content" "center"

                -- , style "translate" ("0 " ++ String.fromInt (dy * -110) ++ "%")
                , style "border-radius" "0.5rem"
                ]
                [ text (String.fromInt val)
                , div [ style "font-size" "0.5rem" ] [ text ("drop dy = " ++ String.fromInt dy) ]
                ]



-- UTILS


flip fn a b =
    fn b a


memberOf =
    flip List.member


reject =
    LE.filterNot


type alias NEL a =
    NEL.ListNonempty a


eq =
    (==)


tmap2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


sub =
    (-)


ms f =
    String.fromFloat f ++ "ms"


replaceStyles styles =
    styles |> String.join ";" |> attribute "style"


gridAreaFromGP ( x, y ) =
    style "grid-area" (String.fromInt (y + 1) ++ "/" ++ String.fromInt (x + 1))


padding =
    style "padding"


globalStyles =
    Html.node "style" [] [ text """

:root{
    height:100%;
    font-family:Arial, sans-serif;
    font-size:20px;
    background:#111;
    color:#eee;

    --unit-time: 1000ms;
    --unit-time: 300ms;

}

* { box-sizing:border-box; }

body{ margin:0; height:100%; }

@keyframes slide-from-diff { 
    from{
        translate: calc( -1 * (100% + 0.5rem) * var(--diff-x,0))
                   calc( -1 * (100% + 0.5rem) * var(--diff-y,0)) ;
    }
    to{
    }
}

@keyframes merged-appear{
    from{scale:0;}
    50%{scale: 1.2;}
    to{scale:1;}
}

@keyframes slide-to-diff-and-vanish { 
    from{
    }
    to{
        translate: calc( (100% + 0.5rem) * var(--diff-x,0))
                   calc( (100% + 0.5rem) * var(--diff-y,0)) ;

        opacity:0;
        _visibility:hidden;
        scale:1;
    }
}


    """ ]
