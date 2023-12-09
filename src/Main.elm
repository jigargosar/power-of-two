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


type alias GP =
    ( Int, Int )


type alias Val =
    Int


type alias Cell =
    ( GP, Val )


type alias Cells =
    List Cell


cellGP : Cell -> GP
cellGP =
    Tuple.first


findCellAt : GP -> Cells -> Maybe Cell
findCellAt gp cells =
    LE.find (cellGP >> eq gp) cells


type alias ConnectionCells =
    NEL Cell


initConnectionCells : Cell -> ConnectionCells
initConnectionCells cell =
    NEL.singleton cell


addConnectionCell : Cell -> ConnectionCells -> Maybe ConnectionCells
addConnectionCell cell connectionCells =
    Just (NEL.prependElem cell connectionCells)


type Tile
    = StaticTile Cell
    | MergedTile Cell Int ConnectionCells
    | DroppedTile Cell { dy : Int, dropTileDelay : Int }


type alias Tiles =
    List Tile


tileGP tile =
    case tile of
        StaticTile ( gp, _ ) ->
            gp

        MergedTile ( gp, _ ) _ _ ->
            gp

        DroppedTile ( gp, _ ) _ ->
            gp


type Game
    = Start Cells
    | Connecting ConnectionCells Cells
    | Connected Tiles


connect : GP -> Game -> Maybe Game
connect gp game =
    case game of
        Start cells ->
            findCellAt gp cells
                |> Maybe.map
                    (\cell ->
                        Connecting (initConnectionCells cell) (LE.remove cell cells)
                    )

        Connecting connectionCells cells ->
            findCellAt gp cells
                |> Maybe.andThen
                    (\cell ->
                        addConnectionCell cell connectionCells
                            |> Maybe.map
                                (\newConnectionCells ->
                                    Connecting newConnectionCells (LE.remove cell cells)
                                )
                    )

        _ ->
            Nothing


completeConnection : Game -> Maybe Game
completeConnection game =
    case game of
        Connecting connectionCells cells ->
            Just (Connected (completeConnectionHelp connectionCells cells))

        _ ->
            Nothing


completeConnectionHelp : ConnectionCells -> Cells -> Tiles
completeConnectionHelp connectionTiles notConnectedTiles =
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
            List.any (tileGP >> eq gp) (mergedTileVM :: droppedAndStaticTileVMs)

        emptyGPs =
            NEL.toList connectionTiles
                ++ notConnectedTiles
                |> List.map cellGP
                |> reject tileVMExistsAt
    in
    mergedTileVM
        :: droppedAndStaticTileVMs
        ++ createNewDroppedTileVMs dropTileDelay emptyGPs
        ++ []


countHolesBelow : GP -> ConnectionCells -> Int
countHolesBelow ( x, y ) ( _, prevConnectionTiles ) =
    LE.count (\( ( hx, hy ), _ ) -> x == hx && y < hy) prevConnectionTiles


lastConnectionTileGP : ConnectionCells -> GP
lastConnectionTileGP ( ( gp, _ ), _ ) =
    gp


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


type alias Model =
    { game : Game }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialGame =
            Start mockCells
                -- [ 15, 14, 9, 5, 6, 11 ]
                -- [ 13, 9 ]
                |> withRollback (connectAll (NEL.map idxToGP ( 15, [ 14, 9, 5, 6, 11 ] )))
                |> withRollback completeConnection
    in
    ( { game = initialGame }, Cmd.none )


completedMockGame =
    Start mockCells
        -- [ 15, 14, 9, 5, 6, 11 ]
        -- [ 13, 9 ]
        |> withRollback (connectAll (NEL.map idxToGP ( 15, [ 14, 9, 5, 6, 11 ] )))
        |> withRollback completeConnection


mockCells =
    List.range 1 16
        |> List.map (\i -> ( idxToGP i, i ))


idxToGP i =
    ( modBy 4 (i - 1), (i - 1) // 4 )


withRollback fn a =
    fn a |> Maybe.withDefault a


connectAll : NEL GP -> Game -> Maybe Game
connectAll ( h, t ) g =
    connect h g
        |> Maybe.andThen
            (case NEL.fromList t of
                Just nel ->
                    connectAll nel

                Nothing ->
                    Just
            )


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
            [ viewGame model.game
            , viewGame completedMockGame
            ]
        ]


viewGame game =
    case game of
        Start cells ->
            viewGrid (cells |> List.map StaticTile)

        Connecting connectionCells cells ->
            tileContainer (NEL.toList connectionCells |> List.map viewConnectingTile)

        Connected tiles ->
            viewGrid tiles


tileContainer children =
    div
        [ style "display" "inline-block"
        , style "align-self" "start"
        , style "overflow" "hidden"
        ]
        [ div
            [ style "background-color" "#555"
            , style "border-radius" "0.5rem"
            , style "position" "relative"
            ]
            [ Svg.svg
                [ style "position" "absolute"
                , style "display" "inline-block"
                , SA.viewBox "-0.5 -0.5 4 4"
                , style "inset" "0"
                , SA.strokeWidth "0.05"

                -- , style "z-index" "1"
                ]
                [ Svg.polyline [ SA.points "0 0 1 1", SA.stroke "#000" ] [] ]
            , div
                [ style "" ""
                , style "position" "relative"
                , style "display" "grid"
                , style "grid-template" "repeat(4, 100px)/ repeat(4,100px)"
                , style "padding" "0.5rem"
                , style "gap" "0.5rem"
                , style "font-size" "2rem"
                ]
                children
            ]
        ]


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


viewConnectingTile ( gp, val ) =
    div
        [ gridAreaFromGP gp
        , style "display" "grid"
        , style "background-color" "#222"
        , style "place-content" "center"
        , style "border-radius" "0.5rem"
        ]
        [ text (String.fromInt val)
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
