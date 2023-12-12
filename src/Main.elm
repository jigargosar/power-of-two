module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes as HA exposing (attribute, class, style)
import Html.Events as HE exposing (onClick)
import Html.Keyed
import Json.Decode as JD
import List.Extra as LE
import List.Nonempty as NEL
import Maybe.Extra as ME
import Random exposing (Generator, Seed)
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
    | ConnectingTile Cell { to : GP }
    | LastConnectingTile Cell { to : ( Float, Float ) }


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

        _ ->
            Debug.todo "impl"


tileCell tile =
    case tile of
        StaticTile cell ->
            cell

        MergedTile cell _ _ ->
            cell

        DroppedTile cell _ ->
            cell

        _ ->
            Debug.todo "impl"


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

        Connected tiles ->
            -- Just (Start (List.map tileCell tiles))
            connect gp (Start (List.map tileCell tiles))


areNeighbours : GP -> GP -> Bool
areNeighbours a b =
    let
        dxy =
            tmap2 sub a b |> tmap abs
    in
    dxy == ( 1, 1 ) || dxy == ( 1, 0 ) || dxy == ( 0, 1 )


onTileEntered : GP -> Game -> Maybe Game
onTileEntered gp game =
    case game of
        Connecting (( last, previous ) as connectionCells) cells ->
            if areNeighbours gp (cellGP last) then
                findCellAt gp cells
                    |> Maybe.andThen
                        (\cell ->
                            addConnectionCell cell connectionCells
                                |> Maybe.map
                                    (\newConnectionCells ->
                                        Connecting newConnectionCells (LE.remove cell cells)
                                    )
                        )
                    |> ME.orElseLazy
                        (\_ ->
                            LE.uncons previous
                                |> Maybe.andThen
                                    (\(( secondLast, _ ) as newConnectionCells) ->
                                        if cellGP secondLast == gp then
                                            Just (Connecting newConnectionCells (last :: cells))

                                        else
                                            Nothing
                                    )
                        )

            else
                Nothing

        _ ->
            Nothing


onTileClicked : GP -> Game -> Maybe Game
onTileClicked gp game =
    case game of
        Start cells ->
            findCellAt gp cells
                |> Maybe.map
                    (\cell ->
                        Connecting (initConnectionCells cell) (LE.remove cell cells)
                    )

        Connecting connectionCells cells ->
            if lastConnectionCellGP connectionCells == gp && NEL.length connectionCells > 1 then
                Just (Connected (completeConnectionHelp connectionCells cells))

            else
                Just (Start (NEL.toList connectionCells ++ cells))

        Connected tiles ->
            -- Just (Start (List.map tileCell tiles))
            connect gp (Start (List.map tileCell tiles))


completeConnection : Game -> Maybe Game
completeConnection game =
    case game of
        Connecting connectionCells cells ->
            Just (Connected (completeConnectionHelp connectionCells cells))

        _ ->
            Nothing


completeConnectionHelp : ConnectionCells -> Cells -> Tiles
completeConnectionHelp connectionCells nonConnectionCells =
    let
        dropTileDelay =
            NEL.length connectionCells

        droppedAndStaticTiles =
            nonConnectionCells
                |> List.map
                    (\( gp, v ) ->
                        let
                            ct =
                                countHolesBelow gp connectionCells
                        in
                        if ct > 0 then
                            DroppedTile ( gpMoveDownBy ct gp, v ) { dy = ct, dropTileDelay = dropTileDelay }

                        else
                            StaticTile ( gp, v )
                    )

        mergedTile =
            let
                gp =
                    lastConnectionCellGP connectionCells

                ct =
                    countHolesBelow gp connectionCells
            in
            MergedTile ( gpMoveDownBy ct gp, 99 ) ct connectionCells

        tileExistsAt gp =
            List.any (tileGP >> eq gp) (mergedTile :: droppedAndStaticTiles)

        emptyGPs =
            NEL.toList connectionCells
                ++ nonConnectionCells
                |> List.map cellGP
                |> reject tileExistsAt
    in
    mergedTile
        :: droppedAndStaticTiles
        ++ createNewDroppedTiles dropTileDelay emptyGPs
        ++ []


gpMoveDownBy dy ( x, y ) =
    ( x, y + dy )


countHolesBelow : GP -> ConnectionCells -> Int
countHolesBelow ( x, y ) ( _, prevConnectionTiles ) =
    LE.count (\( ( hx, hy ), _ ) -> x == hx && y < hy) prevConnectionTiles


lastConnectionCellGP : ConnectionCells -> GP
lastConnectionCellGP ( ( gp, _ ), _ ) =
    gp


createNewDroppedTiles dropTileDelay emptyGPs =
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
    { ct : Int, game : Game }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialSeed =
            Random.initialSeed 0

        ( initialCells, seed ) =
            Random.step randomInitialCells initialSeed

        initialGame =
            Start initialCells
    in
    ( { ct = 0, game = initialGame }, Cmd.none )


randomVal : Generator Val
randomVal =
    Random.uniform 2 [ 4, 8, 16 ]


randomInitialCells : Generator Cells
randomInitialCells =
    Random.list 16 randomVal
        |> Random.map
            (\vals ->
                vals
                    |> List.indexedMap (\i val -> ( idxToGP (i + 1), val ))
            )


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
    = TileClicked GP
    | TileEntered GP


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TileClicked gp ->
            ( case onTileClicked gp model.game of
                Just game ->
                    { model | ct = model.ct + 1, game = game }

                _ ->
                    model
            , Cmd.none
            )

        TileEntered gp ->
            ( case onTileEntered gp model.game of
                Just game ->
                    { model | ct = model.ct + 1, game = game }

                _ ->
                    model
            , Cmd.none
            )


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
        , div
            [ style "display" "flex"
            , style "gap" "1rem"
            ]
            [ viewKeyedContent (String.fromInt model.ct) (viewGame model.game)
            ]
        ]


viewGame game =
    case game of
        Start cells ->
            viewGrid (cells |> List.map StaticTile)

        Connecting connectionCells cells ->
            viewGrid
                (let
                    lastCell =
                        NEL.head connectionCells

                    lastCellGP =
                        cellGP lastCell
                 in
                 [ LastConnectingTile lastCell { to = tmap toFloat lastCellGP } ]
                    ++ (List.foldl
                            (\c ( toGP, acc ) -> ( cellGP c, ConnectingTile c { to = toGP } :: acc ))
                            ( lastCellGP, [] )
                            (NEL.tail connectionCells)
                            |> Tuple.second
                       )
                    ++ List.map StaticTile cells
                )

        Connected tiles ->
            viewGrid tiles


viewGrid tiles =
    div
        [ style "display" "inline-block"
        , style "align-self" "start"
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
                , style "overflow" "hidden"
                ]
                (tiles |> List.map viewTile)
            ]
        ]


onMouseOver msg =
    -- HE.on "mouseover" (JD.succeed msg)
    HE.onMouseOver msg


viewTile tile =
    case tile of
        StaticTile ( gp, val ) ->
            div
                [ style "display" "grid"
                , style "background-color" "#111"
                , style "place-content" "center"
                , style "border-radius" "0.5rem"
                , style "grid-area" (gridAreaFromGP gp)
                , onClick (TileClicked gp)
                , onMouseOver (TileEntered gp)
                ]
                [ text (String.fromInt val)
                ]

        ConnectingTile ( gp, val ) { to } ->
            viewContents
                [ viewConnectingStroke (tmap toFloat gp) (tmap toFloat to)
                , viewConnectingTile gp val
                ]

        LastConnectingTile ( gp, val ) { to } ->
            viewContents
                [ viewConnectingStroke (tmap toFloat gp) to
                , viewConnectingTile gp val
                ]

        MergedTile td mdy connectionCells ->
            let
                viewNewMergedTile ( gp, val ) =
                    withStyles
                        div
                        [ cssVars
                            [ "--diff-x:" ++ String.fromInt 0
                            , "--diff-y:" ++ String.fromInt mdy
                            , "--drop-tile-delay: calc($len * var(--unit-time))"
                                |> String.replace "$len" (String.fromInt (NEL.length connectionCells))
                            , "--merge-appear-delay: calc(var(--drop-tile-delay) - var(--unit-time))"
                            ]
                        , style_ "animation"
                            ([ "var(--unit-time) ease-out var(--merge-appear-delay) 1 normal both running merged-appear"
                             , "calc(var(--unit-time) * var(--diff-y)) ease-out var(--drop-tile-delay) 1 normal both running slide-from-diff"
                             ]
                                |> String.join ","
                            )
                        , style_ "grid-area" (gridAreaFromGP gp)
                        , style_ "display" "grid"
                        , style_ "background-color" "#111"
                        , style_ "place-content" "center"
                        , style_ "border-radius" "0.5rem"
                        ]
                        [ onClick (TileClicked gp) ]
                        [ text (String.fromInt val)
                        , div [ style "font-size" "0.5rem" ] [ text ("merged dy = " ++ String.fromInt mdy) ]
                        ]
            in
            viewContents
                ([]
                    ++ (let
                            collapsingCells =
                                List.reverse (NEL.toList connectionCells)

                            collapsingToCells =
                                List.map2
                                    (\( gp, v ) ( ngp, _ ) ->
                                        ( ( gp, v ), ngp )
                                    )
                                    collapsingCells
                                    (List.drop 1 collapsingCells ++ (LE.last collapsingCells |> Maybe.map List.singleton |> Maybe.withDefault []))
                        in
                        List.indexedMap viewCollapsingToTile collapsingToCells
                       )
                    ++ [ viewNewMergedTile td ]
                )

        DroppedTile ( gp, val ) { dy, dropTileDelay } ->
            withStyles div
                [ cssVars
                    [ "--diff-x:" ++ String.fromInt 0
                    , "--diff-y:" ++ String.fromInt dy
                    , "--drop-tile-delay: calc($len * var(--unit-time))"
                        |> String.replace "$len" (String.fromInt dropTileDelay)
                    ]
                , style_ "animation" "calc(var(--unit-time) * var(--diff-y)) ease-out var(--drop-tile-delay) 1 normal both running slide-from-diff"
                , style_ "grid-area" (gridAreaFromGP gp)
                , style_ "display" "grid"
                , style_ "background-color" "#111"
                , style_ "place-content" "center"
                , style_ "border-radius" "0.5rem"
                ]
                [ onClick (TileClicked gp) ]
                [ text (String.fromInt val)
                , div [ style "font-size" "0.5rem" ] [ text ("drop dy = " ++ String.fromInt dy) ]
                ]


viewConnectingTile gp val =
    div
        [ style "grid-area" (gridAreaFromGP gp)
        , style "display" "grid"
        , style "background-color" "#222"
        , style "place-content" "center"
        , style "border-radius" "0.5rem"
        , onClick (TileClicked gp)
        , onMouseOver (TileEntered gp)
        ]
        [ text (String.fromInt val)
        ]


viewCollapsingToTile i ( ( ( gx, gy ) as gp, val ), ( ngx, ngy ) as ngp ) =
    let
        slideDuration =
            "calc(var(--unit-time))"

        slideDelay =
            "calc($slideDuration * $idx)"
                |> String.replace "$slideDuration" slideDuration
                |> String.replace "$idx" (String.fromInt i)
    in
    viewContents
        [ viewCollapsingStroke slideDuration slideDelay (tmap toFloat gp) (tmap toFloat ngp)
        , withStyles div
            [ cssVars
                [ "--diff-x:" ++ String.fromInt (ngx - gx)
                , "--diff-y:" ++ String.fromInt (ngy - gy)
                , "--duration:" ++ slideDuration
                , "--delay:" ++ slideDelay
                ]
            , style_ "animation" "var(--duration) ease-out var(--delay) 1 normal both running slide-to-diff-and-vanish"
            , style_ "grid-area" (gridAreaFromGP ( gx, gy ))
            , style_ "display" "grid"
            , style_ "background-color" "#111"
            , style_ "place-content" "center"
            , style_ "border-radius" "0.5rem"
            ]
            []
            [ text (String.fromInt val)
            , div [ style "font-size" "0.5rem" ] [ text ("cidx = " ++ String.fromInt i) ]
            ]
        ]


viewConnectingStroke ( gx, gy ) ( ngx, ngy ) =
    Svg.svg
        [ style "position" "absolute"
        , style "display" "inline-block"
        , style "inset" "0"
        , style "z-index" "1"
        , style "pointer-events" "none"
        , SA.viewBox "-0.5 -0.5 4 4"
        , SA.strokeWidth "0.05"
        ]
        [ Svg.polyline
            [ SA.points ([ gx, gy, ngx, ngy ] |> List.map String.fromFloat |> String.join " ")
            , SA.stroke "#999"
            ]
            []
        ]


viewCollapsingStroke slideDuration slideDelay ( gx, gy ) ( ngx, ngy ) =
    Svg.svg
        [ style "position" "absolute"
        , style "display" "inline-block"
        , style "inset" "0"
        , style "z-index" "1"
        , style "pointer-events" "none"
        , SA.viewBox "-0.5 -0.5 4 4"
        , SA.strokeWidth "0.05"
        ]
        [ withStyles Svg.polyline
            [ cssVars
                [ "--duration:" ++ slideDuration
                , "--delay:" ++ slideDelay
                ]
            , style_ "animation" "var(--duration) ease-out var(--delay) 1 normal both running stroke-vanish"
            , style_ "stroke-dasharray" "1"
            , style_ "stroke-dashoffset" "-0.1"
            , style_ "stroke-dashoffset" "0"
            ]
            [ SA.points ([ gx, gy, ngx, ngy ] |> List.map String.fromFloat |> String.join " ")
            , SA.stroke "#999"
            , SA.pathLength "1"
            ]
            []
        ]


style_ k v =
    k ++ ":" ++ v



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


tmap fn =
    Tuple.mapBoth fn fn


tmap2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


sub =
    (-)


ms f =
    String.fromFloat f ++ "ms"


type alias Styles_ =
    List String


withStyles : (List (Attribute msg) -> List (Html msg) -> Html msg) -> Styles_ -> List (Attribute msg) -> List (Html msg) -> Html msg
withStyles fn styles attrs =
    fn (attrStyle styles :: attrs)


attrStyle styles =
    styles |> String.join ";" |> attribute "style"


cssVars styles =
    styles |> String.join ";"


gridAreaFromGP ( x, y ) =
    String.fromInt (y + 1) ++ "/" ++ String.fromInt (x + 1)


viewKeyedContent key content =
    Html.Keyed.node "div" [ HA.style "display" "contents" ] [ ( key, content ) ]


viewContents =
    Html.div [ HA.style "display" "contents" ]


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

@keyframes stroke-vanish { 
    from{
        stroke-dasharray:1;
        stroke-dashoffset:0;
    }
    to{
        stroke-dashoffset:-1;
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
