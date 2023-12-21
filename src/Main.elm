module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes as HA exposing (attribute, class, style)
import Html.Events as HE exposing (onClick, onMouseOver)
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


allGPs =
    let
        ( w, h ) =
            ( 4, 4 )
    in
    LE.initialize
        (w * h)
        (\i -> ( modBy w i, i // h ))


gpNeighbours : GP -> GP -> Bool
gpNeighbours a b =
    let
        dxy =
            tmap2 sub a b |> tmap abs
    in
    dxy == ( 1, 1 ) || dxy == ( 1, 0 ) || dxy == ( 0, 1 )


type alias Val =
    Int


randomVal : Generator Val
randomVal =
    Random.uniform 2 [ 4, 8, 16 ]


type alias Cell =
    ( GP, Val )


cellNeighbours : Cell -> Cell -> Bool
cellNeighbours a b =
    gpNeighbours (cellGP a) (cellGP b)


cellGP : Cell -> GP
cellGP =
    Tuple.first


type alias Cells =
    List Cell


randomInitialCells : Generator Cells
randomInitialCells =
    Random.list (List.length allGPs) randomVal
        |> Random.map (LE.zip allGPs)


findCellAt : GP -> Cells -> Maybe Cell
findCellAt gp cells =
    LE.find (cellGP >> eq gp) cells


type Tile
    = StaticTile Cell
    | MergedTile Cell Int Cells
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


type Game
    = Running Cells Cells
    | ConnectionComplete Cells Tiles


randomGame : Generator Game
randomGame =
    randomInitialCells
        |> Random.map (Running [])


onTileClicked : GP -> Game -> Maybe (Generator Game)
onTileClicked gp game =
    case game of
        Running connecting notConnecting ->
            onTileClickedHelp gp connecting notConnecting

        ConnectionComplete cells tiles ->
            onTileClickedHelp gp [] cells


onTileClickedHelp : GP -> Cells -> Cells -> Maybe (Generator Game)
onTileClickedHelp gp connecting notConnecting =
    case connecting of
        -- not connecting
        [] ->
            -- start connecting
            findCellAt gp notConnecting
                |> Maybe.map
                    (\cell ->
                        Running [ cell ] (LE.remove cell notConnecting)
                            |> Random.constant
                    )

        -- just started connecting
        only :: [] ->
            -- reset connecting chain
            Running [] (connecting ++ notConnecting)
                |> Random.constant
                |> Just

        -- may complete connecting
        last :: secondLast :: previous ->
            if cellGP last == gp then
                finishConnecting last (secondLast :: previous) notConnecting
                    |> Just

            else
                -- reset connecting chain
                Running [] (connecting ++ notConnecting)
                    |> Random.constant
                    |> Just


onTileEntered : GP -> Game -> Maybe Game
onTileEntered gp game =
    case game of
        Running connecting notConnecting ->
            case connecting of
                [] ->
                    Nothing

                -- just started connecting
                only :: [] ->
                    -- attempt to extend connecting chain of single cell
                    findCellAt gp notConnecting
                        |> Maybe.andThen
                            (\cell ->
                                if
                                    cellNeighbours cell only
                                    -- && val eq
                                then
                                    Just (Running (cell :: connecting) (LE.remove cell notConnecting))

                                else
                                    Nothing
                            )

                -- can remove last connecting cell or extend chain
                last :: secondLast :: previous ->
                    if cellGP secondLast == gp then
                        -- remove last connecting cell
                        Just (Running (secondLast :: previous) (last :: notConnecting))

                    else
                        -- attempt to extend connecting chain of more than one cell
                        findCellAt gp notConnecting
                            |> Maybe.andThen
                                (\cell ->
                                    if
                                        cellNeighbours cell last
                                        -- && val eq or next power of two
                                    then
                                        Just (Running (cell :: connecting) (LE.remove cell notConnecting))

                                    else
                                        Nothing
                                )

        ConnectionComplete _ _ ->
            Nothing


finishConnecting : Cell -> Cells -> Cells -> Generator Game
finishConnecting last previous notConnecting =
    let
        dropTileDelay =
            List.length previous + 1

        holesBelow ( x, y ) =
            LE.count (\( ( hx, hy ), _ ) -> x == hx && y < hy) previous

        droppedAndStaticTiles =
            notConnecting
                |> List.map
                    (\( gp, v ) ->
                        let
                            ct =
                                holesBelow gp
                        in
                        if ct > 0 then
                            DroppedTile ( gpMoveDownBy ct gp, v ) { dy = ct, dropTileDelay = dropTileDelay }

                        else
                            StaticTile ( gp, v )
                    )

        mergedTile =
            let
                gp =
                    cellGP last

                ct =
                    holesBelow gp

                val =
                    mergeVal (last :: previous)
            in
            MergedTile ( gpMoveDownBy ct gp, val ) ct (last :: previous)

        filledGPs =
            List.map tileGP (mergedTile :: droppedAndStaticTiles)

        emptyGPs =
            allGPs |> removeAll filledGPs
    in
    createNewDroppedTiles dropTileDelay emptyGPs
        |> Random.map
            (\newDroppedTiles ->
                let
                    tiles =
                        mergedTile
                            :: droppedAndStaticTiles
                            ++ newDroppedTiles

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
                in
                ConnectionComplete (List.map tileCell tiles) tiles
            )


mergeVal cells =
    cells
        |> List.map Tuple.second
        |> List.sum
        |> toFloat
        |> logBase 2
        |> ceiling
        |> (^) 2


gpMoveDownBy dy ( x, y ) =
    ( x, y + dy )


createNewDroppedTiles : Int -> List GP -> Generator (List Tile)
createNewDroppedTiles dropTileDelay emptyGPs =
    let
        maxYOfEmptyGPs =
            emptyGPs
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0

        dy =
            maxYOfEmptyGPs + 1
    in
    Random.list (List.length emptyGPs) randomVal
        |> Random.map
            (List.map2
                (\gp val ->
                    DroppedTile ( gp, val ) { dy = dy, dropTileDelay = dropTileDelay }
                )
                emptyGPs
            )


type alias Model =
    { ct : Int, game : Game, seed : Seed }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialSeed =
            Random.initialSeed 0

        ( initialGame, seed ) =
            Random.step randomGame initialSeed
    in
    ( { ct = 0, game = initialGame, seed = seed }, Cmd.none )


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
                Just gameGen ->
                    let
                        ( game, seed ) =
                            Random.step gameGen model.seed
                    in
                    { model | ct = model.ct + 1, game = game, seed = seed }

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
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "1rem"
        ]
        [ div [] [ text (gameMergeTileHint game) ]
        , viewTiles (gameTiles game)
        ]


gameTiles game =
    case game of
        Running connecting notConnecting ->
            connectingTiles connecting ++ List.map StaticTile notConnecting

        ConnectionComplete _ tiles ->
            tiles


connectingTiles connecting =
    case connecting of
        [] ->
            []

        last :: previous ->
            let
                lastCellGP =
                    cellGP last
            in
            LastConnectingTile last { to = tmap toFloat lastCellGP }
                :: (List.foldl
                        (\c ( toGP, acc ) -> ( cellGP c, ConnectingTile c { to = toGP } :: acc ))
                        ( lastCellGP, [] )
                        previous
                        |> Tuple.second
                   )


gameMergeTileHint game =
    case game of
        Running connecting notConnecting ->
            "Merge Val = " ++ String.fromInt (mergeVal connecting)

        ConnectionComplete _ tiles ->
            "\u{00A0}"


viewTiles tiles =
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
                len =
                    List.length connectionCells

                viewNewMergedTile ( gp, val ) =
                    withStyles
                        div
                        [ cssVars
                            [ "--diff-x:" ++ String.fromInt 0
                            , "--diff-y:" ++ String.fromInt mdy
                            , "--drop-tile-delay: calc($len * var(--unit-time))"
                                |> String.replace "$len" (String.fromInt len)
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

                        -- , div [ style "font-size" "0.5rem" ] [ text ("merged dy = " ++ String.fromInt mdy) ]
                        ]
            in
            viewContents
                ([]
                    ++ (let
                            collapsingCells =
                                List.reverse connectionCells

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

                -- , div [ style "font-size" "0.5rem" ] [ text ("drop dy = " ++ String.fromInt dy) ]
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

            -- , div [ style "font-size" "0.5rem" ] [ text ("cidx = " ++ String.fromInt i) ]
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



-- UTILS


add =
    (+)


removeAll aa =
    reject (isMemberOf aa)


isMemberOf =
    flip List.member


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


style_ k v =
    k ++ ":" ++ v


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
