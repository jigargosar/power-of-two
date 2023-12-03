module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes as HA exposing (attribute, class, style)
import Html.Events as HE exposing (onClick)
import List.Extra as LE
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
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


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
        , let
            tiles =
                initialTiles
                    |> updateTilesWithConnections2
                        [ ( 0, 2 )
                        , ( 0, 1 )
                        , ( 1, 1 )
                        ]
          in
          viewGrid tiles
        ]


type alias GP =
    ( Int, Int )


type alias Val =
    Int


type alias TileData =
    ( GP, Val )


type TileVM
    = StaticTile TileData
    | MergedTile TileData (List TileData)
    | DroppedTile TileData Int



--initialTileVMs =
--    List.range 1 16
--        |> List.map
--            (\i ->
--                let
--                    gp =
--                        ( modBy 4 (i - 1), (i - 1) // 4 )
--                in
--                StaticTile ( gp, i )
--            )
--
--


initialTiles =
    List.range 1 16
        |> List.map
            (\i ->
                let
                    gp =
                        ( modBy 4 (i - 1), (i - 1) // 4 )
                in
                ( gp, i )
            )


updateTilesWithConnections2 : List GP -> List TileData -> List TileVM
updateTilesWithConnections2 cgps its =
    case LE.last cgps of
        Nothing ->
            []

        Just mgp ->
            let
                ( ctiles, others ) =
                    List.partition (\( gp, v ) -> List.member gp cgps) its

                mergedTileVM =
                    MergedTile ( mgp, 99 ) ctiles

                droppedAndStaticTileVMs =
                    others
                        |> List.map
                            (\( ( x, y ), v ) ->
                                let
                                    ct =
                                        LE.count (\( cx, cy ) -> x == cx && y < cy) cgps
                                in
                                if ct > 0 then
                                    DroppedTile ( ( x, y + ct ), v ) ct

                                else
                                    StaticTile ( ( x, y ), v )
                            )
            in
            mergedTileVM :: droppedAndStaticTileVMs


updateTilesWithConnections connections tiles =
    let
        staticTiles =
            tiles
                |> List.filterMap
                    (\( gp, val ) ->
                        if List.member gp connections then
                            Nothing

                        else
                            Just (StaticTile ( gp, val ))
                    )

        mergedTiles =
            connections
                |> List.filterMap
                    (\cgp -> tiles |> LE.find (\( gp, _ ) -> gp == cgp))
                |> (\ls ->
                        ls
                            |> LE.last
                            |> Maybe.map (\( gp, _ ) -> [ MergedTile ( gp, 99 ) ls ])
                            |> Maybe.withDefault []
                   )
    in
    staticTiles ++ mergedTiles


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

        MergedTile td ls ->
            let
                v1 i ( gp, val ) =
                    div
                        [ gridAreaFromGP gp
                        , style "display" "grid"
                        , style "background-color" "#111"
                        , style "place-content" "center"
                        , style "border-radius" "0.5rem"
                        , style "opacity" "0.5"
                        ]
                        [ text (String.fromInt val)
                        , div [ style "font-size" "0.5rem" ] [ text ("" ++ String.fromInt i) ]
                        ]
            in
            div [ style "display" "contents" ]
                -- (v1 -1 td :: List.indexedMap v1 ls)
                (List.indexedMap v1 ls)

        DroppedTile ( gp, val ) dy ->
            div
                [ gridAreaFromGP gp
                , style "display" "grid"
                , style "background-color" "#111"
                , style "place-content" "center"
                , style "border-radius" "0.5rem"
                , style "translate" ("0 " ++ String.fromInt (dy * -110) ++ "%")
                ]
                [ text (String.fromInt val)
                ]


gridAreaFromGP ( x, y ) =
    style "grid-area" (String.fromInt (y + 1) ++ "/" ++ String.fromInt (x + 1))


padding =
    style "padding"



-- viewGrid =
--     div
--         [ style "display" "inline-block"
--         , style "align-self" "start"
--         , style "overflow" "hidden"
--         , padding "1rem"
--         ]
--         [ div
--             [ style "background-color" "#333"
--             , style "border-radius" "0.5rem"
--             , style "position" "relative"
--             ]
--             [ text ""
--             , viewConnections
--             , viewCells
--             ]
--         ]
-- viewCells =
--     div
--         [ style "" ""
--         , style "display" "grid"
--         , style "grid-template" "repeat(4, 100px)/ repeat(4,100px)"
--         , style "padding" "0.5rem"
--         , style "gap" "0.5rem"
--         ]
--         (List.map
--             (\i ->
--                 let
--                     styleLookup =
--                         [ ( 1
--                           , ( [ "--drop-down-diff:2" ]
--                             , [ style "animation" "1000ms ease-out 1000ms 1 normal both running drop-down-cell"
--                               ]
--                             )
--                           )
--                         , ( 2
--                           , ( [ "--drop-down-diff:1" ]
--                             , [ style "animation" "1000ms ease-out 1000ms 1 normal both running drop-down-cell"
--                               ]
--                             )
--                           )
--                         , ( 3
--                           , ( [ "--drop-down-diff:1" ]
--                             , [ style "animation" "1000ms ease-out 1000ms 1 normal both running drop-down-cell"
--                               ]
--                             )
--                           )
--                         , ( 9
--                           , ( [ "--diff-y:-1" ]
--                             , [ style "animation" "calc(1000ms/4) linear 0s 1 normal both running slide-for-merge"
--                               ]
--                             )
--                           )
--                         , ( 5
--                           , ( [ "--diff-x:1" ]
--                             , [ style "animation" "calc(1000ms/4) linear calc(1s / 4 * 1) 1 normal both running slide-for-merge"
--                               ]
--                             )
--                           )
--                         , ( 6
--                           , ( [ "--diff-x:1" ]
--                             , [ style "animation" "calc(1000ms/4) linear calc(1s / 4 * 2) 1 normal both running slide-for-merge"
--                               ]
--                             )
--                           )
--                         , ( 7
--                           , ( [ "--diff-x:1", "--diff-y:-1" ]
--                             , [ style "animation" "calc(1000ms/4) linear calc(1s / 4 * 3) 1 normal both running slide-for-merge"
--                               ]
--                             )
--                           )
--                         , ( 4
--                           , ( [ "--diff-x:0", "--diff-y:0" ]
--                             , [ style "animation" "calc(1000ms/4) linear calc(1s / 4 * 4) 1 normal both running slide-for-merge"
--                               ]
--                             )
--                           )
--                         ]
--                     ( computedCssVars, computedStyles ) =
--                         styleLookup
--                             |> List.filter (Tuple.first >> (\n -> n == i))
--                             |> List.head
--                             |> Maybe.map Tuple.second
--                             |> Maybe.withDefault ( [], [] )
--                 in
--                 viewTile (gpFromIndex i) i computedCssVars computedStyles
--             )
--             (List.range 1 16)
--             ++ List.map
--                 (\i ->
--                     viewTile (gpFromIndex i)
--                         i
--                         []
--                         [ style "opacity" "0"
--                         , style "animation" "1000ms ease-out 1000ms 1 normal both running appear-from-top"
--                         ]
--                 )
--                 [ 1, 5, 2, 3 ]
--             ++ [ viewTile (gpFromIndex 4) 256 [] <|
--                     [ style "animation" "1000ms ease-out 1000ms 1 normal both running merged-appear"
--                     ]
--                ]
--         )
-- viewTileAtIndex i =
--     viewTile (gpFromIndex i) i
-- gpFromIndex i =
--     ( modBy 4 (i - 1), (i - 1) // 4 )
-- viewTile gp val cssVars attrs =
--     div
--         ([ attribute "style" (cssVars |> String.join ";")
--          , gridAreaFromGP gp
--          , style "display" "grid"
--          , style "background-color" "#111"
--          , style "place-content" "center"
--          , style "border-radius" "0.5rem"
--          , style "z-index" "1"
--          ]
--             ++ attrs
--         )
--         [ text (String.fromInt val)
--         -- , text <| Debug.toString gp
--         ]
-- viewConnections =
--     Svg.svg
--         [ SA.viewBox "-0.5 -0.5 4 4"
--         , style "position" "absolute"
--         , style "inset" "0"
--         , style "fill" "none"
--         , style "pointer-events" "none"
--         , style "z-index" "1"
--         ]
--         [ Svg.polyline
--             [ SA.points "0,2 0,1 1,1 2,1 3,0"
--             , SA.stroke "#666"
--             , SA.strokeWidth "0.04"
--             , SA.pathLength "1"
--             , style "stroke-dasharray" "1"
--             , style "stroke-dashoffset" "0"
--             , style "stroke-dashoffset" "-1"
--             , style "transition" "all 1s"
--             , style "animation" "1s linear 0s 1 normal both running collapse-stroke"
--             ]
--             []
--         ]


globalStyles =
    Html.node "style" [] [ text """

:root{
    height:100%;
    font-family:Arial, sans-serif;
    font-size:20px;
    background:#111;
    color:#eee;
}

* { box-sizing:border-box; }

body{ margin:0; height:100%; }

@keyframes vanish-stroke { to{stroke-width:0;}}
@keyframes collapse-stroke { 
    from{stroke-dashoffset:0;}
    to{stroke-dashoffset:-1;}
}
@keyframes vanish { to{scale:0;} }

@keyframes drop-down-cell { 
    to{translate:0 calc( (100% + 0.5rem) * var(--drop-down-diff,0));} 
}

@keyframes appear-from-top { 
    from{
        opacity:1;
        translate:0 calc( -1 * (100% + 0.5rem) * var(--appear-from-top-diff,2));
    }
    to{ opacity:1;}
}

@keyframes merged-appear{
    from{scale:0;}
    50%{scale: 1.2;}
    to{scale:1;}
}

@keyframes slide-for-merge { 
    from{opacity:1;}
    to{
        translate: calc( (100% + 0.5rem) * var(--diff-x,0))
                   calc( (100% + 0.5rem) * var(--diff-y,0)) ;

        opacity:1;
        _visibility:hidden;
        scale:0;
    }
}


    """ ]
