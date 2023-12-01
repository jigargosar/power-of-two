module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes as HA exposing (attribute, class, style)
import Html.Events as HE exposing (onClick)
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


padding =
    style "padding"


view : Model -> Html Msg
view model =
    div
        []
        [ globalStyles
        , viewGrid
        , text "V7 emulate mouse for making connections"
        ]


viewGrid =
    div
        [ style "display" "inline-block"
        , padding "1rem"
        ]
        [ div
            [ style "background-color" "#333"
            , style "border-radius" "0.5rem"
            , style "position" "relative"
            ]
            [ text ""
            , viewCells
            , viewMouseConnections

            -- , viewConnections
            -- , viewNewCells
            ]
        ]


viewNewCells =
    div
        [ style "" ""
        , style "display" "grid"
        , style "grid-template" "repeat(4, 100px)/ repeat(4,100px)"
        , style "padding" "0.5rem"
        , style "gap" "0.5rem"
        , style "position" "absolute"
        , style "inset" "0"
        ]
        (List.map
            (\i ->
                div
                    [ style "opacity" "0"
                    , style "display" "grid"
                    , style "background-color" "#111"
                    , style "place-content" "center"
                    , style "border-radius" "0.5rem"
                    , if List.member i [ 1, 2, 3, 5, 7 ] then
                        style "animation" "1000ms ease-out 1000ms 1 normal both running appear-from-top"

                      else
                        style "" ""
                    ]
                    [ text (String.fromInt i) ]
            )
            (List.range 1 16)
        )


viewCells =
    div
        [ style "" ""
        , style "display" "grid"
        , style "grid-template" "repeat(4, 100px)/ repeat(4,100px)"
        , style "padding" "0.5rem"
        , style "gap" "0.5rem"
        ]
        (List.map
            (\i ->
                let
                    styleLookup =
                        [ ( 9
                          , ( [ "--diff-y:-1" ]
                            , [ style "animation" "calc(1000ms/4) linear 0s 1 normal both running slide-for-merge"
                              ]
                            )
                          )
                        , ( 5
                          , ( [ "--diff-x:1" ]
                            , [ style "animation" "calc(1000ms/4) linear calc(1s / 4 * 1) 1 normal both running slide-for-merge"
                              ]
                            )
                          )
                        , ( 6
                          , ( [ "--diff-x:1" ]
                            , [ style "animation" "calc(1000ms/4) linear calc(1s / 4 * 2) 1 normal both running slide-for-merge"
                              ]
                            )
                          )
                        , ( 7
                          , ( [ "--diff-x:1", "--diff-y:-1" ]
                            , [ style "animation" "calc(1000ms/4) linear calc(1s / 4 * 3) 1 normal both running slide-for-merge"
                              ]
                            )
                          )
                        ]

                    ( computedCssVars, computedStyles ) =
                        styleLookup
                            |> List.filter (Tuple.first >> (\n -> n == i))
                            |> List.head
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault ( [], [] )
                            |> always ( [], [] )
                in
                div
                    ([ attribute "style" (computedCssVars |> String.join ";")
                     , style "display" "grid"
                     , style "background-color" "#111"
                     , style "place-content" "center"
                     , style "border-radius" "0.5rem"
                     , style "z-index" "1"
                     ]
                        ++ computedStyles
                    )
                    [ text (String.fromInt i) ]
            )
            (List.range 1 16)
        )


viewConnections =
    Svg.svg
        [ SA.viewBox "-0.5 -0.5 4 4"
        , style "position" "absolute"
        , style "inset" "0"
        , style "fill" "none"
        , style "pointer-events" "none"
        , style "z-index" "1"
        ]
        [ Svg.polyline
            [ SA.points "0,2 0,1 1,1 2,1 3,0"
            , SA.stroke "#666"
            , SA.strokeWidth "0.04"
            , SA.pathLength "1"
            , style "stroke-dasharray" "1"
            , style "stroke-dashoffset" "0"
            , style "stroke-dashoffset" "-1"
            , style "transition" "all 1s"
            , style "animation" "1s linear 0s 1 normal both running collapse-stroke"
            ]
            []
        ]


viewMouseConnections =
    Svg.svg
        [ SA.viewBox "-0.5 -0.5 4 4"
        , style "position" "absolute"
        , style "inset" "0"
        , style "fill" "none"
        , style "overflow" "visible"

        -- , style "pointer-events" "none"
        , style "z-index" "1"
        ]
        [ Svg.polyline
            [ -- this is how we computed stroke end point reflecting mouse pos.
              -- $('svg').addEventListener('mousemove',e=>
              --     console.log(e.offsetX * 4 / 450 - 0.5,e.offsetY * 4 / 450 - 0.5))
              SA.points "0,2 4.92 1.49"
            , SA.stroke "#666"
            , SA.strokeWidth "0.04"
            ]
            []
        ]


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

@keyframes slide-for-merge { 
    from{opacity:1;}
    to{
        translate: calc( (100% + 0.5rem) * var(--diff-x,0))
                   calc( (100% + 0.5rem) * var(--diff-y,0)) ;

        opacity:1;
        visibility:hidden;
    }
}


    """ ]
