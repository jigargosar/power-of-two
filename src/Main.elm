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
        , text "V5 drop tile animation"
        ]


viewGrid =
    div
        [ padding "1rem"
        , style "display" "inline-block"
        , style "position" "relative"
        ]
        [ text ""
        , viewConnections
        , viewCells
        ]


viewCells =
    div
        [ style "" ""
        , style "display" "inline-grid"
        , style "background-color" "#333"
        , style "grid-template" "repeat(4, 100px)/ repeat(4,100px)"
        , style "padding" "0.5rem"
        , style "gap" "0.5rem"
        , style "border-radius" "0.5rem"
        ]
        (List.map
            (\i ->
                div
                    [ attribute "style"
                        ([ if List.member i [ 1 ] then
                            "--drop-down-diff:2"

                           else
                            ""
                         , if List.member i [ 2 ] then
                            "--drop-down-diff:1"

                           else
                            ""
                         ]
                            |> String.join ";"
                        )
                    , style "display" "grid"
                    , style "background-color" "#111"
                    , style "place-content" "center"
                    , style "border-radius" "0.5rem"
                    , if List.member i [ 9, 5, 6, 7, 3 ] then
                        style "animation" "1000ms ease-in 0s 1 normal both running vanish"

                      else
                        style "" ""
                    , if List.member i [ 1, 2 ] then
                        style "animation" "1000ms ease-out 1000ms 1 normal both running drop-down-cell"

                      else
                        style "" ""
                    ]
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
        ]
        [ Svg.polyline
            [ SA.points "0,2 0,1 1,1 2,1 2,0"
            , SA.stroke "#666"
            , SA.strokeWidth "0.04"
            , style "animation" "1000ms ease-in 0s 1 normal both running vanish-stroke"
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
@keyframes vanish { to{scale:0;} }
@keyframes drop-down-cell { 
    to{translate:0 calc( (100% + 0.5rem) * var(--drop-down-diff,0));} 
}


    """ ]
