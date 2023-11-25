module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE exposing (onClick)
import Svg exposing (Svg)
import Svg.Attributes


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
        , div
            [ padding "1rem"
            , style "display" "inline-block"
            ]
            [ viewGrid
            ]
        , text "V3 render tile connections"
        ]


viewGrid =
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
                    [ style "display" "grid"
                    , style "background-color" "#111"
                    , style "place-content" "center"
                    , style "border-radius" "0.5rem"
                    ]
                    [ text (String.fromInt i) ]
            )
            (List.range 1 16)
        )


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

    """ ]
