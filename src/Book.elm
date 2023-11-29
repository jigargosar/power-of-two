module Book exposing (main)

import ElmBook exposing (..)
import ElmBook.Actions exposing (logAction)
import ElmBook.Chapter exposing (..)
import ElmBook.ThemeOptions
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Main


main : Book ()
main =
    book "My Book"
        |> withThemeOptions
            [ ElmBook.ThemeOptions.preferDarkMode
            ]
        |> withChapters
            [ chapter "Buttons"
                |> (let
                        props =
                            { disabled = False }
                    in
                    renderComponentList
                        [ ( "Default", Main.view {} )
                        , ( "Disabled", view { props | disabled = True } )
                        ]
                   )
            ]


view _ =
    text "Foo and Bar"
