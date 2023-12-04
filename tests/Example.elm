module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra as LE
import Test exposing (..)


suite : Test
suite =
    -- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    smokeTest


smokeTest : Test
smokeTest =
    concat
        [ test "smoke test" <|
            \_ ->
                Expect.equal True True
        , test "smoke test 2" <|
            \_ ->
                let
                    _ =
                        1
                in
                [ [ 1, 2 ]
                , [ 3, 4 ]
                ]
                    |> llToTileData
                    |> updateWithConnections [ ( 0, 1 ), ( 1, 0 ) ]
                    |> tileDataToLL
                    |> Expect.equal
                        [ [ 0, 0 ]
                        , [ 1, 4 ]
                        ]
        ]


type alias GP =
    ( Int, Int )


llToTileData : List (List a) -> List ( GP, a )
llToTileData ll =
    ll
        |> List.indexedMap
            (\y l ->
                l
                    |> List.indexedMap (\x v -> ( ( x, y ), v ))
            )
        |> List.concat


tileDataToLL : List ( GP, a ) -> List (List a)
tileDataToLL tds =
    tds
        |> LE.gatherEqualsBy (Tuple.first >> Tuple.second)
        |> List.map
            (\( h, t ) ->
                h
                    :: t
                    |> List.sortBy (Tuple.first >> Tuple.first)
                    |> List.map Tuple.second
            )


updateWithConnections cgps tll =
    let
        isConn ( gp, _ ) =
            List.member gp cgps

        ( cts, others ) =
            tll |> List.partition isConn

        -- sortFn : ( GP, a ) -> Int
        -- sortFn ( gp, _ ) =
        --     LE.elemIndex gp cgps |> Maybe.withDefault 0
        -- cts2 =
        --     List.sortBy sortFn cts
        dss =
            others
                |> List.map
                    (\( ( x, y ), v ) ->
                        let
                            ct =
                                LE.count (\( cx, cy ) -> x == cx && y < cy) cgps
                        in
                        if ct > 0 then
                            ( ( x, y + ct ), v )

                        else
                            ( ( x, y ), v )
                    )

        dsGPs =
            List.map Tuple.first dss

        newGPs =
            tll
                |> List.map Tuple.first
                |> List.filter
                    (\gp ->
                        not (List.member gp dsGPs)
                    )

        new =
            newGPs |> List.map (\gp -> ( gp, 0 ))
    in
    -- (cts |> List.map (Tuple.mapSecond (always 0))) ++ dss
    new ++ dss



--
