module DateTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr
import Main
import Model as M
import Model.Date as Date
import Model.Interval as I
import Test exposing (..)
import Test.Html.Query as Q
import Test.Html.Selector as S
import Model.Date exposing (..)





suite : Test
suite =
    describe "Model.Date module"
        [ test "Date view for year-only date contains the year" <|
            \_ ->
                Date.view (Date.onlyYear 2010)
                    |> Q.fromHtml
                    |> Q.has [ S.text "2010" ]
        , test "Date view for full date contains the year" <|
            \_ ->
                Date.view (Date.full 2010 Date.Jan)
                    |> Q.fromHtml
                    |> Q.has [ S.text "2010" ]
        , test "Date view for full date contains the month" <|
            \_ ->
                Date.view (Date.full 2010 Date.Jan)
                    |> Q.fromHtml
                    |> Q.has [ S.text (Date.monthToString Date.Jan) ]
        ]
spec5 : Test.Test
spec5 =
    Test.test "#monthsBetween: \n\n    monthsBetween (full 2018 Mar) (full 2020 Jan)\n    --> Just 22" <|
        \() ->
            Expect.equal
                (
                monthsBetween (full 2018 Mar) (full 2020 Jan)
                )
                (
                Just 22
                )

spec4 : Test.Test
spec4 =
    Test.test "#monthsBetween: \n\n    monthsBetween (full 2018 Mar) (full 2020 Jan)\n    --> Just 40" <|
        \() ->
            Expect.equal
                (
                monthsBetween (full 2018 Mar) (full 2021 Jul)
                )
                (
                Just 40
                )