module Main exposing (main)

import Browser
import Doomsday
import Html exposing (Html, div, option, select, text)
import Html.Attributes exposing (attribute, class, href, selected, value)
import Html.Events exposing (onInput)
import Time exposing (Month(..))
import Time.Extra exposing (monthFromString, monthToInt, monthToString, months, weekdayStringFromInt)


main =
    Browser.sandbox
        { init = init
        , view = viewWithStylesheet
        , update = update
        }


complete : Int -> Maybe Month -> Int -> Bool
complete day month year =
    day /= 0 && month /= Nothing && year /= 0 && valid day month year


valid : Int -> Maybe Month -> Int -> Bool
valid day month year =
    if day == 31 && List.member month (List.map Just [ Feb, Apr, Jun, Sep, Nov ]) then
        False

    else if day == 30 && month == Just Feb then
        False

    else if day == 29 && month == Just Feb && modBy 4 year /= 0 then
        False

    else
        True


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectDay day_ ->
            let
                day =
                    String.toInt day_ |> Maybe.withDefault 0

                month =
                    if valid day model.month model.year then
                        model.month

                    else
                        Nothing
            in
            { model | day = day, month = month }

        SelectMonth month_ ->
            let
                month =
                    monthFromString month_

                day =
                    if valid model.day month model.year then
                        model.day

                    else
                        0
            in
            { model | month = month, day = day }

        SelectYear year_ ->
            let
                year =
                    String.toInt year_ |> Maybe.withDefault 0

                ( day, month ) =
                    if valid model.day model.month year then
                        ( model.day, model.month )

                    else
                        ( 0, Nothing )
            in
            { model | day = day, month = month, year = year }


type alias Model =
    { day : Int
    , month : Maybe Month
    , year : Int
    }


type Msg
    = SelectDay String
    | SelectMonth String
    | SelectYear String


init : Model
init =
    { day = 0
    , month = Nothing
    , year = 0
    }


viewWithStylesheet : Model -> Html Msg
viewWithStylesheet model =
    div
        []
        [ Html.node "link"
            [ attribute "rel" "stylesheet"
            , href "/style.css"
            ]
            []
        , view model
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ select
            [ onInput SelectDay ]
            (option
                [ value "0"
                , selected (model.day == 0)
                ]
                [ text "Select a day"
                ]
                :: List.map
                    (\opt ->
                        option
                            []
                            [ text (String.fromInt opt)
                            ]
                    )
                    (List.range 1 31)
            )
        , select
            [ onInput SelectMonth ]
            (option
                [ value "Nothing"
                , selected (model.month == Nothing)
                ]
                [ text "Select a month"
                ]
                :: List.map
                    (\opt ->
                        option
                            []
                            [ text opt
                            ]
                    )
                    (List.map monthToString months)
            )
        , select
            [ onInput SelectYear ]
            (option
                [ value "0"
                , selected (model.year == 0)
                ]
                [ text "Select a year"
                ]
                :: List.map
                    (\opt ->
                        option
                            []
                            [ text (String.fromInt opt)
                            ]
                    )
                    (List.range 1800 2100)
            )
        , div
            []
            [ if complete model.day model.month model.year then
                div
                    []
                    [ text (String.fromInt model.day ++ " " ++ monthToString (Maybe.withDefault Jan model.month) ++ " " ++ String.fromInt model.year)
                    , case weekdayStringFromInt (Doomsday.calcWeekday model.day (monthToInt model.month) model.year) of
                        Just weekday ->
                            text (" is on " ++ weekday)

                        Nothing ->
                            text ""
                    ]

              else
                text ""
            ]
        ]
