module Main exposing (main)

import Browser
import Doomsday
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (attribute, class, href, selected, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator)
import Time exposing (Month(..))
import Time.Extra
    exposing
        ( monthFromInt
        , monthFromString
        , monthToInt
        , monthToString
        , months
        , weekdayStringFromInt
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = viewWithStylesheet
        , update = update
        , subscriptions = \_ -> Sub.none
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectDay day_ ->
            let
                day =
                    String.toInt day_ |> Maybe.withDefault 0

                ( month, year ) =
                    if model.generated then
                        ( Nothing, 0 )

                    else if valid day model.month model.year then
                        ( model.month, model.year )

                    else
                        ( Nothing, model.year )
            in
            ( { model | day = day, month = month, year = year, showing = True, generated = False }, Cmd.none )

        SelectMonth month_ ->
            let
                month =
                    monthFromString month_

                ( day, year ) =
                    if model.generated then
                        ( 0, 0 )

                    else if valid model.day month model.year then
                        ( model.day, model.year )

                    else
                        ( 0, model.year )
            in
            ( { model | day = day, month = month, year = year, showing = True, generated = False }, Cmd.none )

        SelectYear year_ ->
            let
                year =
                    String.toInt year_ |> Maybe.withDefault 0

                ( day, month ) =
                    if model.generated then
                        ( 0, Nothing )

                    else if valid model.day model.month year then
                        ( model.day, model.month )

                    else
                        ( 0, Nothing )
            in
            ( { model | day = day, month = month, year = year, showing = True, generated = False }, Cmd.none )

        Show ->
            ( { model | showing = not model.showing }, Cmd.none )

        Generate ->
            ( model, generateDate )

        NewDate ( day, month, year ) ->
            ( { model | day = day, month = monthFromInt month, year = year, showing = False, generated = True }
            , Cmd.none
            )


generateDate : Cmd Msg
generateDate =
    Random.generate NewDate generateDate_


generateDate_ : Generator ( Int, Int, Int )
generateDate_ =
    Random.pair (Random.int 1 31) (Random.int 1800 2100)
        |> Random.andThen
            (\( day, year ) ->
                let
                    possibleMonths =
                        case ( day, modBy 4 year /= 0 ) of
                            ( 31, _ ) ->
                                [ 0, 2, 4, 6, 7, 9, 11 ]

                            ( 30, _ ) ->
                                [ 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]

                            ( 29, False ) ->
                                [ 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]

                            _ ->
                                List.range 0 11
                in
                triple (Random.constant day) (Random.int 0 11) (Random.constant year)
            )


triple : Generator Int -> Generator Int -> Generator Int -> Generator ( Int, Int, Int )
triple genA genB genC =
    Random.map3 (\a b c -> ( a, b, c )) genA genB genC


type alias Model =
    { day : Int
    , month : Maybe Month
    , year : Int
    , generated : Bool
    , showing : Bool
    }


type Msg
    = SelectDay String
    | SelectMonth String
    | SelectYear String
    | Show
    | Generate
    | NewDate ( Int, Int, Int )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { day = 0, month = Nothing, year = 0, showing = True, generated = False }, Cmd.none )


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
                            if model.showing then
                                text (" is on " ++ weekday)

                            else
                                button
                                    [ onClick Show
                                    ]
                                    [ text "Show"
                                    ]

                        Nothing ->
                            text ""
                    ]

              else
                text ""
            ]
        , div
            []
            [ button
                [ onClick Generate
                ]
                [ text "Generate"
                ]
            ]
        ]
