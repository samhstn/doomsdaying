module Time.Extra exposing
    ( monthFromString
    , monthToInt
    , monthToString
    , months
    , weekdayStringFromInt
    )

import Time exposing (Month(..))


months : List Month
months =
    [ Jan
    , Feb
    , Mar
    , Apr
    , May
    , Jun
    , Jul
    , Aug
    , Sep
    , Oct
    , Nov
    , Dec
    ]


monthToInt : Maybe Month -> Maybe Int
monthToInt maybeMonth =
    case maybeMonth of
        Just month ->
            List.indexedMap Tuple.pair months
                |> List.filter (\( _, m ) -> month == m)
                |> List.head
                |> Maybe.map Tuple.first

        Nothing ->
            Nothing


weekdayStringFromInt : Maybe Int -> Maybe String
weekdayStringFromInt maybeInt =
    case maybeInt of
        Just int ->
            case int of
                0 ->
                    Just "Sunday"

                1 ->
                    Just "Monday"

                2 ->
                    Just "Tuesday"

                3 ->
                    Just "Wednesday"

                4 ->
                    Just "Thursday"

                5 ->
                    Just "Friday"

                6 ->
                    Just "Saturday"

                _ ->
                    Nothing

        Nothing ->
            Nothing


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


monthFromString : String -> Maybe Month
monthFromString str =
    case str of
        "Jan" ->
            Just Jan

        "Feb" ->
            Just Feb

        "Mar" ->
            Just Mar

        "Apr" ->
            Just Apr

        "May" ->
            Just May

        "Jun" ->
            Just Jun

        "Jul" ->
            Just Jul

        "Aug" ->
            Just Aug

        "Sep" ->
            Just Sep

        "Oct" ->
            Just Oct

        "Nov" ->
            Just Nov

        "Dec" ->
            Just Dec

        _ ->
            Nothing
