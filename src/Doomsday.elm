module Doomsday exposing (calcWeekday)

{- weekdays are corresponded to integers in the following way:
   sun 0
   mon 1
   tue 2
   wed 3
   thu 4
   fri 5
   sat 6

   day is from 1-31
   month is from 0-11
   year is > 0
-}


doomsdayFromMonthYear : Int -> Int -> Int
doomsdayFromMonthYear month year =
    case month of
        0 ->
            if modBy 4 year == 0 then
                4

            else
                3

        1 ->
            if modBy 4 year == 0 then
                29

            else
                28

        2 ->
            7

        3 ->
            4

        4 ->
            9

        5 ->
            6

        6 ->
            11

        7 ->
            8

        8 ->
            5

        9 ->
            10

        10 ->
            7

        _ ->
            12


calcWeekday : Int -> Maybe Int -> Int -> Maybe Int
calcWeekday day maybeMonthInt year =
    Maybe.map (\m -> calcWeekday_ day m year) maybeMonthInt


calcWeekday_ : Int -> Int -> Int -> Int
calcWeekday_ day month year =
    let
        anchorDay =
            if modBy 400 year >= 300 then
                3

            else if modBy 400 year >= 200 then
                5

            else if modBy 400 year >= 100 then
                0

            else
                2

        doomsdayWeekday =
            List.sum
                [ modBy 100 year // 12
                , modBy 12 (modBy 100 year)
                , modBy 12 (modBy 100 year) // 4
                , anchorDay
                ]
    in
    modBy 7 (doomsdayWeekday + day - doomsdayFromMonthYear month year)
