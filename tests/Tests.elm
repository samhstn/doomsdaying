module Tests exposing (all)

import Doomsday exposing (calcWeekday)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Test exposing (..)


all : Test
all =
    describe "Doomsday.calcWeekday"
        (Decode.decodeString jsonDataDecoder jsonData
            |> Result.withDefault []
            |> List.map
                (\{ millis, day, weekday, month, year, name } ->
                    test name <|
                        \_ ->
                            Expect.equal (calcWeekday day (Just month) year) (Just weekday)
                )
        )


type alias TestData =
    { millis : Int
    , day : Int
    , weekday : Int
    , month : Int
    , year : Int
    , name : String
    }


jsonDataDecoder : Decoder (List TestData)
jsonDataDecoder =
    Decode.list
        (Decode.succeed TestData
            |> required "millis" Decode.int
            |> required "day" Decode.int
            |> required "weekday" Decode.int
            |> required "month" Decode.int
            |> required "year" Decode.int
            |> required "name" Decode.string
        )



{- Json data generated from:
   ```bash
   ./generate_test_data.js 10
   ```
-}


jsonData : String
jsonData =
    """
[{"millis":-2081639208193,"date":"1904-01-14T22:53:11.807Z","day":14,"weekday":4,"month":0,"year":1904,"name":"Thu, 14/1/1904"},{"millis":-5067433401007,"date":"1809-06-03T03:36:38.993Z","day":3,"weekday":6,"month":5,"year":1809,"name":"Sat, 3/6/1809"},{"millis":-5067286243189,"date":"1809-06-04T20:29:16.811Z","day":4,"weekday":0,"month":5,"year":1809,"name":"Sun, 4/6/1809"},{"millis":782304322012,"date":"1994-10-16T10:45:22.012Z","day":16,"weekday":0,"month":9,"year":1994,"name":"Sun, 16/10/1994"},{"millis":-2792242198910,"date":"1881-07-08T09:10:01.090Z","day":8,"weekday":5,"month":6,"year":1881,"name":"Fri, 8/7/1881"},{"millis":1665092918851,"date":"2022-10-06T21:48:38.851Z","day":6,"weekday":4,"month":9,"year":2022,"name":"Thu, 6/10/2022"},{"millis":2437789516534,"date":"2047-04-02T03:45:16.534Z","day":2,"weekday":2,"month":3,"year":2047,"name":"Tue, 2/4/2047"},{"millis":983948135134,"date":"2001-03-07T06:55:35.134Z","day":7,"weekday":3,"month":2,"year":2001,"name":"Wed, 7/3/2001"},{"millis":3091211427345,"date":"2067-12-15T21:50:27.345Z","day":15,"weekday":4,"month":11,"year":2067,"name":"Thu, 15/12/2067"},{"millis":957683818202,"date":"2000-05-07T07:16:58.202Z","day":7,"weekday":0,"month":4,"year":2000,"name":"Sun, 7/5/2000"}]
  """
