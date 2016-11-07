port module Main exposing (..)

import TicTacToeTest
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Value
main =
    run emit TicTacToeTest.ticTacToeTest


port emit : ( String, Value ) -> Cmd msg
