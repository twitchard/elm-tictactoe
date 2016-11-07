module TicTacToeTest exposing (..)

import Test exposing (..)
import TicTacToe
import Expect

xsTurn = 

ticTacToeTest : Test
ticTacToeTest =
    describe "lib/TicTacToe"
        [ describe "turn"
            [ test "Fails" <|
                \() -> Expect.fail "fails, hooray!"
            ]
        ]
