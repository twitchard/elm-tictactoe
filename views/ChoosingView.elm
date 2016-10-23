module ChoosingView exposing (..)

import Html
import Html.Events
import Html.Attributes
import TicTacToe exposing (Marker(X, O), markerString, Choice(Nobody, TheyChose, YouChose))


choosingView name choose choice =
    let
        available marker =
            Html.div []
                [ Html.button [ Html.Events.onClick (choose marker) ] [ Html.text <| "Play as " ++ markerString marker ]
                ]

        unavailable marker id =
            Html.div [] [ Html.text <| "Player " ++ id ++ " has chosen to play as " ++ markerString marker ]
    in
        Html.div []
            [ Html.h2 [] [ Html.text <| "You have joined room " ++ name ]
            , Html.div [] <|
                case choice of
                    Nobody ->
                        [ available X, available O ]

                    TheyChose X them ->
                        [ unavailable X them, available O ]

                    TheyChose O them ->
                        [ available X, unavailable O them ]

                    YouChose m ->
                        [ Html.text "Waiting for your opponent" ]
            ]
