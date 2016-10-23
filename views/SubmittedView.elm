module SubmittedView exposing (..)

import Html

submittedView s = Html.div [] [Html.text <| "Connecting to room " ++ s ++ " ..."]
