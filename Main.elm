module Main exposing (..)
import Array exposing (Array)
import Html exposing (Html)
import Html.App
import ChoosingView exposing (..)
import ConnectingView exposing (..)
import PlayingView exposing (..)
import SubmittedView exposing (..)
import DebugView exposing (..)

type Marker
  = X
  | O

type Msg = Noop

type alias Board = Array (Array (Maybe Marker))

type State
  = Connecting
  | Choosing String
  | Submitted String
  | Playing
    { marker : Marker
    , stage  : Board
    , board  : Board
    , version: Board
    }

type alias Model =
  { id : String
  , state : State
  , debug : String
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Noop -> (model, Cmd.none)

view model =
  Html.div [] <|
    [ case model.state of
        Connecting  -> connectingView
        Choosing s  -> choosingView s
        Submitted s -> submittedView s
        Playing p   -> playingView p
    , Html.div [] [ debugView model.debug]
    ]

type alias Flags = 
  { id : String }

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { id = flags.id
    , state = Connecting
    , debug = ""
    }
  , Cmd.none
  )

subscriptions model = Sub.none

main =
  Html.App.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }

