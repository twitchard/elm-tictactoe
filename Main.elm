module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.App
import Json.Decode exposing (oneOf, Decoder)
import Json.Encode
import ChoosingView exposing (choosingView)
import DebugView exposing (debugView)
import NamingView exposing (namingView)
import PlayingView exposing (playingView)
import SubmittedView exposing (submittedView)
import TicTacToe exposing (Marker(X, O), Board, Choice(Nobody, YouChose, TheyChose), Game, Players, markerOf, boardDecoder, choiceDecoder, gameDecoder, encodeGame, encodeChoice, emptyBoard, move)
import SharedState exposing (register, connectToRoom, sendSharedState, Event(ServerError, UnknownError, EmptyState, NewState))
import WebSocket


wsURL =
    "ws://localhost:8000"

type alias GameName = String
type alias VersionMarker = String

type Msg
    = Noop
    | NameInput String
    | SubmitName
    | Debug String
    | GameUpdate String SharedState
    | Choose Marker
    | Move Int Int


type State
    = Naming String
    | Submitted GameName
    | Choosing GameName VersionMarker Choice
    | Playing Players Marker GameName VersionMarker Board


type alias Model =
    { id : String
    , debug : String
    , state : State
    }


type SharedState
    = SharedChoice Choice
    | SharedGame Game


sharedStateDecoder : String -> Decoder SharedState
sharedStateDecoder you =
    oneOf
        [ Json.Decode.map SharedChoice <| choiceDecoder you
        , Json.Decode.map SharedGame gameDecoder
        ]


encodeSharedState : String -> SharedState -> Json.Encode.Value
encodeSharedState you ss =
    case ss of
        SharedGame g ->
            encodeGame g

        SharedChoice c ->
            encodeChoice you c


initialState : SharedState
initialState =
    SharedChoice Nobody


eventHandler : Event SharedState -> Msg
eventHandler event =
    case event of
        ServerError err ->
            Debug err

        UnknownError err ->
            Debug err

        EmptyState version ->
            GameUpdate version initialState

        NewState version state ->
            GameUpdate version state


handleGameUpdate : GameName -> VersionMarker -> SharedState -> Model -> ( Model, Cmd Msg )
handleGameUpdate name version newState model =
    case newState of
        SharedChoice c ->
            ( { model | state = Choosing name version c }, Cmd.none )

        SharedGame game ->
            case (markerOf model.id game.players) of
                Nothing ->
                    ( { model | debug = "This game is being played by two different people!" }, Cmd.none )

                Just m ->
                    ( { model | state = (Playing game.players m name version game.board) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( Noop, _ ) ->
            ( model, Cmd.none )

        ( Debug m, _ ) ->
            ( { model | debug = m }, Cmd.none )

        ( NameInput name, Naming _ ) ->
            ( { model | state = Naming name }, Cmd.none )

        ( SubmitName, Naming name ) ->
            ( { model | state = Submitted name }, connectToRoom wsURL model.id name )

        ( GameUpdate version newState, Submitted name ) ->
            handleGameUpdate name version newState model

        ( GameUpdate version newState, Choosing name _ _ ) ->
            handleGameUpdate name version newState model

        ( GameUpdate version newState, Playing _ _ name _ _ ) ->
            handleGameUpdate name version newState model

        ( Choose m, Choosing name version choice ) ->
            let
                sendState sharedState =
                    ( model, sendSharedState wsURL model.id name version (encodeSharedState model.id sharedState) )
            in
                case choice of
                    YouChose _ ->
                        ( { model | debug = "Invalid choice" }, Cmd.none )

                    TheyChose X them ->
                        case m of
                            X ->
                                ( { model | debug = "Invalid choice" }, Cmd.none )

                            O ->
                                sendState (SharedGame (Game emptyBoard (Players them model.id)))

                    TheyChose O them ->
                        case m of
                            X ->
                                sendState (SharedGame (Game emptyBoard (Players model.id them)))

                            O ->
                                ( { model | debug = "Invalid choice" }, Cmd.none )

                    Nobody ->
                        sendState (SharedChoice (YouChose m))

        ( Move row column, Playing players marker name version board ) ->
            let
                sendState sharedState =
                    ( model, sendSharedState wsURL model.id name version (encodeSharedState model.id sharedState) )
            in
                case (move row column marker board) of
                    Err errmessage ->
                        ( { model | debug = errmessage }, Cmd.none )

                    Ok newBoard ->
                        sendState (SharedGame (Game newBoard players))

        _ ->
            ( model, Cmd.none )


view model =
    Html.div [] <|
        [ Html.h1 [] [Html.text "Tic Tac Toe"]
        , case model.state of
            Naming s ->
                namingView s NameInput SubmitName

            Submitted s ->
                submittedView s

            Choosing name version choice ->
                choosingView name Choose choice

            Playing players marker name version board ->
                playingView players marker name version board Move
        , Html.div [] [ debugView model.debug ]
        ]


type alias Flags =
    { id : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { id = flags.id
      , state = Naming ""
      , debug = ""
      }
    , Cmd.none
    )


subscriptions model =
    register wsURL (sharedStateDecoder model.id) eventHandler


main =
    Html.App.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }
