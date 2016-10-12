module Main exposing (..)

import Messages exposing (..)
import View exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (autofocus)
import Array exposing (Array)
import WebSocket
import Json.Encode
import Json.Decode exposing (..)
import Svg exposing (svg, rect, polyline, text, text')
import Svg.Attributes exposing (x, y, rx, ry, width, height, viewBox, style, points)

type Marker
  = X
  | O


type Msg
  = Noop
  | Click Int Int
  | ReceiveUpdate String Board
  | RoomNameInput String
  | SubmitRoomName
  | InitializeState String
  | Debug String


type alias Board =
  Array (Array (Maybe Marker))

type alias Model =
  { roomName : String
  , room : Maybe String
  , marker : Maybe Marker
  , stage : Maybe Board
  , version : String
  , board : Board
  , debug : String
  , id    : String
  }

setInBoard marker x y board =
  case Array.get y board of
    Nothing ->
      board

    Just row ->
      Array.set y (Array.set x (Just marker) row) board

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Noop ->
      ( model, Cmd.none )

    Click x y ->
      ( { model
          | board = setInBoard X x y model.board
        }
      , Cmd.none
      )

    RoomNameInput s ->
      ( { model | roomName = s }, Cmd.none )

    SubmitRoomName ->
      ( { model | room = Just model.roomName }, connectToRoom model.id model.roomName )

    ReceiveUpdate version board ->
      ( { model | board = board, version = version }, Cmd.none )

    InitializeState version ->
      ( model, case model.room of
        Nothing -> Cmd.none
        Just roomName -> ) )

    Debug string ->
      ( { model | debug = string }, Cmd.none )


emptyBoard : Board
emptyBoard =
  Array.repeat 3 <| Array.repeat 3 (Nothing)

model =
  { stage = Nothing
  , board = emptyBoard
  , room = Nothing
  , roomName = ""
  , marker = Just X
  , id = ""
  , version = ""
  , debug = ""
  }

type alias Flags = 
  { id : String }

init : Flags -> (Model, Cmd Msg)
init flags =
  ( {model | id = flags.id}, Cmd.none )


main =
  App.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


renderCell yindex xindex cell =
  let
    letter =
      case cell of
        Just X ->
          "x"

        Just O ->
          "o"

        Nothing ->
          ""

    leftCornerX =
      toString <| xindex * 100

    leftCornerY =
      toString <| yindex * 100

    centerX =
      toString <| xindex * 100 + 50

    centerY =
      toString <| yindex * 100 + 50
  in
    Svg.g
      []
      [ Svg.rect
        [ x leftCornerX
        , y leftCornerY
        , width "100"
        , height "100"
        , style "fill: transparent"
        , onClick (Click xindex yindex)
        ]
       []
      , Svg.text'
          [ x centerX
          , y centerY
          , Svg.Attributes.textAnchor "middle"
          , Svg.Attributes.alignmentBaseline "central"
          ]
          [ Svg.text letter ]
      ]


renderRow ycoord row =
  Array.toList <| Array.indexedMap (renderCell ycoord) row


renderBoard board =
  List.concat <| Array.toList <| Array.indexedMap renderRow board


outline =
  [ polyline
      [ points "100 0, 100, 300"
      , style "fill:none; stroke: black;"
      ]
      []
  , polyline
      [ points "200 0, 200, 300"
      , style "fill:none; stroke: black;"
      ]
      []
  , polyline
      [ points "0 100, 300, 100"
      , style "fill:none; stroke: black;"
      ]
      []
  , polyline
      [ points "0 200, 300, 200"
      , style "fill:none; stroke: black;"
      ]
      []
  ]


enterRoom model =
  Html.div
    []
    [ Html.span [] [ Html.text "Select a room name" ]
    , Html.input [ onInput RoomNameInput, autofocus True ] []
    , Html.button [ onClick SubmitRoomName ] [ Html.text "Connect" ]
    , Html.div [] [ Html.text model.debug ]
    ]


view model =
  Html.div []
    <| case model.room of
        Nothing ->
          [ enterRoom model ]

        Just _ ->
          [ svg [ width "400", height "400" ] <| List.concat [ outline, renderBoard model.board ]
          , Html.text model.debug
          ]

processMessage : String -> Msg
processMessage s =
  let decoded = decodeString responseDecoder s
  in
    case decoded of
      Err err -> Debug err
      Ok (ErrorResponse err) -> Debug err
      Ok (StateResponse version board) -> ReceiveUpdate version board
      Ok (EmptyStateResponse version) -> InitializeState version

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsURL processMessage
