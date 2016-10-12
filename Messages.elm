module Messages exposing (..)

wsURL =
  "ws://localhost:8080"

sendInitialState id roomName version =
  WebSocket.send wsURL <|
    Json.Encode.encode 0
      ( Json.Encode.object 
          [ ("version", Json.Encode.string version)
          , ("id", Json.Encode.string id)
          , ("roomName", Json.Encode.string roomName)
          , ("update", Json.Encode.array (Array.repeat 3 (Json.Encode.array (Array.repeat 3 Json.Encode.null))))
          ]
      )

connectToRoom id roomName =
  WebSocket.send wsURL <|
    JsonEncode.encode 0 <| 
      Json.Encode.object
        [ ("id", Json.Encode.string model.id)
        , ("roomName", Json.Encode.string model.roomName)
        ]

type Response = ErrorResponse String | StateResponse String Board | EmptyStateResponse String

responseDecoder : Decoder Response
responseDecoder = 
  oneOf
    [ errorResponseDecoder
    , stateResponseDecoder
    , emptyStateResponseDecoder
    , fail "Unexpected message from the server"
    ]

nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    oneOf
    [ null Nothing
    , map Just decoder
    ]

errorResponseDecoder : Decoder Response
errorResponseDecoder = 
  customDecoder ("error" := string) (Ok << ErrorResponse)

decodeMarker : String -> Result String Marker
decodeMarker s = if s == "X" then Ok X else if s == "O" then Ok O else Err "Invalid Marker"

stateDecoder : Decoder Board
stateDecoder = array (array (nullOr <| customDecoder string decodeMarker))

stateResponseDecoder : Decoder Response
stateResponseDecoder =
  "data" := (object2 (StateResponse) ("version" := string) ("state" := stateDecoder))

emptyStateResponseDecoder : Decoder Response
emptyStateResponseDecoder = 
  "data" := (object2 (\x _ -> EmptyStateResponse x) ("version" := string) ("state" := null Nothing))
