module SharedState exposing (register, connectToRoom, sendSharedState, Event(..))

import WebSocket
import Json.Decode exposing (..)
import Json.Encode


type Event a
    = ServerError String
    | UnknownError String
    | EmptyState String
    | NewState String a


register : String -> Decoder a -> (Event a -> b) -> Sub b
register wsURL decoder eventHandler =
    let
        eventDecoder =
            responseDecoder decoder

        decodeEvent s =
            case decodeString eventDecoder s of
                Ok e ->
                    e

                Err s ->
                    UnknownError s
    in
        WebSocket.listen wsURL (decodeEvent >> eventHandler)


connectToRoom wsURL id roomName =
    WebSocket.send wsURL <|
        Json.Encode.encode 0 <|
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "roomName", Json.Encode.string roomName )
                ]


responseDecoder : Decoder a -> Decoder (Event a)
responseDecoder decoder =
    oneOf
        [ errorResponseDecoder
        , emptyStateResponseDecoder
        , stateResponseDecoder decoder
        , fail "Unexpected message from the shared state server"
        ]


errorResponseDecoder : Decoder (Event a)
errorResponseDecoder =
    customDecoder ("error" := string) (Ok << ServerError)


stateResponseDecoder : Decoder a -> Decoder (Event a)
stateResponseDecoder decoder =
    "data" := (object2 (NewState) ("version" := string) ("state" := decoder))


emptyStateResponseDecoder : Decoder (Event a)
emptyStateResponseDecoder =
    "data" := (object2 (\x _ -> EmptyState x) ("version" := string) ("state" := null Nothing))


sendSharedState : String -> String -> String -> String -> Json.Encode.Value -> Cmd a
sendSharedState wsURL id roomName version encodedState =
    WebSocket.send wsURL <|
        Json.Encode.encode 0 <|
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "version", Json.Encode.string version )
                , ( "roomName", Json.Encode.string roomName )
                , ( "update", encodedState )
                ]
