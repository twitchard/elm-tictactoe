module TicTacToe exposing (Marker(X, O), Board, Choice(Nobody, TheyChose, YouChose), Game, Players, markerOf, boardDecoder, choiceDecoder, gameDecoder, encodeGame, encodeChoice, emptyBoard, move, markerString)

import Json.Decode exposing (..)
import Json.Encode
import Array exposing (Array)


type Marker
    = X
    | O



type Choice
    = Nobody
    | TheyChose Marker String
    | YouChose Marker

type alias MaybeChoice =
    { x : Maybe String
    , o : Maybe String
    }

encodeChoice : String -> Choice -> Json.Encode.Value
encodeChoice you choice =
    Json.Encode.object <|
        case choice of
            Nobody ->
                [ ( markerString X, Json.Encode.null )
                , ( markerString O, Json.Encode.null )
                ]

            TheyChose marker them ->
                [ ( markerString marker, Json.Encode.string them )
                , ( markerString <| opposite marker, Json.Encode.null )
                ]

            YouChose marker ->
                [ ( markerString marker, Json.Encode.string you )
                , ( markerString <| opposite marker, Json.Encode.null )
                ]


type alias Board =
    Array (Array (Maybe Marker))


opposite : Marker -> Marker
opposite m =
    case m of
        X ->
            O

        O ->
            X


markerString : Marker -> String
markerString m =
    case m of
        X ->
            "x"

        O ->
            "o"


encodeMarker : Marker -> Json.Encode.Value
encodeMarker =
    Json.Encode.string << markerString


encodeGame : Game -> Json.Encode.Value
encodeGame game =
    let
        markerValue v =
            case v of
                Just X ->
                    Json.Encode.string "x"

                Just O ->
                    Json.Encode.string "o"

                Nothing ->
                    Json.Encode.null

        boardValue =
            game.board |> Array.map (Array.map markerValue) |> Array.map Json.Encode.array |> Json.Encode.array

        playersValue =
            Json.Encode.object
                [ ( "x", Json.Encode.string game.players.x )
                , ( "o", Json.Encode.string game.players.o )
                ]
    in
        Json.Encode.object
            [ ( "board", boardValue )
            , ( "players", playersValue )
            ]


emptyBoard =
    (Array.repeat 3 (Array.repeat 3 Nothing))


type alias Players =
    { x : String
    , o : String
    }


type alias Game =
    { board : Board
    , players : Players
    }


markerOf : String -> Players -> Maybe Marker
markerOf id players =
    if players.x == id then
        Just X
    else if players.o == id then
        Just O
    else
        Nothing


sum : Array Int -> Int
sum arrayOfInts = Array.foldl (+) 0 arrayOfInts

countInArray : a -> Array a -> Int
countInArray what = Array.foldl (\x n -> if x == what then n + 1 else n) 0

turn : Board -> Marker 
turn board = 
  let 
    countX = sum <| Array.map (countInArray (Just X)) board
    countO = sum <| Array.map (countInArray (Just O)) board
  in
    if countX >= countO then X else O

move : Int -> Int -> Marker -> Board -> Result String Board
move x y marker board =
  let maybeRow = Array.get y board
      whoseTurn = turn board
  in
    if not (whoseTurn == marker) then Err "It is not your turn" else
      case maybeRow of 
        Nothing -> Err "There was no such row"
        Just row ->
          let maybeCell = Array.get x row in
            case maybeCell of
              Nothing -> Err "There was no such column"
              Just (Just _) -> Err "There was already a piece there"
              Just Nothing -> Ok <|
                Array.set y (Array.set x (Just marker) row) board


decodeMarker : String -> Result String Marker
decodeMarker s =
    if s == markerString X then
        Ok X
    else if s == markerString O then
        Ok O
    else
        Err "Invalid Marker"


boardDecoder : Decoder Board
boardDecoder =
    array (array (nullOr <| customDecoder string decodeMarker))


maybeChoiceDecoder : Decoder MaybeChoice
maybeChoiceDecoder =
    object2 (MaybeChoice) ("x" := nullOr string) ("o" := nullOr string)


decodeChoice : String -> MaybeChoice -> Result String Choice
decodeChoice you c =
    case ( c.x, c.o ) of
        ( Just _, Just _ ) ->
            Err "No choice left!"

        ( Nothing, Nothing ) ->
            Ok Nobody

        ( Just x, Nothing ) ->
            Ok <|
                if x == you then
                    YouChose X
                else
                    TheyChose X x

        ( Nothing, Just o ) ->
            Ok <|
                if o == you then
                    YouChose O
                else
                    TheyChose O o


choiceDecoder : String -> Decoder Choice
choiceDecoder you =
    customDecoder maybeChoiceDecoder (decodeChoice you)


playersDecoder : Decoder Players
playersDecoder =
    object2 (Players) ("x" := string) ("o" := string)


gameDecoder : Decoder Game
gameDecoder =
    object2 (Game) ("board" := boardDecoder) ("players" := playersDecoder)


nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    oneOf
        [ null Nothing
        , map Just decoder
        ]
