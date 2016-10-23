module PlayingView exposing (..)

import Html
import TicTacToe exposing (Marker(X, O))
import Svg exposing (polyline, svg)
import Svg.Attributes exposing (x, y, width, height, style, points)
import Svg.Events exposing (onClick)
import Array

playingView players marker name version board click =
  svg [ width "400", height "400" ] <| List.concat [outline, renderBoard click board]

renderCell click yindex xindex cell =
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
        , onClick (click xindex yindex)
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


renderRow click ycoord row =
  Array.toList <| Array.indexedMap (renderCell click ycoord) row


renderBoard click board =
  List.concat <| Array.toList <| Array.indexedMap (renderRow click) board


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
