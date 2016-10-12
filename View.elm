module View exposing (..)
import Svg.Attributes exposing (x, y, rx, ry, width, height, viewBox, style, points)
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
