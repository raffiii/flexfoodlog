module CorrelationChart exposing (..)

import Chart as C
import Chart.Attributes as CA
import Html exposing (Html)
import Html.Attributes as Attr
import Time 


view : Html msg
view =
    Html.div []
        [ Html.header [] [ Html.h2 [] [ Html.text "Correlation Chart" ] ]
        , Html.main_ [ Attr.style "padding" "2em" ] [ makeChart ]
        ]


makeChart : Html msg
makeChart =
    C.chart
        [ CA.height 300
        , CA.width 600
        ]
        [ C.xAxis []
        , C.xTicks [ CA.times <| Time.customZone 0 [] ]
        , C.xLabels []
        -- , C.yAxis []
        -- , C.yTicks []
        -- , C.yLabels []
        ]
