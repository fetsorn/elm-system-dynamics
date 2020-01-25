module ChartAB exposing (view)

import Axis
import Color exposing (Color)
import Path exposing (Path)
import Scale exposing (ContinuousScale, OrdinalScale)
import Scale.Color
import Shape
import Statistics
import Time
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fill, fontFamily, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, strokeWidth, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), em)


-- MAIN

-- MODEL

type alias DoublePlot =
    { time : Float
    , firstLine : Float
    , secondLine : Float
    }

-- UPDATE


-- SUBSCRIPTIONS


-- VIEW


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


series =
    [ { label = "A"
      , accessor = .firstLine
      }
    , { label = "B"
      , accessor = .secondLine
      }
    ]


accessors : List (DoublePlot -> Float)
accessors =
    List.map .accessor series


values : DoublePlot -> List Float
values i =
    List.map (\a -> a i) accessors


colorScale : OrdinalScale String Color
colorScale =
    List.map .label series
        |> Scale.ordinal [(Color.rgba 1 0 0 0.3)
                         ,(Color.rgba 0 1 0 0.3)
                         ,(Color.rgba 0 0 1 0.3)
                         ]


color : String -> Color
color =
    Scale.convert colorScale >> Maybe.withDefault Color.black



view : List DoublePlot -> Svg msg
view model =
    let
        last =
            List.reverse model
                |> List.head
                |> Maybe.withDefault (DoublePlot 0 0 0 )

        first =
            List.head model
                |> Maybe.withDefault (DoublePlot 0 0 0 )

        xScale : ContinuousScale Float
        xScale =
            model
                |> List.map (.time)
                |> Statistics.extent
                |> Maybe.withDefault (0,0)
                |> Scale.linear ( 0, w - 2 * padding )

        yScale : ContinuousScale Float
        yScale =
            model
                |> List.map (values >> List.maximum >> Maybe.withDefault 0)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\b -> ( 0, b ))
                |> Scale.linear ( h - 2 * padding, 0 )
                |> Scale.nice 4

        lineGenerator : ( Float, Float ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale (x), Scale.convert yScale (y) )

        areaGenerator : ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        areaGenerator ( x, y ) =
            Just
                ( ( Scale.convert xScale (x), Tuple.first (Scale.rangeExtent yScale) )
                , ( Scale.convert xScale (x), Scale.convert yScale (y) )
                )

        line : (DoublePlot -> Float) -> Path
        line accessor =
            List.map (\i -> ( .time i, accessor i )) model
                |> List.map lineGenerator
                |> Shape.line Shape.monotoneInXCurve

        area : (DoublePlot -> Float) -> Path
        area accessor =
            List.map (\i -> ( .time i, accessor i )) model
                |> List.map areaGenerator
                |> Shape.area Shape.monotoneInXCurve
    in
    svg [ viewBox 0 0 w h ]
        [ g [ class [ "tata" ], fontSize 40, fontFamily [ "sans-serif" ], transform [ Translate (padding - 1) (h - padding) ] ]
            [ Axis.bottom [ Axis.tickCount 3 ] xScale ]
        , g [ class [ "dada" ], fontSize 40, transform [ Translate (padding - 1) padding ] ]
            [ Axis.left [ Axis.tickCount 3 ] yScale
            ]
        , g [ fontSize 20,  transform [ Translate padding padding ], class [ "series" ] ]
            (List.map
                (\{ accessor, label } ->
                    Path.element (line accessor)
                        [ stroke (color label)
                        , strokeWidth 3
                        , fill FillNone
                        ]
                )
                series
            )
        , g [ fontSize 20, transform [ Translate padding padding ], class [ "series" ] ]
            (List.map
                (\{ accessor, label } ->
                    Path.element (area accessor) [ strokeWidth 3, fill <| Fill <| (color label) ]
                )
                series
            )
        , g [ fontFamily [ "sans-serif" ], fontSize 20 ]
            (List.map
                (\{ accessor, label } ->
                    g
                        [ transform
                            [ Translate (w - padding + 10) (padding + Scale.convert yScale (accessor last))
                            ]
                        ]
                        [ text_ [ fill (Fill (color label)) ] [ text label ] ]
                )
                series
            )
        ]
