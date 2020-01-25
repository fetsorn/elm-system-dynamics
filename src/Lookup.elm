module Lookup exposing (..)

lookupList : List (Float, Float)
lookupList = [ (0.5, 10), (0.6, 6), (1, 5), (1.5,4), (2,1) ]

lookupTuples : List (Float, Float)
lookupTuples = [ (0.6, 6), (1, 5) ]

interpolate : Float -> Float -> Float -> Float -> Float -> Float
interpolate x1 y1 x3 y3 x2 =
    if x1 == x3 then
        y1
    else
        y1 + ( ( (x2-x1)*(y3-y1) ) / (x3-x1) )

eatTuples : ( (Float, Float), (Float, Float) ) -> Float -> Float
eatTuples tuples x =
    let
        xone =
            Tuple.first (Tuple.first tuples)

        yone =
            Tuple.second (Tuple.first tuples)

        xtri =
            Tuple.first (Tuple.second tuples)

        ytri =
            Tuple.second (Tuple.second tuples)
    in
    interpolate xone yone xtri ytri x

pickLowTuple : List (Float, Float) -> Float -> (Float, Float)
pickLowTuple list x =
    case (List.maximum (List.filter (\n -> Tuple.first n <= x) list) ) of
        Just val ->
            val

        Nothing ->
            (0,0)
--    (0,0)

pickHighTuple : List (Float, Float) -> Float -> (Float, Float)
pickHighTuple list x =
    case (List.minimum (List.filter (\n -> Tuple.first n >= x) list) ) of
        Just val ->
            val

        Nothing ->
            (0,0)
-- case tail of value value of nothing (0,0)

pickTuples : List (Float, Float) -> Float -> ((Float, Float), (Float, Float))
pickTuples list x =
    case ((List.maximum (List.filter (\n -> Tuple.first n <= x) list))
    ,(List.minimum (List.filter (\n -> Tuple.first n >= x) list))
    ) of
        (Just val, Just val1) ->
            (val, val1)

        (Just val, Nothing) ->
            (val, val)

        (Nothing, Just val) ->
            (val, val)

        _ ->
            ((0,0), (0,0))

lookup : List (Float, Float) -> Float -> Float
lookup list x =
    let
        tuples =
            case ((List.maximum (List.filter (\n -> Tuple.first n <= x) list))
                 ,(List.minimum (List.filter (\n -> Tuple.first n >= x) list))
                 ) of

                (Just val, Just val1) ->
                    (val, val1)

                (Just val, Nothing) ->
                    (val, val)

                (Nothing, Just val) ->
                    (val, val)

                _ ->
                    ((0,0), (0,0))

        xone =
            Tuple.first (Tuple.first tuples)

        yone =
            Tuple.second (Tuple.first tuples)

        xtri =
            Tuple.first (Tuple.second tuples)

        ytri =
            Tuple.second (Tuple.second tuples)
    in
        interpolate xone yone xtri ytri x
