module Dev3 exposing (..)

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Html exposing (Html, div)
import Html.Attributes exposing (style, class)
import Task
import Time
import Css exposing (fontSize)
import Css.Global exposing (class, global)
import Html.Styled exposing (toUnstyled)
import LineChart exposing (Config)
import LineChart.Colors as Colors
import LineChart.Junk as Junk
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Junk as Junk
import LineChart.Dots as Dots
import LineChart.Grid as Grid
import LineChart.Dots as Dots
import LineChart.Line as Line
import LineChart.Colors as Colors
import LineChart.Events as Events
import LineChart.Legends as Legends
import LineChart.Container as Container
import LineChart.Interpolation as Interpolation
import LineChart.Axis.Intersection as Intersection
import LineChart.Coordinate as Coordinate
import LineChart.Axis as Axis
import LineChart.Axis.Title as Title
import LineChart.Axis.Range as Range
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Ticks as Ticks

-- MAIN
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { stocks : Stocks
    , plots : Plots
    , parameters : Parameters
    , times : Times
    , states : States
    }

type alias Stocks =
    { potentialStock : Float
    , regularAStock : Float
    , regularBStock : Float
    , loyalAStock : Float
    , loyalBStock : Float
    , profitAStock : Float
    , profitBStock : Float
    }

type alias Plots =
    { visitorsPlot1 : List Point
    , visitorsPlot2 : List Point
    , balanceAPlot1 : List Point
    , balanceAPlot2 : List Point
    , balanceBPlot1 : List Point
    , balanceBPlot2 : List Point
    , incomePlot1 : List Point
    , incomePlot2 : List Point
    , profitPlot1 : List Point
    , profitPlot2 : List Point
    }

type alias Point =
    { x : Float
    , y : Float
    }

type alias Parameters =
    { priceA : Float
    , priceB : Float
    , adA : Float
    , adB : Float
    , storesA : Float
    , storesB : Float
    }

type alias Times =
    { plotTime : Float
    , stockTime : Float
    }

type alias States =
    { paused : Bool
    , autoA : Bool
    , autoB : Bool
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { stocks = { potentialStock = 50000
                 , regularAStock = 0
                 , regularBStock = 0
                 , loyalAStock = 0
                 , loyalBStock = 0
                 , profitAStock = 0
                 , profitBStock = 0
                 }
      , plots = { visitorsPlot1 = []
                , visitorsPlot2 = []
                , balanceAPlot1 = []
                , balanceAPlot2 = []
                , balanceBPlot1 = []
                , balanceBPlot2 = []
                , incomePlot1 = []
                , incomePlot2 = []
                , profitPlot1 = []
                , profitPlot2 = []
                }
      , parameters = { priceA = 6
                     , priceB = 6
                     , adA = 0.002
                     , adB = 0.002
                     , storesA = 1
                     , storesB = 1
                     }
      , times = { plotTime = 0
                , stockTime = 0
                }
      , states = { paused = False
                 , autoA = True
                 , autoB = True
                 }
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = PlotTick Time.Posix
    | StockTick Time.Posix
    | AdjustAdA Float
    | AdjustAdB Float
    | AdjustPriceA Float
    | AdjustPriceB Float
    | OpenStoreA
    | OpenStoreB
    | AutoStoreA Bool
    | AutoStoreB Bool
    | PauseModel Bool
    | RestartModel

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        PlotTick newTime ->
            case model.states.paused of

                False ->
                    ( { model | times = { plotTime = model.times.plotTime + 1, stockTime = model.times.stockTime },
                            plots = { visitorsPlot1 = updateVisitorsPlot1 model
                                    , visitorsPlot2 = updateVisitorsPlot2 model
                                    , balanceAPlot1 = updateBalanceAPlot1 model
                                    , balanceAPlot2 = updateBalanceAPlot2 model
                                    , balanceBPlot1 = updateBalanceBPlot1 model
                                    , balanceBPlot2 = updateBalanceBPlot2 model
                                    , incomePlot1 = updateIncomePlot1 model
                                    , incomePlot2 = updateIncomePlot2 model
                                    , profitPlot1 = updateProfitPlot1 model
                                    , profitPlot2 = updateProfitPlot2 model
                                    }
                      }
                    , Cmd.none
                    )

                True ->
                    ( model
                    , Cmd.none
                    )

        StockTick newTime ->
            case model.states.paused of

                False ->
                    ( { model | times = { plotTime = model.times.plotTime, stockTime = model.times.stockTime + 1 },
                            stocks = { potentialStock = integral model model.stocks.potentialStock potentialFlowSum 0.1
                                     , regularAStock = integral model model.stocks.regularAStock regularAFlowSum 0.1
                                     , regularBStock = integral model model.stocks.regularBStock regularBFlowSum 0.1
                                     , loyalAStock = integral model model.stocks.loyalAStock loyalAFlowSum 0.1
                                     , loyalBStock = integral model model.stocks.loyalBStock loyalBFlowSum 0.1
                                     , profitAStock = integral model model.stocks.profitAStock profitAFlowSum 0.1
                                     , profitBStock = integral model model.stocks.profitBStock profitBFlowSum 0.1
                                     }
                      }
                    , Cmd.batch [(if model.states.autoA then
                                      Task.succeed OpenStoreA
                                          |> Task.perform identity
                                  else
                                      Cmd.none)
                                ,(if model.states.autoB then
                                      Task.succeed OpenStoreB
                                          |> Task.perform identity
                                  else
                                      Cmd.none)
                                ]

                    )

                True ->
                    ( model
                    , Cmd.none
                    )

        AdjustAdA newAd ->
            let
                oldParameters = model.parameters
                newParameters = { oldParameters | adA = newAd }
            in
                ( { model | parameters = newParameters }
                , Cmd.none
                )

        AdjustAdB newAd ->
            let
                oldParameters = model.parameters
                newParameters = { oldParameters | adB = newAd }
            in
                ( { model | parameters = newParameters }
                , Cmd.none
                )

        AdjustPriceA newPrice ->
            let
                oldParameters = model.parameters
                newParameters = { oldParameters | priceA = newPrice }
            in
                ( { model | parameters = newParameters }
                , Cmd.none
                )

        AdjustPriceB newPrice ->
            let
                oldParameters = model.parameters
                newParameters = { oldParameters | priceB = newPrice }
            in
                ( { model | parameters = newParameters }
                , Cmd.none
                )

        OpenStoreA ->
            let
                oldParameters = model.parameters
                newParameters = { oldParameters | storesA = model.parameters.storesA + 1 }
                oldStocks = model.stocks
                newStocks = { oldStocks | profitAStock = model.stocks.profitAStock - singleStoreCost }
            in
                if model.stocks.profitAStock > singleStoreCost then
                    ( { model | parameters = newParameters, stocks = newStocks }
                    , Cmd.none
                    )
                else
                    ( model
                    , Cmd.none
                    )

        OpenStoreB ->
            let
                oldParameters = model.parameters
                newParameters = { oldParameters | storesB = model.parameters.storesB + 1 }
                oldStocks = model.stocks
                newStocks = { oldStocks | profitBStock = model.stocks.profitBStock - singleStoreCost }
            in
                if model.stocks.profitBStock > singleStoreCost then
                    ( { model | parameters = newParameters, stocks = newStocks }
                    , Cmd.none
                    )
                else
                    ( model
                    , Cmd.none
                    )

        AutoStoreA state ->
            let
                oldStates = model.states
                newStates = { oldStates | autoA = state }
            in
                ( { model | states = newStates }
                , Cmd.none
                )

        AutoStoreB state ->
            let
                oldStates = model.states
                newStates = { oldStates | autoB = state }
            in
                ( { model | states = newStates }
                , Cmd.none
                )

        PauseModel state ->
            let
                oldStates = model.states
                newStates = { oldStates | paused = state }
            in
                ( { model | states = newStates }
                , Cmd.none
                )

        RestartModel ->
             (init ())

-- FUNCTIONS
--
--- PLOTS

updateVisitorsPlot1 : Model -> List Point
updateVisitorsPlot1 model =
    List.append model.plots.visitorsPlot1 [ Point model.times.plotTime (model.stocks.regularAStock + model.stocks.loyalAStock) ]

updateVisitorsPlot2 : Model -> List Point
updateVisitorsPlot2 model =
    List.append model.plots.visitorsPlot2 [ Point model.times.plotTime (model.stocks.regularBStock + model.stocks.loyalBStock) ]

updateBalanceAPlot1 : Model -> List Point
updateBalanceAPlot1 model =
    List.append model.plots.balanceAPlot1 [ Point model.times.plotTime (revenueAFlow model) ]

updateBalanceAPlot2 : Model -> List Point
updateBalanceAPlot2 model =
    List.append model.plots.balanceAPlot2 [ Point model.times.plotTime (costsAFlow model)]

updateBalanceBPlot1 : Model -> List Point
updateBalanceBPlot1 model =
    List.append model.plots.balanceBPlot1 [ Point model.times.plotTime (revenueBFlow model) ]

updateBalanceBPlot2 : Model -> List Point
updateBalanceBPlot2 model =
    List.append model.plots.balanceBPlot2 [ Point model.times.plotTime (costsBFlow model) ]

updateIncomePlot1 : Model -> List Point
updateIncomePlot1 model =
    List.append model.plots.incomePlot1 [ Point model.times.plotTime (revenueAFlow model - costsAFlow model) ]

updateIncomePlot2 : Model -> List Point
updateIncomePlot2 model =
    List.append model.plots.incomePlot2 [ Point model.times.plotTime (revenueBFlow model - costsBFlow model) ]

updateProfitPlot1 : Model -> List Point
updateProfitPlot1 model =
    List.append model.plots.profitPlot1 [ Point model.times.plotTime model.stocks.profitAStock ]

updateProfitPlot2 : Model -> List Point
updateProfitPlot2 model =
    List.append model.plots.profitPlot2 [ Point model.times.plotTime model.stocks.profitBStock ]


--- STOCKS


potentialFlowSum : Model -> Float
potentialFlowSum model =
    -(adAFlow model) + -(adBFlow model) + (capacityLossAFlow model) + (capacityLossBFlow model)

regularAFlowSum : Model -> Float
regularAFlowSum model =
    (adAFlow model) + (chooseAFlow model) + -(loyalGainAFlow model) + -(capacityLossAFlow model)

regularBFlowSum : Model -> Float
regularBFlowSum model =
    (adBFlow model) + (chooseBFlow model) + -(loyalGainBFlow model) + -(capacityLossBFlow model)

loyalAFlowSum : Model -> Float
loyalAFlowSum model =
    (loyalGainAFlow model)

loyalBFlowSum : Model -> Float
loyalBFlowSum model =
    (loyalGainBFlow model)

profitAFlowSum : Model -> Float
profitAFlowSum model =
    (revenueAFlow model) + -(costsAFlow model)

profitBFlowSum : Model -> Float
profitBFlowSum model =
    (revenueBFlow model) + -(costsBFlow model)


--- FLOWS


adAFlow : Model -> Float
adAFlow model =
    model.parameters.adA * model.stocks.potentialStock

adBFlow : Model -> Float
adBFlow model =
    model.parameters.adB * model.stocks.potentialStock

capacityLossAFlow : Model -> Float
capacityLossAFlow model =
    if (model.stocks.regularAStock + model.stocks.loyalAStock) > (targetVisitorsA model) then
        if chooseAFlow model > 0 then
            adAFlow model + chooseAFlow model
        else
            adAFlow model
    else
        0

capacityLossBFlow : Model -> Float
capacityLossBFlow model =
    if (model.stocks.regularBStock + model.stocks.loyalBStock) > (targetVisitorsB model) then
        if chooseBFlow model > 0 then
            adBFlow model + chooseBFlow model
        else
            adBFlow model
    else
        0

chooseAFlow : Model -> Float
chooseAFlow model =
    let
        relativeRatingA = ratingA model / (ratingA model + ratingB model)
        regularSum = model.stocks.regularAStock + model.stocks.regularBStock
    in
        ( ( (relativeRatingA * regularSum) - model.stocks.regularAStock ) * chooseLossFraction )

chooseBFlow : Model -> Float
chooseBFlow model =
    let
        relativeRatingB = ratingB model / (ratingA model + ratingB model)
        regularSum = model.stocks.regularAStock + model.stocks.regularBStock
    in
        ( ( (relativeRatingB * regularSum) - model.stocks.regularBStock ) * chooseLossFraction )

loyalGainAFlow : Model -> Float
loyalGainAFlow model =
    model.stocks.regularAStock * loyalGainFraction

loyalGainBFlow : Model -> Float
loyalGainBFlow model =
    model.stocks.regularBStock * loyalGainFraction

revenueAFlow : Model -> Float
revenueAFlow model =
    (model.stocks.regularAStock + model.stocks.loyalAStock) * marginPerPersonA model

revenueBFlow : Model -> Float
revenueBFlow model =
    (model.stocks.regularBStock + model.stocks.loyalBStock) * marginPerPersonB model

costsAFlow : Model -> Float
costsAFlow model =
    adCostA model + rentCostA model

costsBFlow : Model -> Float
costsBFlow model =
    adCostB model + rentCostB model


--- VARIABLES


marginPerPersonA : Model -> Float
marginPerPersonA model =
    let
        primeCostA = model.parameters.priceA * primeCostFraction
    in
        model.parameters.priceA - primeCostA

marginPerPersonB : Model -> Float
marginPerPersonB model =
    let
        primeCostB = model.parameters.priceB * primeCostFraction
    in
        model.parameters.priceB - primeCostB

targetVisitorsA : Model -> Float
targetVisitorsA model =
    model.parameters.storesA * singleStoreVisitors

targetVisitorsB : Model -> Float
targetVisitorsB model =
    model.parameters.storesB * singleStoreVisitors

adCostA : Model -> Float
adCostA model =
    let
        singleAdCostA = model.parameters.priceA/2
    in
        adAFlow model * singleAdCostA

adCostB : Model -> Float
adCostB model =
    let
        singleAdCostB = model.parameters.priceB/2
    in
        adBFlow model * singleAdCostB

rentCostA : Model -> Float
rentCostA model =
    model.parameters.storesA * singleRentCost

rentCostB : Model -> Float
rentCostB model =
    model.parameters.storesB * singleRentCost

ratingA : Model -> Float
ratingA model =
    let
        relativePriceA = model.parameters.priceA / model.parameters.priceB
    in
        lookup ratingLookup relativePriceA

ratingB : Model -> Float
ratingB model =
    let
        relativePriceB = model.parameters.priceB / model.parameters.priceA
    in
        lookup ratingLookup relativePriceB


--- CONSTANTS


primeCostFraction : Float
primeCostFraction = 0.3

singleStoreVisitors : Float
singleStoreVisitors = 1000

loyalGainFraction : Float
loyalGainFraction = 0.001

singleRentCost : Float
singleRentCost = 3600

chooseLossFraction : Float
chooseLossFraction = 0.5

singleStoreCost : Float
singleStoreCost = 50000

ratingLookup : List (Float, Float)
ratingLookup = [ (0.5, 10), (0.6, 6), (1, 5), (1.5,4), (2,1) ]
--- LOOKUP


interpolate : Float -> Float -> Float -> Float -> Float -> Float
interpolate x1 y1 x3 y3 x2 =
    if x1 == x3 then
        y1
    else
        y1 + ( ( (x2-x1)*(y3-y1) ) / (x3-x1) )

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

--- INTEGRAL


integral : Model -> Float -> (Model -> Float) -> Float -> Float
integral model y flow step =
    y + ((1 / 6) * (k1 model flow step + (2 * k2 model flow step) + (2 * k3 model flow step) + k4 model flow step))

k1 : Model -> (Model -> Float) -> Float -> Float
k1 model flow step =
    step * flow model


k2 : Model -> (Model -> Float) -> Float -> Float
k2 model flow step =
    step * flow model


k3 : Model -> (Model -> Float) -> Float -> Float
k3 model flow step =
    step * flow model


k4 : Model -> (Model -> Float) -> Float -> Float
k4 model flow step =
    step * flow model

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 PlotTick
        , Time.every 1 StockTick
        ]

-- VIEW


view : Model -> Html Msg
view model =
    layout []
        (column [height fill]
             [ row [padding 10]
                   [ el [width fill] (html <| visitorsPlot model)
                   , el [width fill] (html <| balanceAPlot model)
                   , el [width fill] (html <| incomePlot model)
                   , el [width fill] (html <| balanceBPlot model)
                   , el [width fill] (html <| profitPlot model)
                   ]
             , row [alignBottom, width fill]
                   [ el [alignLeft, padding 10] (controlsA model)
                   , Input.button [] { onPress = Just RestartModel, label = text "Restart" }
                   , paragraph
                         [ centerX
                         , Font.center
                         , alignBottom
                         , padding 10
                         ]
                         [ text (String.concat [ "Week: "
                                              , (String.fromFloat model.times.plotTime)
                                              , "  Round: "
                                              , (String.fromInt (floor(model.times.plotTime/12)))
                                              ])
                         ]
                   , Input.checkbox [alignRight]
                           { onChange = PauseModel
                           , icon = Input.defaultCheckbox
                           , checked = model.states.paused
                           , label = Input.labelRight [] (text "pause model")
                           }
                   , el [alignRight, padding 10] (controlsB model)
                   ]
             ]
        )

visitorsPlot : Model -> Html msg
visitorsPlot model =
    LineChart.viewCustom visitorsConfig
        [ LineChart.line Colors.green Dots.none "visitors A" model.plots.visitorsPlot1
        , LineChart.line Colors.purple Dots.none "visitors B" model.plots.visitorsPlot2
        ]

visitorsConfig : Config Point msg
visitorsConfig =
  { y = Axis.default 400 "weeks" .y
  , x = visitorsAxisConfig
  , container = Container.responsive "line-chart-1"
  , interpolation = Interpolation.default
  , intersection = Intersection.default
  , legends = Legends.default
  , events = Events.default
  , junk = Junk.default
  , grid = Grid.default
  , area = Area.normal 0.1
  , line = Line.default
  , dots = Dots.default
  }

visitorsAxisConfig : Axis.Config Point msg
visitorsAxisConfig =
    Axis.custom
        { title = Title.default "People per week"
        , variable = Just << .x
        , pixels = 700
        , range = Range.window 0 120
        , axisLine = AxisLine.default
        , ticks = Ticks.int 2
        }

balanceAPlot : Model -> Html msg
balanceAPlot model =
    LineChart.viewCustom balanceConfig
        [ LineChart.line Colors.blue Dots.none "revenue A" model.plots.balanceAPlot1
        , LineChart.line Colors.rust Dots.none "expenses A" model.plots.balanceAPlot2
        ]

balanceBPlot : Model -> Html msg
balanceBPlot model =
    LineChart.viewCustom balanceConfig
        [ LineChart.line Colors.blue Dots.none "revenue B" model.plots.balanceBPlot1
        , LineChart.line Colors.rust Dots.none "expenses B" model.plots.balanceBPlot2
        ]

balanceConfig : Config Point msg
balanceConfig =
  { y = Axis.default 400 "weeks" .y
  , x = balanceAxisConfig
  , container = Container.responsive "line-chart-2"
  , interpolation = Interpolation.default
  , intersection = Intersection.default
  , legends = Legends.default
  , events = Events.default
  , junk = Junk.default
  , grid = Grid.default
  , area = Area.normal 0.2
  , line = Line.default
  , dots = Dots.default
  }

balanceAxisConfig : Axis.Config Point msg
balanceAxisConfig =
    Axis.custom
        { title = Title.default "Dollars per week"
        , variable = Just << .x
        , pixels = 700
        , range = Range.window 0 120
        , axisLine = AxisLine.default
        , ticks = Ticks.int 2
        }

incomePlot : Model -> Html msg
incomePlot model =
    LineChart.viewCustom incomeConfig
        [ LineChart.line Colors.green Dots.none "net income A" model.plots.incomePlot1
        , LineChart.line Colors.purple Dots.none "net income B" model.plots.incomePlot2
        ]

incomeConfig : Config Point msg
incomeConfig =
  { y = Axis.default 400 "weeks" .y
  , x = incomeAxisConfig
  , container = Container.responsive "line-chart-3"
  , interpolation = Interpolation.default
  , intersection = Intersection.default
  , legends = Legends.default
  , events = Events.default
  , junk = Junk.default
  , grid = Grid.default
  , area = Area.normal 0.1
  , line = Line.default
  , dots = Dots.default
  }

incomeAxisConfig : Axis.Config Point msg
incomeAxisConfig =
    Axis.custom
        { title = Title.default "Dollars per week"
        , variable = Just << .x
        , pixels = 700
        , range = Range.window 0 120
        , axisLine = AxisLine.default
        , ticks = Ticks.int 2
        }

profitPlot : Model -> Html msg
profitPlot model =
    LineChart.viewCustom profitConfig
        [ LineChart.line Colors.green Dots.none "profit A" model.plots.profitPlot1
        , LineChart.line Colors.purple Dots.none "profit B" model.plots.profitPlot2
        ]

profitConfig : Config Point msg
profitConfig =
  { y = Axis.default 400 "weeks" .y
  , x = profitAxisConfig
  , container = Container.responsive "line-chart-4"
  , interpolation = Interpolation.default
  , intersection = Intersection.default
  , legends = Legends.default
  , events = Events.default
  , junk = Junk.default
  , grid = Grid.default
  , area = Area.normal 0.1
  , line = Line.default
  , dots = Dots.default
  }

profitAxisConfig : Axis.Config Point msg
profitAxisConfig =
    Axis.custom
        { title = Title.default "Dollars per week"
        , variable = Just << .x
        , pixels = 700
        , range = Range.window 0 120
        , axisLine = AxisLine.default
        , ticks = Ticks.int 2
        }

controlsA : Model -> Element Msg
controlsA model =
    row [width (px 400), Font.size 16, Background.color (rgba255 96 173 118 0.2), Border.rounded 10]
        [ column [alignLeft, width (px 200), padding 10]
              [ paragraph [Font.center] [text "Ad A"]
              , sliderFloat AdjustAdA model.parameters.adA 0.002 0.004
              , paragraph [Font.center] [text "Price A"]
              , sliderInt AdjustPriceA model.parameters.priceA 2 12
              ]
        , column [alignRight, spacing 10, padding 10]
              [ Input.checkbox []
                    { onChange = AutoStoreA
                    , icon = Input.defaultCheckbox
                    , checked = model.states.autoA
                    , label = Input.labelRight [] (text "auto stores")
                    }
              , paragraph
                    [ Font.center
                    , Background.color (rgba255 255 255 255 1)
                    , width (px 20)
                    , centerX
                    , Border.rounded 10]
                    [ text (String.fromFloat model.parameters.storesA) ]
              , Input.button [Background.color (rgba 204 240 255 0.5), padding 10] { onPress = Just OpenStoreA, label = text "Open Store A" }
              ]
        ]

controlsB : Model -> Element Msg
controlsB model =
    row [width (px 400), Font.size 15, Background.color (rgba255 173 96 151 0.2), Border.rounded 10]
        [ column [alignLeft, width (px 200), padding 10]
              [ paragraph [Font.center] [text "Ad B"]
              , sliderFloat AdjustAdB model.parameters.adB 0.002 0.004
              , paragraph [Font.center] [text "Price B"]
              , sliderInt AdjustPriceB model.parameters.priceB 2 12
              ]
        , column [alignRight, spacing 10, padding 10]
              [ Input.checkbox []
                    { onChange = AutoStoreB
                    , icon = Input.defaultCheckbox
                    , checked = model.states.autoB
                    , label = Input.labelRight [] (text "auto stores")
                    }
              , paragraph
                    [ Font.center
                    , Background.color (rgba255 255 255 255 1)
                    , width (px 20)
                    , centerX
                    , Border.rounded 10]
                    [ text (String.fromFloat model.parameters.storesB) ]
              , Input.button [Background.color (rgba 204 240 255 0.5), padding 10] { onPress = Just OpenStoreB, label = text "Open Store B" }
              ]
        ]

sliderInt : (Float -> Msg) -> Float -> Float -> Float -> Element Msg
sliderInt msg v vmin vmax =
    Input.slider
        [ Element.height (Element.px 30)

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color (rgb255 96 151 173)
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = msg
        , label = Input.labelAbove [] (text (String.fromInt (floor v)))
        , min = vmin
        , max = vmax
        , step = Nothing
        , value = v
        , thumb = Input.defaultThumb
        }

sliderFloat : (Float -> Msg) -> Float -> Float -> Float -> Element Msg
sliderFloat msg v vmin vmax =
    Input.slider
        [ Element.height (Element.px 30)

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color (rgb255 96 151 173)
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = msg
        , label = Input.labelAbove [] (text (String.concat [ (String.fromInt (round(v*1000)))
                                                           , "%"
                                                           ]))
        , min = vmin
        , max = vmax
        , step = Nothing
        , value = v
        , thumb = Input.defaultThumb
        }
