module Dev3 exposing (..)

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html, div)
import Html.Attributes exposing (style, class)
import Task
import Time
import Css exposing (fontSize)
import Css.Global exposing (class, global)
import Html.Styled exposing (toUnstyled)

import ChartAB exposing (view)

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
    { visitorsPlot : List DoublePlot
    , balanceAPlot : List DoublePlot
    , balanceBPlot : List DoublePlot
    , incomePlot : List DoublePlot
    , profitPlot : List DoublePlot
    }

type alias DoublePlot =
    { time : Float
    , firstLine : Float
    , secondLine : Float
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
      , plots = { visitorsPlot = []
                , balanceAPlot = []
                , balanceBPlot = []
                , incomePlot = []
                , profitPlot = []
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
      , states = { paused = True
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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlotTick newTime ->
            ( { model | times = { plotTime = model.times.plotTime + 1, stockTime = model.times.stockTime },
                        plots = { visitorsPlot = updateVisitorsPlot model
                                , balanceAPlot = updateBalanceAPlot model
                                , balanceBPlot = updateBalanceBPlot model
                                , incomePlot = updateIncomePlot model
                                , profitPlot = updateProfitPlot model
                                }
              }
            , Cmd.none
            )

        StockTick newTime ->
            ( { model | times = { plotTime = model.times.plotTime, stockTime = model.times.stockTime + 1 },
                        stocks = { potentialStock = integral model model.stocks.potentialStock potentialFlowSum 0.001
                                 , regularAStock = integral model model.stocks.regularAStock regularAFlowSum 0.001
                                 , regularBStock = integral model model.stocks.regularBStock regularBFlowSum 0.001
                                 , loyalAStock = integral model model.stocks.loyalAStock loyalAFlowSum 0.001
                                 , loyalBStock = integral model model.stocks.loyalBStock loyalBFlowSum 0.001
                                 , profitAStock = integral model model.stocks.profitAStock profitAFlowSum 0.001
                                 , profitBStock = integral model model.stocks.profitBStock profitBFlowSum 0.001
                                 }
              }
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

-- FUNCTIONS
--
--- PLOTS

updateVisitorsPlot : Model -> List DoublePlot
updateVisitorsPlot model =
    List.append model.plots.visitorsPlot [{ time = model.times.plotTime
                                          , firstLine = model.stocks.regularAStock + model.stocks.loyalAStock
                                          , secondLine = model.stocks.regularBStock + model.stocks.loyalBStock
                                          }]

updateBalanceAPlot : Model -> List DoublePlot
updateBalanceAPlot model =
    List.append model.plots.balanceAPlot [{ time = model.times.plotTime
                                          , firstLine = revenueAFlow model
                                          , secondLine = costsAFlow model
                                          }]

updateBalanceBPlot : Model -> List DoublePlot
updateBalanceBPlot model =
    List.append model.plots.balanceBPlot [{ time = model.times.plotTime
                                          , firstLine = revenueBFlow model
                                          , secondLine = costsBFlow model
                                          }]

updateIncomePlot : Model -> List DoublePlot
updateIncomePlot model =
    List.append model.plots.incomePlot [{ time = model.times.plotTime
                                        , firstLine = revenueAFlow model   - costsAFlow model
                                        , secondLine = revenueBFlow model   - costsBFlow model
                                        }]

updateProfitPlot : Model -> List DoublePlot
updateProfitPlot model =
    List.append model.plots.profitPlot [{ time = model.times.plotTime
                                        , firstLine = model.stocks.profitAStock
                                        , secondLine = model.stocks.profitBStock
                                        }]

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
singleStoreCost = 100000

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
        (column []
             [ row []
                   [ html <| ChartAB.view model.plots.visitorsPlot
                   , html <| ChartAB.view model.plots.balanceAPlot
                   , html <| ChartAB.view model.plots.incomePlot
                   , html <| ChartAB.view model.plots.balanceBPlot
                   , html <| ChartAB.view model.plots.profitPlot
                   ]
             , row []
                   [ (controlsA model)
                   , (controlsB model)
                   ]
             ]
        )

controlsA : Model -> Element Msg
controlsA model =
    row []
        [ column []
              [ slider AdjustAdA model.parameters.adA 0.002 0.004
              , slider AdjustPriceA model.parameters.priceA 2 12
              ]
        , column []
              [ Input.checkbox []
                    { onChange = AutoStoreA
                    , icon = Input.defaultCheckbox
                    , checked = model.states.autoA
                    , label = Input.labelRight [] (text "auto stores")
                    }
              , text (String.fromFloat model.parameters.storesA)
              , Input.button [] { onPress = Just OpenStoreA, label = text "Open Store A" }
              ]
        ]

controlsB : Model -> Element Msg
controlsB model =
    row []
        [ column []
              [ slider AdjustAdB model.parameters.adB 0.002 0.004
              , slider AdjustPriceB model.parameters.priceB 2 12
              ]
        , column []
              [ Input.checkbox []
                    { onChange = AutoStoreB
                    , icon = Input.defaultCheckbox
                    , checked = model.states.autoB
                    , label = Input.labelRight [] (text "auto stores")
                    }
              , text (String.fromFloat model.parameters.storesB)
              , Input.button [] { onPress = Just OpenStoreB, label = text "Open Store B" }
              ]
        ]

slider : (Float -> Msg) -> Float -> Float -> Float -> Element Msg
slider msg v vmin vmax =
    Input.slider
        [ Element.height (Element.px 30)

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color (rgb 0 0.5 0)
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = msg
        , label = Input.labelAbove [] (text (String.fromFloat v))
        , min = vmin
        , max = vmax
        , step = Nothing
        , value = v
        , thumb = Input.defaultThumb
        }
