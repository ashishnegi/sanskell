module Main exposing (main)

import Html exposing (Html, div, text, button, br, a, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, class, href, value)
import Html.App as App
import Api as Api
import Task
import Http
import Dict exposing (Dict)
import Time
import Set exposing (Set)
import Random
import Svg as Svg
import Svg.Attributes as SvgA
import String as String

type alias URL = String

type StatusMsg = NoStatus
               | TextMessage String
               | ErrorMessage String
               | URLMessage String URL

type alias WordCloud = Dict String (Count, Weight, Position)
type alias SpiralParams = { initial : (Float, Float)
                          , deltas  : (Float, Float)
                          , acceleration : (Float, Float)
                          }

type alias Model = { websiteUrl : URL
                   , statusMessage : StatusMsg
                   , wordCounts : Dict Api.JobId Api.JobResult
                   , pendingRequests : Set Api.JobId
                   , spiralParams : SpiralParams
                   }

type Msg = WebsiteInput URL
         | PostJob
         | PostJobSuccess Api.JobId
         | PostJobFailed Http.Error
         | StatusJobSuccess Api.JobStatus
         | StatusJobFailed Http.Error
         | JobResultSuccess Api.JobResult
         | JobResultFailed Http.Error
         | CheckJobStatus
         | ChangeInitialR String
         | ChangeInitialTheta String
         | ChangeDeltaR String
         | ChangeDeltaTheta String
         | ChangeAccelerationR String
         | ChangeAccelerationTheta String

type alias Dimension =
    { width : Int
    , height : Int
    }

type alias Count = Float
type alias Weight = Float
type alias Position =
    { x : Int
    , y : Int
    }

type alias Flags =
    { jobId : Maybe Int }

main =
    App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Flags -> ( Model, Cmd Msg)
init flags = ( Model "" NoStatus Dict.empty Set.empty (SpiralParams (1, 1) (1.0, 10.0) (-0.01, -0.01)), cmdFromFlags flags )

cmdFromFlags : Flags -> Cmd Msg
cmdFromFlags flags =
    flags.jobId
        |> Maybe.map ( \ jobId -> Task.perform StatusJobFailed StatusJobSuccess (Api.getJobStatusById jobId))
        |> Maybe.withDefault Cmd.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        WebsiteInput url ->
            ( { model | websiteUrl = url }, Cmd.none )

        PostJob ->
            ( model
            , Task.perform PostJobFailed PostJobSuccess (Api.postJob (Api.JobPostBody model.websiteUrl)))
        PostJobSuccess jobId ->
            ( { model | statusMessage = TextMessage "Job successfully received.. It will take some time to process it." }
            , Task.perform StatusJobFailed StatusJobSuccess (Api.getJobStatusById jobId))
        PostJobFailed error ->
            ( { model | statusMessage = errorMsg error "Failed to post job. Please try after some time." }, Cmd.none )

        StatusJobSuccess jobStatus ->
            let (msg, cmd, pending) =
                    case jobStatus.jobState of
                        Api.Pending  -> ( URLMessage "Still working on your job: " jobStatus.jobResult.message, Cmd.none, True )
                        Api.Finished -> ( URLMessage "You can view the word cloud at: " jobStatus.jobResult.message
                                        , Task.perform JobResultFailed JobResultSuccess (Api.getJobById jobStatus.statusJobId )
                                        , False )
                        Api.Failed   -> ( ErrorMessage jobStatus.jobResult.message, Cmd.none, False )
                pendingRequests = if pending
                                  then Set.insert jobStatus.statusJobId model.pendingRequests
                                  else Set.remove jobStatus.statusJobId model.pendingRequests
            in ( { model | statusMessage = msg, pendingRequests = pendingRequests }, cmd )
        StatusJobFailed error ->
            ( { model | statusMessage = errorMsg error "Failed to get job status. Please try again." }, Cmd.none )

        JobResultSuccess jobResult ->
            ( { model | wordCounts = Dict.insert jobResult.resultJobId jobResult model.wordCounts }
            , Cmd.none)
        JobResultFailed error ->
            ( { model | statusMessage = errorMsg error "Failed to get data. Please try again." }, Cmd.none )

        CheckJobStatus ->
            ( model
            , model.pendingRequests
            |> Set.toList
            |> List.map (\ jobId -> Task.perform StatusJobFailed StatusJobSuccess (Api.getJobStatusById jobId))
            |> Cmd.batch )

        ChangeInitialR iStr ->
            case String.toFloat iStr of
                Ok i ->
                    let (x,y) = model.spiralParams.initial
                        mSpiralParams = model.spiralParams
                    in ( { model | spiralParams = { mSpiralParams | initial = (i, y) }}, Cmd.none)
                Err err -> ( model, Cmd.none )
        ChangeInitialTheta iStr ->
            case String.toFloat iStr of
                Ok i ->
                    let (x,y) = model.spiralParams.initial
                        mSpiralParams = model.spiralParams
                    in ( { model | spiralParams = { mSpiralParams | initial = (x, i) }}, Cmd.none)
                Err err -> ( model, Cmd.none )

        ChangeDeltaR iStr ->
            case String.toFloat iStr of
                Ok i ->
                    let (x,y) = model.spiralParams.deltas
                        mSpiralParams = model.spiralParams
                    in ( { model | spiralParams = { mSpiralParams | deltas = (i, y) }}, Cmd.none)
                Err err -> ( model, Cmd.none )
        ChangeDeltaTheta iStr ->
            case String.toFloat iStr of
                Ok i ->
                    let (x,y) = model.spiralParams.deltas
                        mSpiralParams = model.spiralParams
                    in ( { model | spiralParams = { mSpiralParams | deltas = (x, i) }}, Cmd.none)
                Err err -> ( model, Cmd.none )

        ChangeAccelerationR iStr ->
            case String.toFloat iStr of
                Ok i ->
                    let (x,y) = model.spiralParams.acceleration
                        mSpiralParams = model.spiralParams
                    in ( { model | spiralParams = { mSpiralParams | acceleration = (i, y) }}, Cmd.none)
                Err err -> ( model, Cmd.none )
        ChangeAccelerationTheta iStr ->
            case String.toFloat iStr of
                Ok i ->
                    let (x,y) = model.spiralParams.acceleration
                        mSpiralParams = model.spiralParams
                    in ( { model | spiralParams = { mSpiralParams | acceleration = (x, i) }}, Cmd.none)
                Err err -> ( model, Cmd.none )

errorMsg : Http.Error -> String -> StatusMsg
errorMsg error defaultMsg =
    case error of
        Http.BadResponse _ serverMsg -> ErrorMessage (defaultMsg ++ " : " ++ serverMsg)
        _                            -> ErrorMessage defaultMsg

view : Model -> Html Msg
view model =
    div []
        (List.concat
             [ [ div [ class "sanskell-heading" ]
                     [ text "Sanskell" ]]
             , [ div [ class "url-input" ]
                     [ Html.input [ onInput WebsiteInput
                                  , placeholder "any sanskrit website name" ]
                           []]
               , button [ class "send-job-btn"
                        , onClick PostJob ]
                     [ text "Make word cloud" ]
               -- , text ( toString model.spiralParams )
               ]
             , [ showStatusMsg model.statusMessage ]
             -- buttons to play with word cloud
             , spiralButtons model
             -- word cloud
             , (List.map wordCloud (List.map (\jobResult -> wordSpiralPositions jobResult model.spiralParams)
                                        (Dict.values model.wordCounts)))
             ])

spiralButtons : Model -> List (Html Msg)
spiralButtons model =
    let (r, t) = model.spiralParams.initial
        (dr, dt) = model.spiralParams.deltas
        (ddr, ddt) = model.spiralParams.acceleration
    in if Dict.isEmpty model.wordCounts
       then []
       else [ div [ class "input-boxes"]
                  [ text "Changes values to play with word cloud :"
                  , div []
                      [ text "r : "
                      , input [ onInput ChangeInitialR
                              , placeholder (toString r)
                              , value (toString r)
                              ]
                            []]

                  , div []
                      [ text "θ : "
                      , input [ onInput ChangeInitialTheta
                              , placeholder (toString t)
                              , value (toString t)
                              ]
                            []]

                  , div []
                      [ text "Δr : "
                      , input [ onInput ChangeDeltaR
                              , placeholder (toString dr)
                              , value (toString dr)
                              ]
                            []]
                  , div []
                      [ text "Δθ : "
                      , input [ onInput ChangeDeltaTheta
                              , placeholder (toString dt)
                              , value (toString dt)
                              ]
                            []]

                  , div []
                      [ text "ΔΔr : "
                      , input [ onInput ChangeAccelerationR
                              , placeholder (toString ddr)
                              , value (toString ddr)
                      ]
                      []]
                  , div []
                      [ text "ΔΔθ : "
                      , input [ onInput ChangeAccelerationTheta
                              , placeholder (toString ddt)
                              , value (toString ddt)
                              ]
                      []]
                  ]]

showStatusMsg : StatusMsg -> Html Msg
showStatusMsg statusMsg =
    case statusMsg of
        NoStatus -> div [] []
        TextMessage msg -> div [ class "status-msg" ]
                               [ text msg ]
        ErrorMessage msg -> div [ class "status-msg error-msg"]
                                [ text msg ]
        URLMessage msg url -> div [ class "status-msg"]
                                  [ text msg
                                  , a [ class "url-msg"
                                      , href url
                                      ]
                                      [ text url ]
                                  ]

timeSubs : Model -> Sub Msg
timeSubs model =
    if Set.isEmpty model.pendingRequests
    then Sub.none
    else Time.every (2 * Time.second) ( \ _ -> CheckJobStatus )

subscriptions : Model -> Sub Msg
subscriptions model =
    timeSubs model

svgDimension : WordCloud -> Dimension
svgDimension wordCloud =
    let positions = wordCloud
                    |> Dict.values
                    |> List.map (\ (_,_,p) -> (p.x, p.y))
        allXs = List.map (\ (x,y) -> x) positions
        maxX = Maybe.withDefault 0 (List.maximum allXs)
        minX = Maybe.withDefault 0 (List.minimum allXs)
        allYs = List.map (\ (x,y) -> y) positions
        maxY = Maybe.withDefault 0 (List.maximum allYs)
        minY = Maybe.withDefault 0 (List.minimum allYs)
        width = abs(maxX - minX)
        height = abs(maxY - minY)
    in Dimension (round width + 1) (round height + 1)

wordCloud : WordCloud -> Html Msg
wordCloud wordsCount =
    let dimension = svgDimension wordsCount
    in wordsCount
        |> Dict.toList
        |> List.map ( \ (name, (c, w, pos)) ->
                          Svg.g [ ]
                                [ Svg.text'
                                      [ SvgA.fill "red"
                                      , SvgA.x (toString pos.x)
                                      , SvgA.y (toString pos.y)
                                      , SvgA.fontSize (toString w)
                                      ]
                                      [ Svg.text name ]
                                ])
        |> Svg.svg [ dimension.width |> toString |> SvgA.width
                   , SvgA.height (toString dimension.height)
                   ]

wordSpiralPositions : Api.JobResult -> SpiralParams -> WordCloud
wordSpiralPositions jobResult { initial, deltas, acceleration } =
    let (initialR, initialTheta) = initial
        (deltaR, deltaTheta) = deltas
        (accR, accTheta) = acceleration
        positions = [1 .. Dict.size jobResult.wordsCount]
                  |> List.scanl (\ _ (r, t) ->
                                     let newR = r + (deltaR * (1 + accR))
                                         newTheta = t + (deltaTheta * (1 + accTheta))
                                     in (newR, newTheta))
                     (initialR, initialTheta)
                  |> List.map (\ (r, t) -> (r * cos t, r * sin t))
        allXs = List.map (\ (x,y) -> x) positions
        maxX = Maybe.withDefault 0 (List.maximum allXs)
        minX = Maybe.withDefault 0 (List.minimum allXs)
        allYs = List.map (\ (x,y) -> y) positions
        maxY = Maybe.withDefault 0 (List.maximum allYs)
        minY = Maybe.withDefault 0 (List.minimum allYs)
        width = abs(maxX - minX)
        height = abs(maxY - minY)
    in positions
        |> List.map (\ (x, y) -> (x + width/2 , y + height/2))
        |> List.map (\ (x,y) -> Position (round x) (round y))
        |> makeWordCloud jobResult

makeWordCloud : Api.JobResult -> List Position -> WordCloud
makeWordCloud jobResult positions =
    case positions of
        [] -> Dict.empty
        otherwise ->
            let weights = jobResult.wordsCount
                        |> Dict.values
                        |> List.sort
                        |> Debug.log "sorted weights : "
                avgWeight = List.sum weights / toFloat (List.length weights) |> Debug.log "avgWeight: "
                minWeight = List.minimum weights |> Maybe.withDefault 0 |> Debug.log "minWeight: "
                diffWeight = avgWeight - minWeight + 1
                scale     = (\x -> (x - minWeight) / diffWeight) >> (*) 10 >> (+) 15 >> min 20
            in positions
                |> List.map2 ( \ (name, count) pos -> (name, (count, scale count, pos)) )
                             (Dict.toList jobResult.wordsCount)
                |> Dict.fromList
