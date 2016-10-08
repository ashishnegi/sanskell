module Main exposing (main)

import Html exposing (Html, div, text, button, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, class)
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

type alias URL = String

type StatusMsg = NoStatus
               | TextMessage String
               | ErrorMessage String
               | URLMessage String URL

type alias WordCloud = Dict String (Count, Weight, Position)

type alias Model = { websiteUrl : URL
                   , statusMessage : StatusMsg
                   , wordCounts : Dict Api.JobId WordCloud
                   , pendingRequests : Set Api.JobId
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
         | RandomWordCloudPositions Api.JobResult (List Position)

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

main =
    App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : ( Model, Cmd Msg)
init = ( Model "" NoStatus Dict.empty Set.empty, Cmd.none )

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
                        Api.Pending  -> ( TextMessage jobStatus.jobResult.message, Cmd.none, True )
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
            ( model, randomWordPositionsCmd jobResult (svgDimension jobResult.wordsCount))
        JobResultFailed error ->
            ( { model | statusMessage = errorMsg error "Failed to get data. Please try again." }, Cmd.none )

        CheckJobStatus ->
            ( model
            , model.pendingRequests
            |> Set.toList
            |> List.map (\ jobId -> Task.perform StatusJobFailed StatusJobSuccess (Api.getJobStatusById jobId))
            |> Cmd.batch )

        RandomWordCloudPositions jobResult positions ->
            ( { model | wordCounts = Dict.insert jobResult.resultJobId (makeWordCloud jobResult positions) model.wordCounts }, Cmd.none )

errorMsg : Http.Error -> String -> StatusMsg
errorMsg error defaultMsg =
    case error of
        Http.BadResponse _ serverMsg -> ErrorMessage (defaultMsg ++ " : " ++ serverMsg)
        _                            -> ErrorMessage defaultMsg

view : Model -> Html Msg
view model =
    div []
        (List.concat
             [ [ div [ class "url-input" ]
                     [ Html.input [ onInput WebsiteInput
                                  , placeholder "any sanskrit website name" ]
                           []]
               , button [ class "send-job-btn"
                        , onClick PostJob ]
                     [ text "Make word cloud" ]
               , text (toString model)
               , br [] []
               ]
             , (List.map wordCloud (Dict.values model.wordCounts))
             , [ button
                     [ onClick (JobResultSuccess (Api.JobResult 1 someWordsCount))  ]
                     [ text "WordCloud" ]
               ]
             ])

someWordsCount = Dict.fromList [("ashish", 30), ("negi", 14)]

subscriptions : Model -> Sub Msg
subscriptions model =
    if Set.isEmpty model.pendingRequests
    then Sub.none
    else Time.every (2 * Time.second) ( \ _ -> CheckJobStatus )

svgDimension : Dict a b -> Dimension
svgDimension d = let dsize = Dict.size d |> (*) 3 >> max 500 >> min 1200
                 in Dimension dsize (round ((toFloat dsize) / 1.5))

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

randomWordPositionsCmd : Api.JobResult -> Dimension -> Cmd Msg
randomWordPositionsCmd jobResult dimension =
    Random.generate (RandomWordCloudPositions jobResult)
        (Random.list (Dict.size jobResult.wordsCount)
             (Random.map2 Position
                  (Random.int 20 (dimension.width - 60))
                  (Random.int 20 (dimension.height - 20))))

makeWordCloud : Api.JobResult -> List Position -> WordCloud
makeWordCloud jobResult positions =
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
