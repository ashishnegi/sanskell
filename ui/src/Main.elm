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

type alias URL = String
type StatusMsg = NoStatus
               | TextMessage String
               | ErrorMessage String
               | URLMessage String URL

type alias Model = { websiteUrl : URL
                   , statusMessage : StatusMsg
                   , jobResults : Dict Api.JobId Api.JobResult
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
         | RandomWordCloudPositions Api.JobId (List (Int,Int))

type alias Dimension =
    { width : Int
    , height : Int
    }

type alias Weight = Int
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
            ( { model | jobResults = Dict.insert jobResult.resultJobId jobResult model.jobResults  }, Cmd.none )
        JobResultFailed error ->
            ( { model | statusMessage = errorMsg error "Failed to get data. Please try again." }, Cmd.none )

        CheckJobStatus ->
            ( model
            , model.pendingRequests
            |> Set.toList
            |> List.map (\ jobId -> Task.perform StatusJobFailed StatusJobSuccess (Api.getJobStatusById jobId))
            |> Cmd.batch )

        RandomWordCloudPositions jobId positions -> ( model, Cmd.none )

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
             , (List.map ( \ result ->
                                  text (toString result.wordsCount))
                    (Dict.values model.jobResults))
             ])

subscriptions : Model -> Sub Msg
subscriptions model =
    if Set.isEmpty model.pendingRequests
    then Sub.none
    else Time.every (2 * Time.second) ( \ _ -> CheckJobStatus )

wordCloud : Dict String (Weight, Position) -> Html Msg
wordCloud wordsCount =
    div [] []

randomGridCmd : Api.JobResult -> Dimension -> Cmd Msg
randomGridCmd jobResult dimension =
    Random.generate (RandomWordCloudPositions jobResult.resultJobId)
        (Random.list (Dict.size jobResult.wordsCount)
             (Random.map2 (,)
                  (Random.int 1 dimension.width)
                  (Random.int 1 dimension.height)))
