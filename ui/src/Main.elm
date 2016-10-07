module Main exposing (main)

import Html exposing (Html, div, text, button, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, class)
import Html.App as App
import Api as Api
import Task
import Http
import Dict exposing (Dict)

type alias URL = String
type StatusMsg = NoStatus
               | TextMessage String
               | ErrorMessage String
               | URLMessage String URL

type alias Model = { websiteUrl : URL
                   , statusMessage : StatusMsg
                   , jobResults : Dict Api.JobId Api.JobResult
                   }

type Msg = WebsiteInput URL
         | PostJob
         | PostJobSuccess Api.JobId
         | PostJobFailed Http.Error
         | StatusJobSuccess Api.JobStatus
         | StatusJobFailed Http.Error
         | JobResultSuccess Api.JobResult
         | JobResultFailed Http.Error

init : ( Model, Cmd Msg)
init = ( Model "" NoStatus Dict.empty, Cmd.none )

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
            let (msg, cmd) =
                    case jobStatus.jobState of
                        Api.Pending  -> ( TextMessage jobStatus.jobResult.message, Cmd.none )
                        Api.Finished -> ( URLMessage "You can view the word cloud at" jobStatus.jobResult.message
                                        , Task.perform JobResultFailed JobResultSuccess (Api.getJobById jobStatus.statusJobId ))
                        Api.Failed   -> ( ErrorMessage jobStatus.jobResult.message, Cmd.none )
            in ( { model | statusMessage = msg }, Cmd.none )
        StatusJobFailed error ->
            ( { model | statusMessage = errorMsg error "Failed to get job status. Please try again." }, Cmd.none )

        JobResultSuccess jobResult ->
            ( { model | jobResults = Dict.insert jobResult.resultJobId jobResult model.jobResults  }, Cmd.none )
        JobResultFailed error ->
            ( { model | statusMessage = errorMsg error "Failed to get data. Please try again." }, Cmd.none )

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

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
