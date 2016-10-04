module Main exposing (main)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, class)
import Html.App as App
import Api as Api
import Task
import Http

type alias URL = String
type StatusMsg = NoStatus
               | TextMessage String
               | ErrorMessage String
               | URLMessage String URL

type alias Model = { websiteUrl : URL
                   , statusMessage : StatusMsg
                   }

type Msg = WebsiteInput URL
         | PostJob
         | PostJobSuccess Api.JobId
         | PostJobFailed Http.Error
         | StatusJobSuccess Api.JobStatus
         | StatusJobFailed Http.Error

init : ( Model, Cmd Msg)
init = ( Model "" NoStatus, Cmd.none )

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
            let errorMsg = case error of
                Http.BadResponse _ serverMsg -> serverMsg
                _                            -> "Please retry after some time"
            in ({ model | statusMessage = ErrorMessage ("Failed to post job : " ++ errorMsg) }, Cmd.none)

        StatusJobSuccess jobStatus ->
            let msg = case jobStatus.jobState of
                          Api.Pending  -> TextMessage jobStatus.jobResult.message
                          Api.Finished -> URLMessage "You can view the word cloud at" jobStatus.jobResult.message
                          Api.Failed   -> ErrorMessage jobStatus.jobResult.message
            in ( { model | statusMessage = msg }, Cmd.none )
        StatusJobFailed error ->
            let errorMsg = case error of
                Http.BadResponse _ serverMsg -> serverMsg
                _                            -> "Something went wrong"
            in ({ model | statusMessage = ErrorMessage ("Failed to post job : " ++ errorMsg) }, Cmd.none)



view : Model -> Html Msg
view model =
    div []
        [ div [ class "url-input" ]
              [ Html.input [ onInput WebsiteInput
                           , placeholder "any sanskrit website name" ]
                           []]
        , button [ class "send-job-btn"
                 , onClick PostJob ]
                 [ text "Make word cloud" ]
        , text (toString model) ]

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
