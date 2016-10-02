module Main exposing (main)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, class)
import Html.App as App
import Api as Api

type alias URL = String
type StatusMsg = NoStatus
               | JobSuccessStatus String URL
               | JobFailureStatus String

type alias Model = { websiteUrl : URL
                   , statusMessage : StatusMsg
                   }

type Msg = WebsiteInput URL
         | SendJob
         | SendJobSuccess
         | SendJobFailed

init : ( Model, Cmd Msg)
init = ( Model "" NoStatus, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        WebsiteInput url ->
            ( { model | websiteUrl = url }, Cmd.none )
        SendJob ->
            ( model, Cmd.none ) -- send http job
        SendJobSuccess ->
            ( model, Cmd.none ) -- add job success message
        SendJobFailed ->
            ( model, Cmd.none ) -- add job post failure message

view : Model -> Html Msg
view model =
    div []
        [ div [ class "url-input" ]
              [ Html.input [ onInput WebsiteInput
                           , placeholder "any sanskrit website name" ]
                           []]
        , button [ class "send-job-btn"
                 , onClick SendJob ]
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