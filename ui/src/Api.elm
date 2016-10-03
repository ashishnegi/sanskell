module Api exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task
import Dict exposing (Dict, fromList)

type JobId
  = JobId Int

type alias JobStatus =
  { statusJobId : JobId
  , jobResult : Message
  , jobState : JobState
  }

type Message
  = Message String

type JobState
  = Pending
  | Finished

decodeJobStatus : Json.Decode.Decoder JobStatus
decodeJobStatus =
  Json.Decode.succeed JobStatus
    |: ("statusJobId" := decodeJobId)
    |: ("jobResult" := decodeMessage)
    |: ("jobState" := decodeJobState)

decodeJobId : Json.Decode.Decoder JobId
decodeJobId = Json.Decode.map JobId Json.Decode.int

decodeMessage : Json.Decode.Decoder Message
decodeMessage = Json.Decode.object1 Message ("msg" := Json.Decode.string)

decodeJobState : Json.Decode.Decoder JobState
decodeJobState = Json.Decode.string `Json.Decode.andThen`
                 (\v -> case v of
                            "Pending" -> Json.Decode.succeed Pending
                            "Finished" -> Json.Decode.succeed Finished
                            _          -> Json.Decode.fail "Unknown jobstate")

getJobStatusById : JobId -> Task.Task Http.Error (JobStatus)
getJobStatusById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "job"
          ++ "/" ++ "status"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeJobStatus
      (Http.send Http.defaultSettings request)

type alias JobPostBody =
  { jobUrl : String
  }

encodeJobPostBody : JobPostBody -> Json.Encode.Value
encodeJobPostBody x =
  Json.Encode.object
    [ ( "jobUrl", Json.Encode.string x.jobUrl )
    ]

postJob : JobPostBody -> Task.Task Http.Error (JobId)
postJob body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "job"
      , body =
          Http.string (Json.Encode.encode 0 (encodeJobPostBody body))
      }
  in
    Http.fromJson
      decodeJobId
      (Http.send Http.defaultSettings request)

type alias JobResult =
  { resultJobId : JobId
  , wordsCount  : Dict String Int
  }

decodeJobResult : Json.Decode.Decoder JobResult
decodeJobResult =
  Json.Decode.succeed JobResult
    |: ("resultJobId" := decodeJobId)
    |: ("wordsCount" := Json.Decode.map Dict.fromList (Json.Decode.list (Json.Decode.tuple2 (,) Json.Decode.string Json.Decode.int)))

getJobById : JobId -> Task.Task Http.Error (JobResult)
getJobById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "job"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeJobResult
      (Http.send Http.defaultSettings request)
