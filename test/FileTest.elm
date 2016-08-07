port module FileTest exposing (..)

import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Keyboard exposing (..)
import Html.App as App
import Platform.Cmd exposing (..) 
import Platform.Sub as S
import Utilities exposing (..)

main =
  App.program { init = "" ! [cmdReturn GetFile], view = view, update = update, subscriptions = subscriptions }

-- UPDATE

type Msg = GetFile | ReceivedFile String

update x oldContent = case x of
                          GetFile -> oldContent ! [getFile ()]
                          ReceivedFile str -> str ! []

port getFile : () -> Cmd msg

port receivedFile : (String -> msg) -> Sub msg

-- VIEW

view content =
  div []
    [ text content
    ]

subscriptions m = receivedFile ReceivedFile
