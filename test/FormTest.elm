import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Keyboard exposing (..)
import Html.App as App
import Platform.Cmd exposing (..) 

main =
  App.program { init = "" ! [], view = view, update = update, subscriptions = subscriptions }


-- UPDATE

type Msg = NewContent String | Enter | DoNothing

update x oldContent = (case x of
                           NewContent content -> content
                           Enter -> ""
                           DoNothing -> oldContent) ! []

-- VIEW

view content =
  div []
    [ input [ value content, onInput NewContent, myStyle ] []
    ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

subscriptions m = 
  presses (\x -> if x==13 then Enter else DoNothing)
