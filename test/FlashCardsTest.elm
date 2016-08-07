module FlashCardsTest exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Random
import Dict as D
import List exposing (..)
import Maybe as M
import Debug exposing (..)
import Keyboard as K
import Time exposing (..)
import Char exposing (..)
import Platform.Sub as S
import Task exposing (perform)
import Time exposing (..)
import Random exposing (generate, float)
import Platform.Cmd as C
import Html.Attributes exposing (class)
import FlashCards exposing (..)

import KeyCodes exposing (..) 

type alias Model = Session BasicFlashCard

--VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ((M.withDefault (flashCard "" "") <| getCurrentCard model).front) ],
      p [class "center"] [text (toString model.progress)],
      p [] [text (toString model.lastTime)]
    ] --((getCurrentCard model).front)

--INIT


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    K.downs (\x -> 
                      if x==enter
                      then GetFinishTime True
                      else if x==space
                      then GetFinishTime False
                      else if x==toCode 'i'
                      then Init
                      else DoNothing)

--MAIN

main =
  App.program
    { init = test
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

{-
elm make src/MyThing.elm --output=my-thing.js
-}
