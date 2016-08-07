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

test = ({ progress = D.fromList [("1", 10), ("2", 10)],
         dict = D.fromList [("1", flashCard "1f" "1b"), ("2", flashCard "2f" "2b")],
         tempF = temperature,
         tempSchedule = \n -> (if (n%5==0) then 0 else 5),
         getScore = \card mTime -> case mTime of
                                       Nothing -> 10 --max score
                                       Just time -> min 5 time,
         current = "",
         step = 0,
         avgFunc = updateMovingAvg 0.9,
         lastTime = 0
       }, perform identity SetStartTime now)

-- SUBSCRIPTIONS

enter = 13
space = 32

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
