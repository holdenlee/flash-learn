module FlashCards exposing (..)

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

{-
logShow : a -> a
logShow x = Debug.log (toString x) x-}

zip : List a -> List b -> List (a,b)
zip = map2 (,)

last : List a -> M.Maybe a
last = head << reverse

tail' : List a -> List a
tail' = M.withDefault [] << tail

dropWhile : (a -> Bool) -> List a -> List a
dropWhile f li = case li of
                   h::rest -> 
                      if f h
                      then dropWhile f rest
                      else li
                   [] -> []

pickUsingNumber : Float -> List (k,Float) -> M.Maybe k
pickUsingNumber n =  M.map fst << head << dropWhile ((\x -> x<=n) << snd)

pick : (Float -> Float) -> D.Dict comparable Float -> Cmd (Maybe comparable)
pick tempF d = 
    let
        vs = D.values d
        partials = tail' <| scanl (+) 0 <| map tempF vs
        total = M.withDefault 0 <| last partials
        pairs = zip (D.keys d) partials
    in
        generate (\x -> pickUsingNumber x pairs) (float 0 total)

temperature : Float -> Float -> Float
temperature c x = e^(c*x)

updateMovingAvg : Float -> Float -> Float -> Float
updateMovingAvg c x y = c*x+(1-c)*y

type alias Session a = 
    { progress : D.Dict String Float,
      dict : D.Dict String a,
      tempF : Float -> Float -> Float,
      tempSchedule : Int -> Float,
      getScore : a -> Maybe Float -> Float,
      current : String,
      step : Int,
      avgFunc : Float -> Float -> Float,
      lastTime : Time
    }
--add pause functionality.

type Msg 
  = Finished Time Bool --finish time, and success/failure
  | GetFinishTime Bool
  | Init
  | SetStartTime Time
  | DoNothing
  | Next (Maybe String)

getCurrentCard : Session a -> M.Maybe a
getCurrentCard  m = D.get m.current m.dict

--UPDATE

modelPickNew : Session a -> D.Dict String Float -> Cmd (M.Maybe String)
modelPickNew model p = pick (model.tempF (model.tempSchedule model.step)) p

update = update' (flashCard "" "")

update' : a -> Msg -> Session a -> (Session a, Cmd Msg)
update' def msg model =
  case msg of
    Finished time bool -> 
        let
            --how to update the score for the current?
            elapsed = if bool then Just ((time - model.lastTime) / second) else Nothing
            f = \x ->
                model.avgFunc x <|
                model.getScore (M.withDefault def <| getCurrentCard model) elapsed
            newProgress = D.update (model.current) (M.map f) model.progress
            nextKey = modelPickNew model newProgress
        in
            ({ model | progress = newProgress,
                       step = model.step + 1,
                       lastTime = time
             }, C.map Next nextKey)
    Next (Just nextKey) -> ({ model | current = nextKey}, Cmd.none)
    Next Nothing -> (model, Cmd.none)
    Init -> (model, perform identity SetStartTime now)
    GetFinishTime bool -> (model, perform identity (\t -> Finished t bool) now)
    SetStartTime time -> ({ model | lastTime = time}, C.map Next (modelPickNew model model.progress))
    DoNothing -> (model, Cmd.none)

--MODEL

type alias BasicFlashCard = { front : String, back : String}

flashCard x y = {front=x, back=y}

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
