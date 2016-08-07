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
import Utilities exposing (..) 

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
