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
      startTime : Time,
      endTime : Time,
      curText : String,
      avgTime : Maybe Float,
      reviewing : Bool
    }
--add pause functionality.

type Msg 
  = Finished Time Bool --finish time, and success/failure
  | Init
  | GetNext
  | SetStartTime Time
  | SetEndTime Time
  | Next String
  | SetReview Bool
--(Time -> (Model, Cmd) -> Model) --this is a callback

getCurrentCard' : Session a -> M.Maybe a
getCurrentCard'  m = D.get m.current m.dict

getCurrentCard : Session BasicFlashCard -> BasicFlashCard
getCurrentCard = M.withDefault (flashCard "" "") << getCurrentCard'

--UPDATE

modelPickNew : Session a -> D.Dict String Float -> Cmd String
modelPickNew model p = C.map (M.withDefault "") <| pick (model.tempF (model.tempSchedule model.step)) p

update = update' (flashCard "" "")

update' : a -> Msg -> Session a -> (Session a, Cmd Msg)
update' def msg model =
  case msg of
    Finished time correct -> 
        let
            --how to update the score for the current?
            elapsed = if correct then Just ((time - model.startTime) / second) else Nothing
            f = \x ->
                model.avgFunc x <|
                model.getScore (M.withDefault def <| getCurrentCard' model) elapsed
            newProgress = D.update (model.current) (M.map f) model.progress
        in
            ({ model | progress = newProgress,
                       step = model.step + 1,
                       startTime = time,
                       avgTime = case (model.avgTime, elapsed) of
                                     (Just t, Just delta) -> Just (model.avgFunc t delta)
                                     (Nothing, e) -> e
                                     (Just t, Nothing) -> Just t
             }, C.none)
    GetNext -> (model, C.map Next (modelPickNew model model.progress))
    Next nextKey -> ({ model | current = nextKey, curText = ""}, cmdReturn <| SetReview False)
    Init -> (model, perform identity SetStartTime now)
--    GetFinishTime bool -> (model, perform identity (\t -> Finished t bool) now)
    SetStartTime time -> ({ model | startTime = time, endTime = 0}, C.map Next (modelPickNew model model.progress))
    SetEndTime time -> ({ model | endTime = time}, C.none)
    SetReview bool -> ({ model | reviewing = bool}, C.none)

--MODEL

type alias BasicFlashCard = { front : String, back : String}

flashCard x y = {front=x, back=y}

check : Session BasicFlashCard -> Bool
check m = m.curText == (getCurrentCard m).back

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
         startTime = 0,
         endTime = 0,
         curText = "",
         avgTime = Nothing,
         reviewing = False
       }, perform identity SetStartTime now)
