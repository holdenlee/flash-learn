module FlashLearn exposing (..)

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
import FlashCards as FC
import View as V
import KeyCodes exposing (..)

--MODEL

type alias Model = {session : FC.Session FC.BasicFlashCard,
                    settings : V.Settings
                   }

type Msg = UserAction V.Msg 
         | InternalAction FC.Msg 
         | RequestTime (Time -> Msg) 
         | Msgs (List Msg)

--UPDATE
update : Msg -> Model -> (Model, C.Cmd Msg)
update msg m = 
    case msg of
        UserAction userMsg ->
            let
                m' = {m | settings = V.updateSettings userMsg m.settings}
            in
                case userMsg of
                    --quickCheck means that we check correctness as it is typed.
                    V.TextChanged str -> 
                        let s = m'.session
                            m'' = {m' | session = {s | curText = str}}
--https://groups.google.com/forum/#!topic/elm-discuss/JeelOj2d00w
--https://groups.google.com/forum/#!topic/elm-discuss/CH77QbLmSTk
                        in (m'', 
                             if m''.settings.quickCheck && FC.check m''.session
                             then
                                 cmdReturn <| RequestTime (finish m'')
                             else C.none)
                    _ ->(m', C.none)
        InternalAction intMsg -> 
            let
                (session', cmd) = FC.update intMsg m.session
            in
                ({m | session = session'}, C.map InternalAction cmd)
        RequestTime f -> (m, perform identity f now)
        Msgs msgs -> (m, C.batch <| map cmdReturn msgs)
{-
V.TextChanged String | EnterPressed | TopPressed | BottomPressed | Save
-}

{- input: whether to go into review mode -}
finish' : Bool -> Bool -> Time -> Msg
finish' correct review time = Msgs 
                           ([(InternalAction (FC.Finished time correct))] ++ [if review then InternalAction (FC.SetReview True) else InternalAction FC.GetNext])

finish : Model -> Time -> Msg
finish m time = 
    let 
        c = FC.check m.session
    in 
        finish' c (whetherToReview c m) time

delayFinish : Time -> Msg
delayFinish = InternalAction << FC.SetEndTime

whetherToReview : Bool -> Model -> Bool
whetherToReview correct m =  (((not correct)) && (m.settings.cards==2)) || (not m.settings.continuous)
-- || (not m.settings.quickCheck)

--SUBSCRIPTIONS

subscriptions : Model -> S.Sub Msg
subscriptions m = 
    K.presses (\kc -> 
                 if kc == enter --continue
                 then
                     if m.session.reviewing --if reviewing
                     then
                         if m.session.endTime /= 0 --if manually checking
                         then Msgs [InternalAction (FC.Finished m.session.endTime True), InternalAction FC.GetNext] --mark as correct, get next card
                         else InternalAction FC.GetNext --get next card
                     else
                         RequestTime (finish m) -- request time and then finish
                 else if kc == down && not m.session.reviewing --flip card
                 then RequestTime delayFinish
                 else if kc == up -- ignore card
                 then InternalAction FC.GetNext
                 else if kc == esc && m.session.reviewing && m.session.endTime/=0 --manually mark answer as wrong
                 then Msgs [InternalAction (FC.Finished m.session.endTime False), InternalAction FC.GetNext]
                 else Msgs [])

-- INIT

init : (Model, Cmd Msg)
init = ({session = fst (FC.test),
         settings = V.defaultSettings}, 
        cmdReturn <| InternalAction FC.Init)

--VIEW

view : Model -> Html Msg
view m = App.map UserAction <| V.makePage (V.Text ((FC.getCurrentCard m.session).front))
                    (if m.session.reviewing then (V.Text ((FC.getCurrentCard m.session).back)) else V.Text "")
                    m.session.curText
                    (M.withDefault 9999 <| m.session.avgTime)
                    (M.withDefault 0 <| M.map (\x -> 60/x) m.session.avgTime)
                    m.settings.cards
                    m.settings.continuous
                    m.settings.quickCheck
--DisplayThing -> DisplayThing -> String -> Float -> Float -> Int -> Bool -> Bool -> Html Msg

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
