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

--MODEL

type alias Model = {session : FC.Session FC.BasicFlashCard,
                    settings : V.Settings,
                    curText : String}

type Msg = UserAction V.Msg | InternalAction FC.Msg

--UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg m = 
    case msg of
        UserAction userMsg ->
            let
                m' = {m | settings = fst <| V.update userMsg m.settings}
            in
                case userMsg of
                    TextChanged str -> ({m' | curText = str}, C.none)
                    EnterPressed str -> (m', C.none)
                    _ ->(m', C.none)

{-
TextChanged String | EnterPressed | TopPressed | BottomPressed | Save
-}
