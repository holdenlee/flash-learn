module View exposing (makePage, DisplayThing (..))

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
import Html.Attributes exposing (..)

type DisplayThing = Text String | Image String

type Msg = TextChanged String | EnterPressed | TopPressed | BottomPressed | Save

makePage : DisplayThing -> DisplayThing -> Float -> Float -> Html Msg
makePage card1 card2 avg speed =
    div [] 
        [table [class "center", attribute "style" "width:100%"] --2x2, consisting of cards, side pane, text input
             [tr [] --cards and side pane
                  [td [] --cards
                       [table [attribute "style" "width:100%;height:100%"
                              ] -- attribute "border" "1" -- nested table for cards
                            [tr [] [td [class "index_card"] [text "1"]],
                             tr [] [td [class "index_card"] [text "1"]]]],
         
                   td [] --side panel
                       [p [] [text "Average time:"],
                        p [class "center"] [text (toString avg)],
                        p [] [text "Speed (per minute):"],
                        p [class "center"] [text (toString speed)],
                        button [onClick TopPressed] [text "top"],
                        br [] [],
                        button [onClick BottomPressed] [text "top"]]]],
          input [onInput TextChanged, attribute "style" "width:100%"] []]
