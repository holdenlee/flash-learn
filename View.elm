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

ht x = attribute "style" ("height:" ++ (toString x) ++ "%")

makeCard : DisplayThing -> Html Msg
makeCard dt = 
    case dt of
        Text str -> text str
        Image url -> img [attribute "src" url] []

makePage : DisplayThing -> DisplayThing -> Float -> Float -> Int -> Bool -> Html Msg
makePage card1 card2 avg speed cards continuous=
    div [] 
        [table [class "center", attribute "style" "width:100%;height:100%;"] --2x2, consisting of cards, side pane, text input
             --http://stackoverflow.com/questions/6654262/100-height-nested-table-in-standards-mode
             [tr [ht 100] --cards and side pane
                  [td [attribute "style" "width:80%;height:100%;"] --cards
                       [table [attribute "style" "width:100%;height:100%;"
                              ] -- attribute "border" "1" -- nested table for cards
                            (if cards==1 
                             then [tr [ht 100] [td [ht 100, class "index_card"] [makeCard card1]]]
                             else

                                 [tr [ht 50] [td [ht 50, class "index_card"] [makeCard card1]],
                                  tr [ht 50] [td [ht 50, class "index_card"] [makeCard card2]]])],
         
                   td [ht 100] --side panel
                       [p [] [text "Average time:"],
                        p [class "center"] [text (toString avg)],
                        p [] [text "Speed (per minute):"],
                        p [class "center"] [text (toString speed)],
                        button [onClick TopPressed] [text <| if cards==1 then "1 card view" else "2 card view"],
                        br [] [],
                        button [onClick BottomPressed] [text <| if continuous then "Continuous" else "With pauses"]]]],
          input [onInput TextChanged, attribute "style" "width:100%"] []]
