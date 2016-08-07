module View exposing (makePage, DisplayThing (..), Msg (..), defaultSettings, updateSettings, Settings)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)

type DisplayThing = Text String | Image String

type Msg = TextChanged String | TopPressed | BottomPressed | CheckPressed | Save

ht x = attribute "style" ("height:" ++ (toString x) ++ "%")

makeCard : DisplayThing -> Html Msg
makeCard dt = 
    case dt of
        Text str -> text str
        Image url -> img [attribute "src" url] []

makePage : DisplayThing -> DisplayThing -> String -> Float -> Float -> Int -> Bool -> Bool -> Html Msg
makePage card1 card2 txt avg speed cards continuous check =
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
                        makeButton (cards==1) TopPressed "1 card view" "2 card view",
                        br [] [],
                        makeButton continuous BottomPressed "Continuous" "With pauses",
                        br [] [] ,
                        makeButton check CheckPressed "Quick-check" "Enter to submit",
                        br [] [],
                        button [onClick Save] [text "Save"]
                        --input [type' "file", id "file-input"] []
                        ]]],
          input [onInput TextChanged, attribute "style" "width:100%", value txt] []]

makeButton b action txt1 txt2 = 
    button [onClick action] [text <| if b then txt1 else txt2]

type alias Settings = 
    {cards : Int,
     continuous : Bool,
     quickCheck : Bool}

defaultSettings = {cards=1, continuous=True, quickCheck = True}

updateSettings msg m = case msg of
                   TopPressed -> {m | cards = 3-m.cards}
                   BottomPressed -> {m | continuous = not m.continuous}
                   CheckPressed -> {m | quickCheck = not m.quickCheck}
                   _ -> m
