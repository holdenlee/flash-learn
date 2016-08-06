module ViewTest exposing (..)

import View exposing (..)
import Html exposing (..)
import Html.App as App

type alias Settings = 
    {cards : Int,
     continuous : Bool}

main =
  App.beginnerProgram { model = model, view = view, update = update }

model = {cards=1, continuous=True}

view m = makePage (Text "1") (Text "2") 0 0 (m.cards) (m.continuous)

update msg m = case msg of
                   TopPressed -> {m | cards = 3-m.cards}
                   BottomPressed -> {m | continuous = not m.continuous}

                   _ -> m
{-TextChanged String | EnterPressed | TopPressed | BottomPressed | Save-}
