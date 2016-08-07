module ViewTest exposing (..)

import View exposing (..)
import Html exposing (..)
import Html.App as App


main =
  App.beginnerProgram { model = defaultSettings, view = view, update = updateSettings }

view m = makePage (Text "1") (Text "2") "" 0 0 (m.cards) (m.continuous) (m.quickCheck)


{-TextChanged String | EnterPressed | TopPressed | BottomPressed | Save-}
