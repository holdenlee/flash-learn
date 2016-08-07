import Keyboard as K
import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Set as S
import Platform.Sub as Sub

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL


type alias Model =
  { keys : S.Set Int
  }


init : (Model, Cmd Msg)
init =
  ({keys = S.empty}, Cmd.none)



-- UPDATE


type Msg
  = Down Int
  | Up Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Down n -> 
        ({model | keys = S.insert n model.keys}, Cmd.none)
    Up n -> 
        ({model | keys = S.remove n model.keys}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [K.downs Down, K.ups Up]



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString (S.toList model.keys)) ]
    ]
