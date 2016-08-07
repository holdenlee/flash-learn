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

type alias Foo ={x:Int, y:FooFunc}

type FooFunc = FooFunc (Foo -> Foo)

type Bar = A (Int -> Bar)
         | B (List Bar)
