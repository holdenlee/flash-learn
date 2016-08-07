module Utilities exposing (..)

import List exposing (..)
import Maybe as M
import Task as T

{-
logShow : a -> a
logShow x = Debug.log (toString x) x-}

zip : List a -> List b -> List (a,b)
zip = map2 (,)

last : List a -> M.Maybe a
last = head << reverse

tail' : List a -> List a
tail' = M.withDefault [] << tail

dropWhile : (a -> Bool) -> List a -> List a
dropWhile f li = case li of
                   h::rest -> 
                      if f h
                      then dropWhile f rest
                      else li
                   [] -> []

--http://package.elm-lang.org/packages/shmookey/cmd-extra/1.0.0/Cmd-Extra
cmdReturn : msg -> Cmd msg
cmdReturn x = T.perform (always x) (identity) (T.succeed x)
