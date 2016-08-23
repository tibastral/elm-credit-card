module CC exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (..)
import List exposing (map)
import Regex


main =
    Html.program { view = view, init = init, subscriptions = (\e -> Sub.none), update = update }


type Msg
    = CardNumber String


init =
    ( "", Cmd.none )


limitSize len str =
    String.slice 0 len str


putSpacesEvery len str =
    let
        putSpacesEveryLen =
            putSpacesEvery len
    in
        if String.length str > len then
            (str |> left len |> putSpacesEveryLen)
                ++ " "
                ++ (str |> dropLeft len |> putSpacesEveryLen)
        else
            str



-- if String.length str > len
--   spacesBy len (str |> slice 0 4) ++ spaceBy len (str |> slice 4 0)
-- else
--   str


spaced text =
    text
        |> limitSize 16
        |> putSpacesEvery 4



-- (String.slide 0 16 text)
--   if String.length text > 4 then
--       String.slice 0 4 text ++ " " ++ String.slice 4 8 text
--   else
--       String.slice 0 4 text
-- String.slice 0 4 text ++ " " ++ slice 4 8 text


removeSpace text =
    Regex.replace Regex.All (Regex.regex " ") (\_ -> "") text


view model =
    input [ type' "text", model |> spaced |> value, onInput CardNumber ] []


update msg model =
    case msg of
        CardNumber newVal ->
            ( newVal |> removeSpace, Cmd.none )
