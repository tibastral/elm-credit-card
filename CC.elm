module CC exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (slice, length, left, dropLeft, join)
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


splitEvery pos str =
    if length str > pos then
        left pos str :: splitEvery pos (dropLeft pos str)
    else
        []



-- [ left pos str, dropLeft pos str ]


putSpacesEvery len str =
    str
        |> splitEvery len
        |> join " "


removeRegex regex text =
    text
        |> Regex.replace Regex.All (Regex.regex regex) (\_ -> "")


onlyNumbers text =
    text
        |> removeRegex "\\D"


removeSpace text =
    text
        |> removeRegex " "


spaced text =
    text
        |> onlyNumbers
        |> limitSize 16
        |> putSpacesEvery 4


view model =
    input [ type' "text", model |> spaced |> value, onInput CardNumber ] []


update msg model =
    case msg of
        CardNumber newVal ->
            ( newVal |> removeSpace, Cmd.none )
