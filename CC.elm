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
    | CardCvvNumber String
    | CardDateNumber String


init =
    ( { number = "", cvv = "", date = "" }, Cmd.none )


limitSize len str =
    String.slice 0 len str


splitAt pos str =
    if length str > pos then
        [ left pos str, dropLeft pos str ]
    else
        [ str ]


splitAtCouple pos str =
    ( left pos str, dropLeft pos str )


splitEvery pos str =
    if length str > pos then
        let
            ( head, tail ) =
                splitAtCouple pos str
        in
            head :: splitEvery pos tail
    else
        [ str ]


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


numberFormat text =
    text
        |> onlyNumbers
        |> limitSize 16
        |> putSpacesEvery 4


cvvFormat text =
    text
        |> onlyNumbers
        |> limitSize 4


dateFormat text =
    text
        |> onlyNumbers
        |> limitSize 6
        |> splitAt 2
        |> join " / "


textInput val inputMsg theId theName =
    div [ style [ ( "padding", "12px" ) ] ]
        [ label [ for theId ] [ text theName ]
        , br [] []
        , input [ style [ ( "width", "100%" ), ( "font-size", "20px" ) ], type' "text", val |> value, id theId, onInput inputMsg ] []
        ]


numberInput model =
    textInput (model.number |> numberFormat) CardNumber "card-number" "Number"


cvvInput model =
    textInput (model.cvv |> cvvFormat) CardCvvNumber "card-cvv" "CVV"


dateInput model =
    textInput (model.date |> dateFormat) CardDateNumber "card-date" "Date"


view model =
    div [ style [ ( "margin-left", "auto" ), ( "margin-right", "auto" ), ( "max-width", "300px" ) ] ]
        [ model |> numberInput
        , model |> dateInput
        , model |> cvvInput
        ]


update msg model =
    case msg of
        CardNumber number ->
            ( { model | number = number |> removeSpace }, Cmd.none )

        CardCvvNumber cvv ->
            ( { model | cvv = cvv |> removeSpace }, Cmd.none )

        CardDateNumber date ->
            ( { model | date = date |> removeSpace }, Cmd.none )
