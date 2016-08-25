module CC exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (slice, length, left, dropLeft, join)
import List exposing (map)
import CssHelpers exposing (..)
import InlineHover exposing (..)
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


textInputStyle =
    [ sP 6
    , sW100
    , sFs 20
    ]


textInput val inputMsg theName =
    div [ style [ sP 12 ] ]
        [ input [ style textInputStyle, type' "text", value val, placeholder theName, onInput inputMsg ] []
        ]


numberInput model =
    textInput (model.number |> numberFormat) CardNumber "Number"


cvvInput model =
    textInput (model.cvv |> cvvFormat) CardCvvNumber "CVV"


dateInput model =
    textInput (model.date |> dateFormat) CardDateNumber "Date"


globalStyles =
    Html.node "style" [] [ text "* {box-sizing: border-box} input:focus, button:focus {outline: none; border-color: transparent; box-shadow: 0 0 10px #bbb;}" ]


view model =
    div [ style (tc :: ffs :: (centeredWithMaxWidth 300)) ]
        [ globalStyles
        , h1 [] [ text "Credit card details" ]
        , Html.form []
            [ model |> numberInput
            , model |> dateInput
            , model |> cvvInput
            , div [ style [ sP 12 ] ] [ hover btnHover button [ style btn ] [ text "Pay" ] ]
            ]
        ]


update msg model =
    case msg of
        CardNumber number ->
            ( { model | number = number |> onlyNumbers }, Cmd.none )

        CardCvvNumber cvv ->
            ( { model | cvv = cvv |> onlyNumbers }, Cmd.none )

        CardDateNumber date ->
            ( { model | date = date |> onlyNumbers }, Cmd.none )
