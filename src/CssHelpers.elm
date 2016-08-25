module CssHelpers exposing (..)


toPx : Int -> String
toPx i =
    (i |> toString) ++ "px"


centeredWithMaxWidth number =
    [ ( "margin-left", "auto" )
    , ( "margin-right", "auto" )
    , ( "max-width", number |> toPx )
    , ( "box-sizing", "border-box" )
    ]


sW100 =
    ( "width", "100%" )


sP number =
    ( "padding", number |> toPx )


sFs number =
    ( "font-size", number |> toPx )


ffs =
    ( "font-family", "Sans-Serif" )


tc =
    ( "text-align", "center" )


myGray =
    "#aaa"


btn =
    sW100
        :: (sP 12)
        :: [ ( "background-color", "white" )
           , ( "color", myGray )
           , ( "border", "1px solid " ++ myGray )
           , ( "border-radius", "5px" )
           ]


btnHover =
    [ ( "background-color", myGray )
    , ( "color", "white" )
    ]
