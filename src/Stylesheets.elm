module Stylesheets exposing (..)

import Css exposing (..)
import Css.Media as Media exposing (only, screen)
import Css.Foreign exposing (Snippet, body, html, media, everything)


globalWithNavbar : List Snippet
globalWithNavbar =
    [ body
        [ paddingTop (px 54)
        ]
    , media
        [ only screen [ Media.minWidth (px 992) ] ]
        [ body [ paddingTop (px 56) ] ]
    ]


globalFull : List Snippet
globalFull =
    let
        properties =
            [ property "display" "-ms-flexbox"
            , property "display" "-webkit-box"
            , displayFlex
            , property "-ms-flex-align" "center"
            , property "-ms-flex-pack" "center"
            , property "-ms-flex-align" "center"
            , property "-ms-flex-align" "center"
            , alignItems center
            , justifyContent center
            , paddingTop (px 40)
            , paddingBottom (px 40)
            , backgroundColor (hex "f5f5f5")
            ]
    in
        [ body properties
        , Css.Foreign.id "root"
            [ width (pct 100)
            ]
        ]


formSignin : Style
formSignin =
    Css.batch
        [ width (pct 100)
        , maxWidth (px 330)
        , padding (px 15)
        , margin2 (px 0) auto
        ]
