module Page.Static.Features exposing (view)

import Html.Styled exposing (..)
import Data.Auth exposing (Session)


view : Maybe Session -> Html msg
view session =
    div []
        [ h2 [] [ text "Features Page" ]
        , div [] [ text "Content" ]
        ]
