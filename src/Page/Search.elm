module Page.Search exposing (view)

import Html.Styled exposing (..)
import Data exposing (Session)


view : Maybe Session -> Html msg
view session =
    div []
        [ h2 [] [ text "Search Page" ]
        , div [] [ text "Content" ]
        ]
