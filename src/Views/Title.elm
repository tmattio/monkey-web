module Views.Title exposing (viewTitle)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


viewTitle : String -> Html msg
viewTitle title =
    section [ class "jumbotron text-center" ]
        [ div [ class "container" ]
            [ h1 [ class "jumbotron-heading" ]
                [ text title ]
            ]
        ]
