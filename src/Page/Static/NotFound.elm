module Page.Static.NotFound exposing (view)

import Html.Styled exposing (Html, main_, h1, div, img, text)
import Html.Styled.Attributes exposing (class, tabindex, id, src, alt)
import Data.Auth exposing (Session)


-- VIEW --


view : Maybe Session -> Html msg
view session =
    main_ [ tabindex -1 ]
        [ h1 [] [ text "Not Found" ]
        ]
