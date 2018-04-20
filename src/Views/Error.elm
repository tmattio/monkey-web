module Views.Error exposing (viewWithError)

import Html.Styled exposing (..)
import RemoteData
import Request.Helpers exposing (WebData, parseGraphQLError)


viewWithError : WebData a -> (a -> Html msg) -> Html msg
viewWithError response successView =
    case response of
        RemoteData.Success r ->
            successView r

        RemoteData.Failure err ->
            let
                errorMsg =
                    parseGraphQLError err
            in
                div []
                    [ p [] [ text "An error occured 😱😱😱😱 " ]
                    , p [] [ text errorMsg ]
                    ]

        RemoteData.Loading ->
            div []
                [ text "Still loading... Here is something to watch in the meantime:"
                , a [] [ text "https://www.youtube.com/watch?v=vaB51HPxYKM" ]
                ]

        RemoteData.NotAsked ->
            div []
                [ text "We're supposed to ask for data, but we didn't 😱😱😱😱"
                ]
