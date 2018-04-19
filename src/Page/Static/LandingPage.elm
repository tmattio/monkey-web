module Page.Static.LandingPage exposing (view)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Data exposing (Session)
import Route exposing (Route)


view : Maybe Session -> Html msg
view session =
    main_ [ attribute "role" "main" ]
        [ viewCarousel
        , viewMarketing
        ]


viewCarousel : Html msg
viewCarousel =
    div [ id "myCarousel", class "carousel slide" ]
        [ div [ class "carousel-inner" ]
            [ div [ class "carousel-item active" ]
                [ img [ class "first-slide", src "data:image/gif;base64,R0lGODlhAQABAIAAAHd3dwAAACH5BAAAAAAALAAAAAABAAEAAAICRAEAOw==", alt "First slide" ]
                    []
                , div [ class "container" ]
                    [ div [ class "carousel-caption text-left" ]
                        [ h1 []
                            [ text "Your home for data." ]
                        , p []
                            [ text "Monkey has been built to allow you to manage your data."
                            , br [] []
                            , text "Here you can find new datasets, label data, share with your team."
                            , br [] []
                            , text "All of this with the certitude that your data is taken care of!"
                            ]
                        , p []
                            [ a
                                [ class "btn btn-lg btn-primary"
                                , Route.href Route.Register
                                , attribute "role" "button"
                                ]
                                [ text "Sign up today" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewMarketing : Html msg
viewMarketing =
    div [ class "container marketing" ]
        [ div [ class "row" ]
            [ div [ class "col-lg-4" ]
                [ div [ class "features-icons-icon d-flex" ]
                    [ i [ class "icon-book-open m-auto text-primary" ]
                        []
                    ]
                , h2 []
                    [ text "Find" ]
                , p []
                    [ text "We are adding public datasets everyday for you to use on Monkey. Here you can find popular datasets like ImageNet or MNIST alongside thousands of datasets created by other users." ]
                ]
            , div [ class "col-lg-4" ]
                [ div [ class "features-icons-icon d-flex" ]
                    [ i [ class "icon-rocket m-auto text-primary" ]
                        []
                    ]
                , h2 []
                    [ text "Build" ]
                , p []
                    [ text "We built dozens of data annotation tools, ranging from image classification to text parsing. And if we don't have the tool you need, you can even hook to our API and build your own!" ]
                ]
            , div [ class "col-lg-4" ]
                [ div [ class "features-icons-icon d-flex" ]
                    [ i [ class "icon-share m-auto text-primary" ]
                        []
                    ]
                , h2 []
                    [ text "Share" ]
                , p []
                    [ text "Create an organization to share datasets with your team, fork and merge datasets together, share your results for a dataset. We're here to empower you to share your data!" ]
                ]
            ]
        , hr [ class "featurette-divider" ]
            []
        , div [ class "row featurette" ]
            [ div [ class "col-md-7" ]
                [ h2 [ class "featurette-heading" ]
                    [ text "First featurette heading."
                    , span
                        [ class "text-muted" ]
                        [ text "It'll blow your mind." ]
                    ]
                , p [ class "lead" ]
                    [ text "Donec ullamcorper nulla non metus auctor fringilla. Vestibulum id ligula porta felis euismod semper. Praesent commodo cursus magna, vel scelerisque nisl consectetur. Fusce dapibus, tellus ac cursus commodo." ]
                ]
            , div [ class "col-md-5" ]
                [ img [ class "featurette-image img-fluid mx-auto", attribute "data-src" "holder.js/500x500/auto", alt "Generic placeholder image" ]
                    []
                ]
            ]
        , hr [ class "featurette-divider" ]
            []
        , div [ class "row featurette" ]
            [ div [ class "col-md-7 order-md-2" ]
                [ h2 [ class "featurette-heading" ]
                    [ text "Oh yeah, it's that good."
                    , span
                        [ class "text-muted" ]
                        [ text "See for yourself." ]
                    ]
                , p [ class "lead" ]
                    [ text "Donec ullamcorper nulla non metus auctor fringilla. Vestibulum id ligula porta felis euismod semper. Praesent commodo cursus magna, vel scelerisque nisl consectetur. Fusce dapibus, tellus ac cursus commodo." ]
                ]
            , div [ class "col-md-5 order-md-1" ]
                [ img [ class "featurette-image img-fluid mx-auto", attribute "data-src" "holder.js/500x500/auto", alt "Generic placeholder image" ]
                    []
                ]
            ]
        , hr [ class "featurette-divider" ]
            []
        , div [ class "row featurette" ]
            [ div [ class "col-md-7" ]
                [ h2 [ class "featurette-heading" ]
                    [ text "And lastly, this one."
                    , span
                        [ class "text-muted" ]
                        [ text "Checkmate." ]
                    ]
                , p [ class "lead" ]
                    [ text "Donec ullamcorper nulla non metus auctor fringilla. Vestibulum id ligula porta felis euismod semper. Praesent commodo cursus magna, vel scelerisque nisl consectetur. Fusce dapibus, tellus ac cursus commodo." ]
                ]
            , div [ class "col-md-5" ]
                [ img [ class "featurette-image img-fluid mx-auto", attribute "data-src" "holder.js/500x500/auto", alt "Generic placeholder image" ]
                    []
                ]
            ]
        ]
