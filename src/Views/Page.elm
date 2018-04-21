module Views.Page exposing (ActivePage(..), Layout(..), layout, init, update, Model, Msg)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)
import Css.Foreign exposing (global)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Route exposing (Route)
import Data.Auth exposing (Session)
import Views.Form as Form
import Stylesheets exposing (globalWithNavbar, globalFull)


type ActivePage
    = Other
    | LandingPage
    | Dashboard
    | Label
    | Features
    | OurVision
    | Search
    | Login
    | Register
    | Pricing
    | Explore
    | Profile
    | PreviewDataset
    | SettingsDataset
    | CreateDataset
    | Settings


type Layout
    = Blank
    | Landing
    | Fluid
    | FullPage


layout : Maybe Session -> Bool -> Layout -> ActivePage -> Html msg -> Html msg
layout session isLoading layout page content =
    let
        ( innerLayout, container ) =
            case layout of
                Blank ->
                    ( [ global globalWithNavbar |> toUnstyled
                      , viewHeader session page |> toUnstyled
                      ]
                    , Grid.containerFluid
                    )

                Landing ->
                    ( [ global globalWithNavbar |> toUnstyled
                      , viewHeader session page |> toUnstyled
                      , content |> toUnstyled
                      , viewFooterLanding |> toUnstyled
                      ]
                    , Grid.containerFluid
                    )

                Fluid ->
                    ( [ global globalWithNavbar |> toUnstyled
                      , viewHeader session page |> toUnstyled
                      , content |> toUnstyled
                      , viewFooter |> toUnstyled
                      ]
                    , Grid.containerFluid
                    )

                FullPage ->
                    ( [ global globalFull |> toUnstyled
                      , content |> toUnstyled
                      ]
                    , Grid.containerFluid
                    )
    in
        container []
            ([ CDN.stylesheet ] ++ innerLayout)
            |> Html.Styled.fromUnstyled


viewHeader : Maybe Session -> ActivePage -> Html msg
viewHeader session page =
    nav [ class "navbar navbar-expand-lg navbar-dark bg-dark fixed-top" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "Monkey" ]
            , button [ class "navbar-toggler", type_ "button" ]
                [ span [ class "navbar-toggler-icon" ] [] ]
            , div [ class "collapse navbar-collapse" ]
                [ ul [ class "navbar-nav mr-auto" ] <|
                    viewHeaderLinks session page
                , Html.Styled.form
                    [ class "form-inline mt-2 mt-md-0" ]
                    [ div [ class "input-group" ]
                        [ Form.input
                            [ class "form-control search-bar"
                            , placeholder "Search Monkey"
                            ]
                            []
                        ]
                    ]
                , viewSignIn session page
                ]
            ]
        ]


viewSignIn : Maybe Session -> ActivePage -> Html msg
viewSignIn session page =
    let
        linkTo =
            navbarLink page
    in
        case session of
            Nothing ->
                a [ class "btn btn-outline-success ml-3", Route.href Route.Login ] [ text "Sign in" ]

            Just s ->
                ul [ class "navbar-nav" ] <|
                    [ li [ class "nav-item dropdown" ]
                        [ a [ class "nav-link dropdown-toggle", href "http://example.com", id "dropdown01", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "false", attribute "aria-expanded" "false" ] [ text s.user.username ]
                        , div [ class "dropdown-menu", attribute "aria-labelledby" "dropdown01" ]
                            [ a [ class "dropdown-item", Route.href (Route.Profile s.user.username) ]
                                [ text "My Profile" ]
                            , div [ class "dropdown-divider" ] []
                            , a [ class "dropdown-item", Route.href Route.Settings ]
                                [ text "Settings" ]
                            , a [ class "dropdown-item", Route.href Route.Logout ]
                                [ text "Sign Out" ]
                            ]
                        ]
                    ]


viewHeaderLinks : Maybe Session -> ActivePage -> List (Html msg)
viewHeaderLinks session page =
    let
        linkTo =
            navbarLink page
    in
        case session of
            Just _ ->
                [ linkTo Route.CreateDataset [ i [ class "ion-compose" ] [], text "Create Dataset" ]
                , linkTo Route.Explore [ i [ class "ion-compose" ] [], text "Explore" ]
                ]

            Nothing ->
                [ linkTo Route.Features [ i [ class "ion-compose" ] [], text "Features" ]
                , linkTo Route.Explore [ i [ class "ion-compose" ] [], text "Explore" ]
                , linkTo Route.Pricing [ i [ class "ion-compose" ] [], text "Pricing" ]
                ]


navbarLink : ActivePage -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


viewFooterLanding : Html msg
viewFooterLanding =
    footer [ class "container pt-4 my-md-5 pt-md-5 border-top" ]
        [ div [ class "row" ]
            [ div [ class "col-12 col-md" ]
                [ img [ class "mb-2", src "https://getbootstrap.com/assets/brand/bootstrap-solid.svg", alt "", width 24, height 24 ]
                    []
                , small [ class "d-block mb-3 text-muted" ]
                    [ text "© 2017-2018" ]
                ]
            , div [ class "col-6 col-md" ]
                [ h5 []
                    [ text "Features" ]
                , ul [ class "list-unstyled text-small" ]
                    [ li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Cool stuff" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Random feature" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Team feature" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Stuff for developers" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Another one" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Last time" ]
                        ]
                    ]
                ]
            , div [ class "col-6 col-md" ]
                [ h5 []
                    [ text "Resources" ]
                , ul [ class "list-unstyled text-small" ]
                    [ li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Resource" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Resource name" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Another resource" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Final resource" ]
                        ]
                    ]
                ]
            , div [ class "col-6 col-md" ]
                [ h5 []
                    [ text "About" ]
                , ul [ class "list-unstyled text-small" ]
                    [ li []
                        [ a [ class "text-muted", Route.href Route.OurVision ]
                            [ text "Our Vision" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Team" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Privacy" ]
                        ]
                    , li []
                        [ a [ class "text-muted", href "#" ]
                            [ text "Terms" ]
                        ]
                    ]
                ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer [ class "container pt-4 my-md-5 pt-md-5 border-top" ]
        [ p [ class "float-right" ]
            [ a [ href "#" ]
                [ text "Back to top" ]
            ]
        , p []
            [ text "© 2017 Monkey, Inc. · "
            , a
                [ href "#" ]
                [ text "Privacy" ]
            , text
                " · "
            , a [ href "#" ]
                [ text "Terms" ]
            ]
        ]


isActive : ActivePage -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( LandingPage, Route.Home ) ->
            True

        ( Dashboard, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        ( Features, Route.Features ) ->
            True

        ( Explore, Route.Explore ) ->
            True

        ( Pricing, Route.Pricing ) ->
            True

        ( CreateDataset, Route.CreateDataset ) ->
            True

        _ ->
            False


type Msg
    = SearchDataset


type alias Model =
    { searchTerm : String
    }


init : Model
init =
    { searchTerm = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchDataset ->
            ( model, Cmd.none )
