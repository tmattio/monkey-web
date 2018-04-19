module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Value)
import Route exposing (..)
import Task exposing (Task)
import Data exposing (Session, sessionDecoder)
import Page.Static.NotFound as NotFound
import Page.Static.LandingPage as LandingPage
import Page.Static.Features as Features
import Page.Static.OurVision as OurVision
import Page.Static.Pricing as Pricing
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Page.Dashboard as Dashboard
import Page.Profile as Profile
import Page.Explore as Explore
import Page.Search as Search
import Page.Login as Login
import Page.Register as Register
import Page.Dataset.Preview as PreviewDataset
import Page.Dataset.Settings as SettingsDataset
import Page.Dataset.Create as CreateDataset
import Page.Settings as Settings
import Page.Label as Label
import Views.Page as Page exposing (ActivePage, Layout)
import Ports


---- MODEL ----


type alias Model =
    { pageState : PageState
    , session : Maybe Session
    }


type Page
    = Blank
    | LandingPage
    | Features
    | OurVision
    | Search
    | Pricing
    | NotFound
    | Login Login.Model
    | Register Register.Model
    | Dashboard Dashboard.Model
    | Profile Profile.Model
    | Explore Explore.Model
    | PreviewDataset PreviewDataset.Model
    | SettingsDataset SettingsDataset.Model
    | CreateDataset CreateDataset.Model
    | Settings Settings.Model
    | Error PageLoadError
    | Label Label.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page



---- UPDATE ----


type Msg
    = SetRoute (Maybe Route)
    | DashboardLoaded (Result PageLoadError Dashboard.Model)
    | LabelLoaded (Result PageLoadError Label.Model)
    | ProfileLoaded (Result PageLoadError Profile.Model)
    | ExploreLoaded (Result PageLoadError Explore.Model)
    | PreviewDatasetLoaded (Result PageLoadError PreviewDataset.Model)
    | SettingsDatasetLoaded (Result PageLoadError SettingsDataset.Model)
    | CreateDatasetLoaded (Result PageLoadError CreateDataset.Model)
    | SettingsLoaded (Result PageLoadError Settings.Model)
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | ExploreMsg Explore.Msg
    | PreviewDatasetMsg PreviewDataset.Msg
    | SettingsDatasetMsg SettingsDataset.Msg
    | CreateDatasetMsg CreateDataset.Msg
    | SettingsMsg Settings.Msg
    | DashboardMsg Dashboard.Msg
    | LabelMsg Label.Msg
    | ProfileMsg Profile.Msg
    | SetSession (Maybe Session)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute route model =
    let
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getPage model.pageState) }, Task.attempt toMsg task )
    in
        case route of
            Nothing ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )

            Just Route.Home ->
                case model.session of
                    Just s ->
                        transition DashboardLoaded (Dashboard.init model.session)

                    Nothing ->
                        ( { model | pageState = Loaded LandingPage }, Cmd.none )

            Just (Route.Label owner datasetName)->
                transition LabelLoaded (Label.init model.session owner datasetName)

            Just Route.Features ->
                ( { model | pageState = Loaded Features }, Cmd.none )

            Just Route.OurVision ->
                ( { model | pageState = Loaded OurVision }, Cmd.none )

            Just Route.Search ->
                ( { model | pageState = Loaded Search }, Cmd.none )

            Just Route.Login ->
                ( { model | pageState = Loaded (Login Login.initialModel) }, Cmd.none )

            Just Route.Logout ->
                ( { model | session = Nothing }
                , Cmd.batch
                    [ Ports.storeSession Nothing
                    , Route.modifyUrl Route.Home
                    ]
                )

            Just Route.Register ->
                ( { model | pageState = Loaded (Register Register.initialModel) }, Cmd.none )

            Just Route.Pricing ->
                ( { model | pageState = Loaded Pricing }, Cmd.none )

            Just Route.Explore ->
                transition ExploreLoaded (Explore.init model.session)

            Just (Route.PreviewDataset username datasetName) ->
                transition PreviewDatasetLoaded (PreviewDataset.init model.session username datasetName)

            Just (Route.Profile username) ->
                transition ProfileLoaded (Profile.init model.session username)

            Just Route.CreateDataset ->
                case model.session of
                    Just s ->
                        transition CreateDatasetLoaded (CreateDataset.init s)

                    Nothing ->
                        pageErrored model
                            Page.Other
                            "You must be signed in to edit datasets."

            Just (Route.SettingsDataset owner datasetName) ->
                case model.session of
                    Just s ->
                        transition SettingsDatasetLoaded (SettingsDataset.init s owner datasetName)

                    Nothing ->
                        pageErrored model
                            Page.Other
                            "You must be signed in to edit datasets."

            Just Route.Settings ->
                case model.session of
                    Just s ->
                        transition SettingsLoaded (Settings.init s)

                    Nothing ->
                        pageErrored model
                            Page.Other
                            "You must be signed in to edit your profile."


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Error.pageLoadError activePage errorMessage
    in
        ( { model | pageState = Loaded (Error error) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
        case ( msg, page ) of
            -- Update for page transitions
            ( SetRoute route, _ ) ->
                setRoute route model

            ( ExploreLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Explore subModel) }, Cmd.none )

            ( ExploreLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Error error) }, Cmd.none )

            ( ExploreMsg subMsg, Explore subModel ) ->
                toPage Explore ExploreMsg (Explore.update) subMsg subModel

            ( DashboardLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Dashboard subModel) }, Cmd.none )

            ( DashboardLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Error error) }, Cmd.none )

            ( DashboardMsg subMsg, Dashboard subModel ) ->
                toPage Dashboard DashboardMsg (Dashboard.update) subMsg subModel

            ( LabelLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Label subModel) }, Cmd.none )

            ( LabelLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Error error) }, Cmd.none )

            ( LabelMsg subMsg, Label subModel ) ->
                toPage Label LabelMsg (Label.update) subMsg subModel

            ( ProfileLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Profile subModel) }, Cmd.none )

            ( ProfileLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Error error) }, Cmd.none )

            ( ProfileMsg subMsg, Profile subModel ) ->
                toPage Profile ProfileMsg (Profile.update) subMsg subModel

            ( PreviewDatasetLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (PreviewDataset subModel) }, Cmd.none )

            ( PreviewDatasetLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Error error) }, Cmd.none )

            ( PreviewDatasetMsg subMsg, PreviewDataset subModel ) ->
                toPage PreviewDataset PreviewDatasetMsg (PreviewDataset.update) subMsg subModel

            ( CreateDatasetLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (CreateDataset subModel) }, Cmd.none )

            ( CreateDatasetLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Error error) }, Cmd.none )

            ( CreateDatasetMsg subMsg, CreateDataset subModel ) ->
                case model.session of
                    Just s ->
                        toPage CreateDataset CreateDatasetMsg (CreateDataset.update s) subMsg subModel

                    Nothing ->
                        ( model, Cmd.none )

            ( SettingsDatasetLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (SettingsDataset subModel) }, Cmd.none )

            ( SettingsDatasetLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Error error) }, Cmd.none )

            ( SettingsDatasetMsg subMsg, SettingsDataset subModel ) ->
                case model.session of
                    Just s ->
                        toPage SettingsDataset SettingsDatasetMsg (SettingsDataset.update s) subMsg subModel

                    Nothing ->
                        ( model, Cmd.none )

            ( SettingsLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Settings subModel) }, Cmd.none )

            ( SettingsLoaded (Err error), _ ) ->
                ( { model | pageState = Loaded (Error error) }, Cmd.none )

            ( SettingsMsg subMsg, Settings subModel ) ->
                case model.session of
                    Just s ->
                        let
                            ( ( pageModel, cmd ), msgFromPage ) =
                                Settings.update s subMsg subModel

                            newModel =
                                case msgFromPage of
                                    Settings.NoOp ->
                                        model

                                    Settings.SetSession (Just session) ->
                                        { model | session = Just session }

                                    Settings.SetSession Nothing ->
                                        { model | session = Nothing }
                        in
                            ( { newModel | pageState = Loaded (Settings pageModel) }, Cmd.map SettingsMsg cmd )

                    Nothing ->
                        ( model, Cmd.none )

            ( LoginMsg subMsg, Login subModel ) ->
                let
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Login.update model.session subMsg subModel

                    newModel =
                        case msgFromPage of
                            Login.NoOp ->
                                model

                            Login.SetSession session ->
                                { model | session = Just session }
                in
                    ( { newModel | pageState = Loaded (Login pageModel) }, Cmd.map LoginMsg cmd )

            ( RegisterMsg subMsg, Register subModel ) ->
                let
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Register.update model.session subMsg subModel

                    newModel =
                        case msgFromPage of
                            Register.NoOp ->
                                model

                            Register.SetSession session ->
                                { model | session = Just session }
                in
                    ( { newModel | pageState = Loaded (Register pageModel) }, Cmd.map RegisterMsg cmd )

            ( SetSession session, _ ) ->
                let
                    cmd =
                        -- If we just signed out, then redirect to Home.
                        if model.session /= Nothing && session == Nothing then
                            Route.modifyUrl Route.Home
                        else
                            Cmd.none
                in
                    ( { model | session = session }, cmd )

            ( _, NotFound ) ->
                -- Disregard incoming messages when we're on the
                -- NotFound page.
                ( model, Cmd.none )

            ( _, _ ) ->
                -- Disregard incoming messages that arrived for the wrong page
                ( model, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page
                |> toUnstyled

        TransitioningFrom page ->
            viewPage model.session True page
                |> toUnstyled


viewPage : Maybe Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        layout =
            Page.layout session isLoading
    in
        case page of
            NotFound ->
                NotFound.view session
                    |> layout Page.Landing Page.Other

            Blank ->
                -- This is for the very intial page load, while we are loading
                -- data via HTTP. We could also render a spinner here.
                text ""
                    |> layout Page.Blank Page.Other

            Error subModel ->
                Error.view session subModel
                    |> layout Page.Landing Page.Other

            LandingPage ->
                LandingPage.view session
                    |> layout Page.Landing Page.LandingPage

            Features ->
                Features.view session
                    |> layout Page.Landing Page.Features

            OurVision ->
                OurVision.view session
                    |> layout Page.Landing Page.OurVision

            Search ->
                Search.view session
                    |> layout Page.Fluid Page.Search

            Login subModel ->
                Login.view session subModel
                    |> layout Page.FullPage Page.Login
                    |> Html.Styled.map LoginMsg

            Register subModel ->
                Register.view session subModel
                    |> layout Page.FullPage Page.Register
                    |> Html.Styled.map RegisterMsg

            Pricing ->
                Pricing.view session
                    |> layout Page.Landing Page.Pricing

            Explore subModel ->
                Explore.view session subModel
                    |> layout Page.Landing Page.Explore
                    |> Html.Styled.map ExploreMsg

            Dashboard subModel ->
                Dashboard.view session subModel
                    |> layout Page.Fluid Page.Dashboard
                    |> Html.Styled.map DashboardMsg

            Label subModel ->
                Label.view session subModel
                    |> layout Page.Fluid Page.Label
                    |> Html.Styled.map LabelMsg

            Profile subModel ->
                Profile.view session subModel
                    |> layout Page.Fluid Page.Profile
                    |> Html.Styled.map ProfileMsg

            CreateDataset subModel ->
                case session of
                    Just s ->
                        CreateDataset.view s subModel
                            |> layout Page.Fluid Page.CreateDataset
                            |> Html.Styled.map CreateDatasetMsg

                    Nothing ->
                        Error.view session (pageLoadError Page.CreateDataset "Cannot access")
                            |> layout Page.Landing Page.Other

            Settings subModel ->
                case session of
                    Just s ->
                        Settings.view s subModel
                            |> layout Page.Fluid Page.Settings
                            |> Html.Styled.map SettingsMsg

                    Nothing ->
                        Error.view session (pageLoadError Page.Settings "Cannot access")
                            |> layout Page.Landing Page.Other

            PreviewDataset subModel ->
                PreviewDataset.view session subModel
                    |> layout Page.Fluid Page.PreviewDataset
                    |> Html.Styled.map PreviewDatasetMsg

            SettingsDataset subModel ->
                case session of
                    Just s ->
                        SettingsDataset.view s subModel
                            |> layout Page.Fluid Page.SettingsDataset
                            |> Html.Styled.map SettingsDatasetMsg

                    Nothing ->
                        Error.view session (pageLoadError Page.SettingsDataset "Cannot access")
                            |> layout Page.Landing Page.Other



---- SUBSCRIPTIONS ----


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions (getPage model.pageState)
        , Sub.map SetSession sessionChange
        ]


sessionChange : Sub (Maybe Session)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue sessionDecoder >> Result.toMaybe)


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        _ ->
            Sub.none



---- PROGRAM ----


initialPage : Page
initialPage =
    Blank


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        , session = decodeUserFromJson val
        }


decodeUserFromJson : Value -> Maybe Session
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString sessionDecoder >> Result.toMaybe)


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
