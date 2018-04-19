module Page.Dataset.Preview exposing (view, update, Model, Msg, init)

import Api.Object
import Api.Object.Dataset as Dataset
import Api.Query as Query
import Graphqelm.Http
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Task exposing (Task)
import RemoteData exposing (RemoteData)
import Request.Helpers exposing (WebData, makeQuery)
import Data exposing (Session)
import Page.Error exposing (PageLoadError, pageLoadError)
import Views.Page as Page
import Route exposing (Route)


-- DATA --


type alias DatasetResponse =
    { dataset : Dataset
    }


type alias Dataset =
    { name : String
    , description : Maybe String
    , slug : String
    , thumbnailUrl : Maybe String
    }


dataset : SelectionSet Dataset Api.Object.Dataset
dataset =
    Dataset.selection Dataset
        |> with Dataset.name
        |> with Dataset.description
        |> with Dataset.slug
        |> with Dataset.thumbnailUrl


query : Query.DatasetRequiredArguments -> SelectionSet DatasetResponse RootQuery
query datasetPayload =
    Query.selection DatasetResponse
        |> with (Query.dataset datasetPayload dataset |> Field.nonNullOrFail)



---- MODEL ----


type alias Model =
    { owner : String
    , dataset : WebData DatasetResponse
    }


init : Maybe Session -> String -> String -> Task PageLoadError Model
init session username datasetName =
    let
        loadDataset =
            { name = datasetName, owner = username }
                |> query
                |> makeQuery session
                |> Graphqelm.Http.toTask
                |> RemoteData.fromTask

        handleLoadError _ =
            pageLoadError Page.Other "The datasests are currently unavailable."
    in
        loadDataset
            |> Task.mapError handleLoadError
            |> Task.map
                (\dataset ->
                    { dataset = dataset
                    , owner = username
                    }
                )



--  VIEW --


view : Maybe Session -> Model -> Html Msg
view session model =
    let
        ( datasetView, settingLink, labelLink ) =
            case model.dataset of
                RemoteData.Success r ->
                    ( div [ class "row" ]
                        [ datasetCard r.dataset
                        ]
                    , Route.href (Route.SettingsDataset model.owner r.dataset.slug)
                    , Route.href (Route.Label model.owner r.dataset.slug)
                    )

                _ ->
                    ( div [ class "home-page" ]
                        [ text "An error occured while fetching the dataset"
                        ]
                    , href "#"
                    , href "#"
                    )
    in
        main_ [ attribute "role" "main" ]
            [ section [ class "jumbotron text-center" ]
                [ div [ class "container" ]
                    [ h1 [ class "jumbotron-heading" ]
                        [ text "Preview Dataset" ]
                    , p [ class "lead text-muted" ]
                        [ text "Something short and leading about the collection below—its contents, the creator, etc. Make it short and sweet, but not too short so folks don't simply skip over it entirely." ]
                    , a
                        [ settingLink, class "btn btn-lg btn-success" ]
                        [ text "Settings" ]
                    , a
                        [ labelLink, class "btn btn-lg btn-primary" ]
                        [ text "Label" ]
                    , a
                        [ class "btn btn-lg btn-info" ]
                        [ text "Export" ]
                    ]
                ]
            , div [ class "album py-5" ]
                [ div [ class "container" ]
                    [ datasetView
                    ]
                ]
            ]


datasetCard : Dataset -> Html Msg
datasetCard dataset =
    let
        datasetImage =
            case dataset.thumbnailUrl of
                Just url ->
                    url

                Nothing ->
                    "https://picsum.photos/286/180"
    in
        div [ class "col-md-4" ]
            [ div [ class "card mb-4 box-shadow" ]
                [ img [ class "card-img-top", src datasetImage, alt "Card image cap" ]
                    []
                , div [ class "card-body" ]
                    [ p [ class "card-text" ]
                        [ text (Maybe.withDefault "" dataset.description) ]
                    , div [ class "d-flex justify-content-between align-items-center" ]
                        [ div [ class "btn-group" ]
                            [ a [ Route.href (Route.PreviewDataset "tmattio" dataset.slug) ]
                                [ button [ type_ "button", class "btn btn-sm btn-outline-secondary" ]
                                    [ text "View" ]
                                ]
                            , button [ type_ "button", class "btn btn-sm btn-outline-secondary" ]
                                [ text "Star" ]
                            ]
                        , small [ class "text-muted" ]
                            [ text "127K samples" ]
                        ]
                    ]
                ]
            ]



-- UPDATE --


type Msg
    = Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )
