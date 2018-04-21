module Page.Dataset.Preview exposing (view, update, Model, Msg, init)

import Api.Object
import Api.Object.DatasetExportPayload
import Api.Mutation as Mutation
import Graphqelm.Http
import Graphqelm.Operation exposing (RootQuery, RootMutation)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with, map)
import Graphqelm.Field as Field
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Task exposing (Task)
import RemoteData exposing (RemoteData)
import Request.Helpers exposing (WebData, makeQuery, makeMutation, parseGraphQLError)
import Request.Dataset exposing (getDataset)
import Data.Auth exposing (Session)
import Data.Dataset exposing (Dataset)
import Page.Error exposing (PageLoadError, pageLoadError)
import Views.Page as Page
import Views.Error exposing (viewWithError)
import Views.Title exposing (viewTitle)
import Views.PreviewDataset exposing (previewDataset)
import Route exposing (Route)


-- DATA --


type alias UrlResponse =
    String


exportQuery : Mutation.ExportDatasetRequiredArguments -> SelectionSet UrlResponse RootMutation
exportQuery datasetPayload =
    Mutation.selection identity
        |> with (Mutation.exportDataset datasetPayload url |> Field.nonNullOrFail)


url : SelectionSet UrlResponse Api.Object.DatasetExportPayload
url =
    Api.Object.DatasetExportPayload.selection identity
        |> with (Api.Object.DatasetExportPayload.exportUrl |> Field.nonNullOrFail)



---- MODEL ----


type alias Model =
    { owner : String
    , datasetName : String
    , dataset : WebData Dataset
    , exportedDataset : WebData UrlResponse
    }


init : Maybe Session -> String -> String -> Task PageLoadError Model
init session username datasetName =
    let
        loadDataset =
            { name = datasetName, owner = username }
                |> getDataset
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
                    , datasetName = datasetName
                    , exportedDataset = RemoteData.NotAsked
                    }
                )



--  VIEW --


view : Maybe Session -> Model -> Html Msg
view session model =
    let
        successView dataset =
            div []
                [ a
                    [ Route.href (Route.SettingsDataset model.owner dataset.slug), class "btn btn-lg btn-success" ]
                    [ text "Settings" ]
                , a
                    [ Route.href (Route.Label model.owner dataset.slug), class "btn btn-lg btn-primary" ]
                    [ text "Label" ]
                , viewExportDataset model
                , div
                    [ class "row" ]
                    [ previewDataset dataset
                    ]
                ]

        datasetView =
            viewWithError model.dataset successView
    in
        main_ [ attribute "role" "main" ]
            [ viewTitle "Preview Dataset"
            , div [ class "container" ]
                [ datasetView
                ]
            ]


viewExportDataset : Model -> Html Msg
viewExportDataset model =
    case model.exportedDataset of
        RemoteData.Success url ->
            div []
                [ text "Your Dataset is ready!"
                , a
                    [ href url
                    , target "_blank"
                    ]
                    [ text "Click Here" ]
                ]

        RemoteData.NotAsked ->
            div [ class "btn btn-lg btn-info", onClick ExportDataset ]
                [ text "Export" ]

        RemoteData.Loading ->
            div [] [ text "Export in progres..." ]

        RemoteData.Failure _ ->
            div [] [ text "Something went wrong :/" ]


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
                            [ a [ Route.href (Route.PreviewDataset dataset.owner dataset.slug) ]
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
    = ExportDataset
    | ExportCompleted (WebData UrlResponse)


update : Maybe Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        ExportDataset ->
            let
                inputObject =
                    modelToExportInputObject model
            in
                ( model
                , exportQuery inputObject
                    |> makeMutation session
                    |> Graphqelm.Http.send (RemoteData.fromResult >> ExportCompleted)
                )

        ExportCompleted dataset ->
            ( { model | exportedDataset = dataset }, Cmd.none )


modelToExportInputObject : Model -> Mutation.ExportDatasetRequiredArguments
modelToExportInputObject model =
    { owner = model.owner
    , name = model.datasetName
    }
