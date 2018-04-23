module Page.Dataset.Insights exposing (view, update, Model, Msg, init)

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
    main_ [ attribute "role" "main" ]
        [ viewTitle "Dataset Insights"
        , div [ class "container" ]
            [ div [ class "card mb-3" ]
                [ div [ class "card-header" ]
                    [ i [ class "fa fa-area-chart" ]
                        []
                    , text
                        "Area Chart Example"
                    ]
                , div [ class "card-body" ]
                    [ div [ class "chartjs-size-monitor", style [ ( "position", "absolute" ), ( "left", "0px" ), ( "top", "0px" ), ( "right", "0px" ), ( "bottom", "0px" ), ( "overflow", "hidden" ), ( "pointer-events", "none" ), ( "visibility", "hidden" ), ( "z-index", "-1" ) ] ]
                        [ div [ class "chartjs-size-monitor-expand", style [ ( "position", "absolute" ), ( "left", "0" ), ( "top", "0" ), ( "right", "0" ), ( "bottom", "0" ), ( "overflow", "hidden" ), ( "pointer-events", "none" ), ( "visibility", "hidden" ), ( "z-index", "-1" ) ] ]
                            [ div [ style [ ( "position", "absolute" ), ( "width", "1000000px" ), ( "height", "1000000px" ), ( "left", "0" ), ( "top", "0" ) ] ]
                                []
                            ]
                        , div [ class "chartjs-size-monitor-shrink", style [ ( "position", "absolute" ), ( "left", "0" ), ( "top", "0" ), ( "right", "0" ), ( "bottom", "0" ), ( "overflow", "hidden" ), ( "pointer-events", "none" ), ( "visibility", "hidden" ), ( "z-index", "-1" ) ] ]
                            [ div [ style [ ( "position", "absolute" ), ( "width", "200%" ), ( "height", "200%" ), ( "left", "0" ), ( "top", "0" ) ] ]
                                []
                            ]
                        ]
                    , canvas [ id "myAreaChart", width 1358, height 406, class "chartjs-render-monitor", style [ ( "display", "block" ), ( "height", "203px" ), ( "width", "679px" ) ] ]
                        []
                    ]
                , div [ class "card-footer small text-muted" ]
                    [ text "Updated yesterday at 11:59 PM" ]
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

