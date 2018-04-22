module Page.Label exposing (view, init, Model, update, Msg)

import Graphqelm.Http
import Html.Styled exposing (..)
import Task exposing (Task)
import RemoteData exposing (RemoteData)
import Request.Helpers exposing (WebData, makeQuery, parseGraphQLError)
import Request.Dataset exposing (getDataset)
import Views.Page as Page
import Page.Error exposing (PageLoadError, pageLoadError)
import Data.Auth exposing (Session)
import Data.Dataset exposing (Dataset, LabelType(..), LabelDefinition(..), labelType, getImages)
import Page.Label.ImageClassification as ImageClassification exposing (Model, Msg, view, update, init)
import Page.Label.ObjectDetection as ObjectDetection exposing (Model, Msg, view, update, init)


type alias Model =
    { dataset : WebData Dataset
    , labelInterface : Maybe LabelInterface
    }


type LabelInterface
    = ImageClassificationInterface ImageClassification.Model
    | ObjectDetectionInterface ObjectDetection.Model


init : Session -> String -> String -> Task PageLoadError Model
init session username datasetName =
    let
        loadDataset =
            { name = datasetName, owner = username }
                |> getDataset
                |> makeQuery (Just session)
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
                    , labelInterface = initLabelInterface session dataset
                    }
                )


initLabelInterface : Session -> WebData Dataset -> Maybe LabelInterface
initLabelInterface session response =
    case response of
        RemoteData.Success dataset ->
            case ( labelType dataset, dataset.labelDefinition ) of
                ( Just ImageClassification, ImageClassificationDefinition def ) ->
                    let
                        maybeDatapoint =
                            getImages dataset |> List.head
                    in
                        case maybeDatapoint of
                            Just datapoint ->
                                Just (ImageClassificationInterface (ImageClassification.init session datapoint def))

                            Nothing ->
                                Nothing

                ( Just ObjectDetection, ObjectDetectionDefinition def ) ->
                    let
                        maybeDatapoint =
                            getImages dataset |> List.head
                    in
                        case maybeDatapoint of
                            Just datapoint ->
                                Just (ObjectDetectionInterface (ObjectDetection.init session datapoint def))

                            Nothing ->
                                Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


view : Session -> Model -> Html Msg
view session model =
    case model.labelInterface of
        Just (ImageClassificationInterface subModel) ->
            ImageClassification.view session subModel
                |> Html.Styled.map ImageClassificationMsg

        Just (ObjectDetectionInterface subModel) ->
            ObjectDetection.view session subModel
                |> Html.Styled.map ObjectDetectionMsg

        Nothing ->
            div [] [ text "Something went wrong." ]


type Msg
    = ImageClassificationMsg ImageClassification.Msg
    | ObjectDetectionMsg ObjectDetection.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case ( msg, model.labelInterface ) of
        ( ImageClassificationMsg subMsg, Just (ImageClassificationInterface subModel) ) ->
            ( model, Cmd.none )

        ( ObjectDetectionMsg subMsg, Just (ObjectDetectionInterface subModel) ) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
