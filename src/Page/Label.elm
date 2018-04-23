module Page.Label exposing (view, init, Model, update, subscriptions, Msg)

import Api.Mutation as Mutation
import Graphqelm.Operation exposing (RootMutation)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Graphqelm.Field as Field
import Graphqelm.Http
import List.Extra exposing (findIndex, getAt)
import Html.Styled exposing (..)
import Task exposing (Task)
import Keyboard
import RemoteData exposing (RemoteData)
import Request.Helpers exposing (WebData, makeQuery, makeMutation, parseGraphQLError)
import Request.Dataset
    exposing
        ( getDataset
        , datapoint
        , label
        , labelDefinition
        )
import Views.Page as Page
import Page.Error exposing (PageLoadError, pageLoadError)
import Data.Auth exposing (Session)
import Data.Dataset exposing (Dataset, Datapoint(..), DatapointRecord, ImageRecord, Label, LabelType(..), LabelDefinition(..), labelType, getImages)
import Data.Label exposing (LabelInterfaceMsg(..))
import Page.Label.ImageClassification as ImageClassification exposing (Model, Msg, view, update, init)
import Page.Label.ObjectDetection as ObjectDetection exposing (Model, Msg, view, update, init)


labelMutation :
    Mutation.UpdateLabelRequiredArguments
    -> SelectionSet (Maybe Label) Graphqelm.Operation.RootMutation
labelMutation labelPayload =
    Mutation.selection identity
        |> with (Mutation.updateLabel labelPayload Request.Dataset.label |> Field.nonNullOrFail)


type alias Model =
    { dataset : WebData Dataset
    , labelInterface : Maybe LabelInterface
    }


type LabelInterface
    = ImageClassificationInterface Dataset ImageClassification.Model (List ImageRecord)
    | ObjectDetectionInterface Dataset ObjectDetection.Model (List ImageRecord)


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
                    , labelInterface = (initLabelInterface session dataset)
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
                                (Just
                                    (ImageClassificationInterface
                                        dataset
                                        (ImageClassification.init session datapoint def)
                                        []
                                    )
                                )

                            Nothing ->
                                Nothing

                ( Just ObjectDetection, ObjectDetectionDefinition def ) ->
                    let
                        maybeDatapoint =
                            getImages dataset |> List.head
                    in
                        case maybeDatapoint of
                            Just datapoint ->
                                (Just
                                    (ObjectDetectionInterface
                                        dataset
                                        (ObjectDetection.init session datapoint def)
                                        []
                                    )
                                )

                            Nothing ->
                                Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


view : Session -> Model -> Html Msg
view session model =
    case model.labelInterface of
        Just (ImageClassificationInterface _ subModel _) ->
            ImageClassification.view session subModel
                |> Html.Styled.map ImageClassificationMsg

        Just (ObjectDetectionInterface _ subModel _) ->
            ObjectDetection.view session subModel
                |> Html.Styled.map ObjectDetectionMsg

        Nothing ->
            div [] [ text "Something went wrong." ]


type Msg
    = ImageClassificationMsg ImageClassification.Msg
    | ObjectDetectionMsg ObjectDetection.Msg
    | LabelCompleted (WebData (Maybe Label))
    | KeyPressed Keyboard.KeyCode


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case ( msg, model.labelInterface ) of
        ( ImageClassificationMsg subMsg, Just (ImageClassificationInterface dataset subModel viewedDatapoints) ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    ImageClassification.update subMsg subModel

                command =
                    Cmd.batch
                        [ labelCommand session model msgFromPage
                        , Cmd.map ImageClassificationMsg cmd
                        ]

                interface =
                    Just (ImageClassificationInterface dataset pageModel viewedDatapoints)
            in
                ( { model | labelInterface = interface }, command )

        ( ObjectDetectionMsg subMsg, Just (ObjectDetectionInterface dataset subModel viewedDatapoints) ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    ObjectDetection.update subMsg subModel

                command =
                    Cmd.batch
                        [ labelCommand session model msgFromPage
                        , Cmd.map ObjectDetectionMsg cmd
                        ]

                interface =
                    Just (ObjectDetectionInterface dataset pageModel viewedDatapoints)
            in
                ( { model | labelInterface = interface }, command )

        ( LabelCompleted response, Just labelInterface ) ->
            ( { model | labelInterface = Just (loadNextDatapoint labelInterface) }, Cmd.none )

        ( KeyPressed 39, Just labelInterface ) ->
            ( { model | labelInterface = Just (loadNextDatapoint labelInterface) }, Cmd.none )

        ( KeyPressed 37, Just labelInterface ) ->
            ( { model | labelInterface = Just (loadPreviousDatapoint labelInterface) }, Cmd.none )

        ( KeyPressed 68, Just labelInterface ) ->
            ( { model | labelInterface = Just (loadNextDatapoint labelInterface) }, Cmd.none )

        ( KeyPressed 65, Just labelInterface ) ->
            ( { model | labelInterface = Just (loadPreviousDatapoint labelInterface) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


labelCommand : Session -> Model -> LabelInterfaceMsg -> Cmd Msg
labelCommand session model msg =
    case ( msg, model.labelInterface ) of
        ( LabelDatapoint labelInput, Just (ImageClassificationInterface dataset subModel viewedDatapoints) ) ->
            { datapointId = subModel.datapoint.id
            , label = labelInput
            , name = dataset.slug
            , owner = dataset.owner
            }
                |> labelMutation
                |> makeMutation (Just session)
                |> Graphqelm.Http.send (RemoteData.fromResult >> LabelCompleted)

        _ ->
            Cmd.none


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest


loadPreviousDatapoint : LabelInterface -> LabelInterface
loadPreviousDatapoint labelInterface =
    loadRelativeDatapoint labelInterface (\idx -> idx - 1)


loadNextDatapoint : LabelInterface -> LabelInterface
loadNextDatapoint labelInterface =
    loadRelativeDatapoint labelInterface (\idx -> idx + 1)


loadRelativeDatapoint : LabelInterface -> (Int -> Int) -> LabelInterface
loadRelativeDatapoint labelInterface indexCalc =
    case labelInterface of
        ImageClassificationInterface dataset subModel viewed ->
            let
                images =
                    getImages dataset

                currentIndex =
                    images
                        |> findIndex (\image -> image.id == subModel.datapoint.id)

                nextImage =
                    case currentIndex of
                        Just idx ->
                            images |> getAt (indexCalc idx)

                        Nothing ->
                            Nothing

                newModel =
                    case nextImage of
                        Just img ->
                            { subModel | datapoint = img }

                        Nothing ->
                            subModel
            in
                ImageClassificationInterface dataset newModel viewed

        ObjectDetectionInterface dataset subModel viewed ->
            ObjectDetectionInterface dataset subModel viewed


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyPressed
