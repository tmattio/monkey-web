module Data.Dataset
    exposing
        ( Dataset
        , DatasetType(..)
        , LabelType
        , Datapoint
        , DatapointUnion(..)
        , ImageRecord
        , VideoRecord
        , datasetType
        , labelType
        , getImages
        )

import Api.Scalar


type alias Dataset =
    { id : Api.Scalar.Id
    , name : String
    , description : Maybe String
    , slug : String
    , thumbnailUrl : Maybe String
    , owner : String
    , datasetType : String
    , labelType : String
    , datapoints : List Datapoint
    }


type DatasetType
    = ImageDataset
    | VideoDataset


type LabelType
    = ImageClassification
    | ObjectDetection


type alias Datapoint =
    { datapoint : Maybe DatapointUnion
    , id : Api.Scalar.Id
    }


type DatapointUnion
    = Image ImageRecord
    | Video VideoRecord


type alias ImageRecord =
    { storagePath : String
    }


type alias VideoRecord =
    { storagePath : String }


datasetType : Dataset -> Maybe DatasetType
datasetType dataset =
    case dataset.datasetType of
        "Image" ->
            Just ImageDataset

        "Video" ->
            Just VideoDataset

        _ ->
            Nothing


labelType : Dataset -> Maybe LabelType
labelType dataset =
    case dataset.labelType of
        "Image Classification" ->
            Just ImageClassification

        "Object Detection" ->
            Just ObjectDetection

        _ ->
            Nothing


filterImages : Datapoint -> List ImageRecord -> List ImageRecord
filterImages datapoint list =
    case datapoint.datapoint of
        Just (Image record) ->
            record :: list

        _ ->
            list


getImages : Dataset -> List ImageRecord
getImages dataset =
    List.foldr filterImages [] dataset.datapoints
