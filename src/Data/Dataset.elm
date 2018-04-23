module Data.Dataset
    exposing
        ( Dataset
        , DatasetType(..)
        , LabelType(..)
        , Datapoint(..)
        , DatapointRecord
        , ImageRecord
        , VideoRecord
        , Label(..)
        , ImageClassificationLabelRecord
        , ObjectDetectionLabelRecord
        , LabelDefinition(..)
        , ImageClassificationDefinitionRecord
        , ObjectDetectionDefinitionRecord
        , datasetType
        , labelType
        , labelTypeFromString
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
    , labelDefinition : LabelDefinition
    , datapoints : List DatapointRecord
    }


type DatasetType
    = ImageDataset
    | VideoDataset


type LabelType
    = ImageClassification
    | ObjectDetection


type Datapoint
    = Image ImageRecord
    | Video VideoRecord


type alias DatapointRecord =
    { datapoint : Maybe Datapoint
    , id : Api.Scalar.Id
    }


type alias ImageRecord =
    { storagePath : String
    , label : List (Maybe Label)
    , id : Api.Scalar.Id
    }


type alias VideoRecord =
    { storagePath : String
    , labels : List (Maybe Label)
    , id : Api.Scalar.Id
    }


type Label
    = ImageClassificationLabel ImageClassificationLabelRecord
    | ObjectDetectionLabel ObjectDetectionLabelRecord


type alias ImageClassificationLabelRecord =
    { class : String
    }


type alias ObjectDetectionLabelRecord =
    { xMin : Float
    , yMin : Float
    , xMax : Float
    , yMax : Float
    , class : String
    }


type LabelDefinition
    = ImageClassificationDefinition ImageClassificationDefinitionRecord
    | ObjectDetectionDefinition ObjectDetectionDefinitionRecord


type alias ImageClassificationDefinitionRecord =
    { classes : List String
    }


type alias ObjectDetectionDefinitionRecord =
    { classes : List String
    }


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
    case ( dataset.labelType, dataset.labelDefinition ) of
        ( "Image Classification", ImageClassificationDefinition _ ) ->
            Just ImageClassification

        ( "Image Object Detection", ObjectDetectionDefinition _ ) ->
            Just ObjectDetection

        _ ->
            Nothing


labelTypeFromString : String -> Maybe LabelType
labelTypeFromString type_ =
    case type_ of
        "Image Classification" ->
            Just ImageClassification

        "Image Object Detection" ->
            Just ObjectDetection

        _ ->
            Nothing


filterImages : DatapointRecord -> List ImageRecord -> List ImageRecord
filterImages datapoint list =
    case datapoint.datapoint of
        Just (Image record) ->
            record :: list

        _ ->
            list


getImages : Dataset -> List ImageRecord
getImages dataset =
    List.foldr filterImages [] dataset.datapoints
