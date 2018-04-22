module Request.Dataset exposing (getDataset)

import Api.Object
import Api.Object.User
import Api.Object.LabelType
import Api.Object.DataType
import Api.Object.Dataset
import Api.Object.DatapointConnection
import Api.Object.DatapointEdge
import Api.Object.Image
import Api.Object.Video
import Api.Object.ImageClass
import Api.Object.ImageClassDefinition
import Api.Object.ImageBoundingBox
import Api.Object.ImageBoundingBoxDefinition
import Api.Interface
import Api.Interface.Datapoint
import Api.Union
import Api.Union.Label
import Api.Union.LabelDefinition
import Api.Query
import Graphqelm.OptionalArgument exposing (OptionalArgument(Present))
import Graphqelm.Operation exposing (RootMutation, RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Data.Dataset
    exposing
        ( Dataset
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
        )


dataset : SelectionSet Dataset Api.Object.Dataset
dataset =
    Api.Object.Dataset.selection Dataset
        |> with Api.Object.Dataset.id
        |> with Api.Object.Dataset.name
        |> with Api.Object.Dataset.description
        |> with Api.Object.Dataset.slug
        |> with Api.Object.Dataset.thumbnailUrl
        |> with (Api.Object.Dataset.owner owner)
        |> with (Api.Object.Dataset.dataType dataType)
        |> with (Api.Object.Dataset.labelType labelType)
        |> with (Api.Object.Dataset.labelDefinition labelDefinition |> Field.nonNullOrFail)
        |> with
            (Api.Object.Dataset.datapoints
                (\optionals ->
                    { optionals
                        | first = Present 10
                    }
                )
                datapoint
                |> Field.nonNullOrFail
            )


foldrValues : Maybe a -> List a -> List a
foldrValues item list =
    case item of
        Nothing ->
            list

        Just v ->
            v :: list


datapoint : SelectionSet (List DatapointRecord) Api.Object.DatapointConnection
datapoint =
    Api.Object.DatapointConnection.selection identity
        |> with (Api.Object.DatapointConnection.edges datapointEdgeSelection |> Field.nonNullOrFail)
        |> SelectionSet.map (List.foldr foldrValues [])


datapointEdgeSelection : SelectionSet DatapointRecord Api.Object.DatapointEdge
datapointEdgeSelection =
    Api.Object.DatapointEdge.selection identity
        |> with (Api.Object.DatapointEdge.node datapointNodeSelection |> Field.nonNullOrFail)


datapointNodeSelection : SelectionSet DatapointRecord Api.Interface.Datapoint
datapointNodeSelection =
    Api.Interface.Datapoint.selection DatapointRecord
        [ Api.Interface.Datapoint.onImage imageSelection
        , Api.Interface.Datapoint.onVideo videoSelection
        ]
        |> with Api.Interface.Datapoint.id


imageSelection : SelectionSet Datapoint Api.Object.Image
imageSelection =
    Api.Object.Image.selection ImageRecord
        |> with Api.Object.Image.storagePath
        |> with (Api.Object.Image.labels label)
        |> SelectionSet.map Image


videoSelection : SelectionSet Datapoint Api.Object.Video
videoSelection =
    Api.Object.Video.selection VideoRecord
        |> with Api.Object.Video.storagePath
        |> with (Api.Object.Video.labels label)
        |> SelectionSet.map Video


label : SelectionSet (Maybe Label) Api.Union.Label
label =
    Api.Union.Label.selection identity
        [ Api.Union.Label.onImageClass imageClassSelection
        , Api.Union.Label.onImageBoundingBox imageBoundingBoxSelection
        ]


imageClassSelection : SelectionSet Label Api.Object.ImageClass
imageClassSelection =
    Api.Object.ImageClass.selection ImageClassificationLabelRecord
        |> with Api.Object.ImageClass.class
        |> SelectionSet.map ImageClassificationLabel


imageBoundingBoxSelection : SelectionSet Label Api.Object.ImageBoundingBox
imageBoundingBoxSelection =
    Api.Object.ImageBoundingBox.selection ObjectDetectionLabelRecord
        |> with Api.Object.ImageBoundingBox.xMin
        |> with Api.Object.ImageBoundingBox.yMin
        |> with Api.Object.ImageBoundingBox.xMax
        |> with Api.Object.ImageBoundingBox.yMax
        |> with Api.Object.ImageBoundingBox.class
        |> SelectionSet.map ObjectDetectionLabel


labelDefinition : SelectionSet (Maybe LabelDefinition) Api.Union.LabelDefinition
labelDefinition =
    Api.Union.LabelDefinition.selection identity
        [ Api.Union.LabelDefinition.onImageClassDefinition imageClassSelectionDefinition
        , Api.Union.LabelDefinition.onImageBoundingBoxDefinition imageBoundingBoxSelectionDefinition
        ]


imageClassSelectionDefinition : SelectionSet LabelDefinition Api.Object.ImageClassDefinition
imageClassSelectionDefinition =
    Api.Object.ImageClassDefinition.selection ImageClassificationDefinitionRecord
        |> with (Api.Object.ImageClassDefinition.classes)
        |> SelectionSet.map ImageClassificationDefinition


imageBoundingBoxSelectionDefinition : SelectionSet LabelDefinition Api.Object.ImageBoundingBoxDefinition
imageBoundingBoxSelectionDefinition =
    Api.Object.ImageBoundingBoxDefinition.selection ObjectDetectionDefinitionRecord
        |> with (Api.Object.ImageBoundingBoxDefinition.classes)
        |> SelectionSet.map ObjectDetectionDefinition


owner : SelectionSet String Api.Object.User
owner =
    Api.Object.User.selection identity
        |> with (Api.Object.User.username)


labelType : SelectionSet String Api.Object.LabelType
labelType =
    Api.Object.LabelType.selection identity
        |> with Api.Object.LabelType.name


dataType : SelectionSet String Api.Object.DataType
dataType =
    Api.Object.DataType.selection identity
        |> with Api.Object.DataType.name


getDataset : Api.Query.DatasetRequiredArguments -> SelectionSet Dataset RootQuery
getDataset datasetPayload =
    Api.Query.selection identity
        |> with (Api.Query.dataset datasetPayload dataset |> Field.nonNullOrFail)
