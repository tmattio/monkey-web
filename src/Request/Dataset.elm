module Request.Dataset exposing (getDataset)

import Api.Object
import Api.Interface
import Api.Object.User
import Api.Object.LabelType
import Api.Object.DataType
import Api.Object.Dataset
import Api.Object.DatapointConnection
import Api.Object.DatapointEdge
import Api.Object.Image
import Api.Object.Video
import Api.Interface.Datapoint
import Api.Query
import Graphqelm.OptionalArgument exposing (OptionalArgument(Present))
import Graphqelm.Operation exposing (RootMutation, RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Data.Dataset exposing (Dataset, Datapoint, DatapointUnion(..), ImageRecord, VideoRecord)


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


datapoint : SelectionSet (List Datapoint) Api.Object.DatapointConnection
datapoint =
    Api.Object.DatapointConnection.selection identity
        |> with (Api.Object.DatapointConnection.edges datapointEdgeSelection |> Field.nonNullOrFail)
        |> SelectionSet.map (List.foldr foldrValues [])


datapointEdgeSelection : SelectionSet Datapoint Api.Object.DatapointEdge
datapointEdgeSelection =
    Api.Object.DatapointEdge.selection identity
        |> with (Api.Object.DatapointEdge.node datapointNodeSelection |> Field.nonNullOrFail)


datapointNodeSelection : SelectionSet Datapoint Api.Interface.Datapoint
datapointNodeSelection =
    Api.Interface.Datapoint.selection Datapoint
        [ Api.Interface.Datapoint.onImage imageSelection
        , Api.Interface.Datapoint.onVideo videoSelection
        ]
        |> with Api.Interface.Datapoint.id


imageSelection : SelectionSet DatapointUnion Api.Object.Image
imageSelection =
    Api.Object.Image.selection ImageRecord
        |> with Api.Object.Image.storagePath
        |> SelectionSet.map Image


videoSelection : SelectionSet DatapointUnion Api.Object.Video
videoSelection =
    Api.Object.Video.selection VideoRecord
        |> with Api.Object.Video.storagePath
        |> SelectionSet.map Video


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
