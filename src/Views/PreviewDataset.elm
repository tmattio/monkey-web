module Views.PreviewDataset
    exposing
        ( previewDataset
        , previewImages
        , previewImage
        )

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Data.Dataset
    exposing
        ( Dataset
        , ImageRecord
        , DatapointUnion(..)
        , DatasetType(..)
        , datasetType
        , getImages
        )


previewImages : List ImageRecord -> Html msg
previewImages images =
    div [] (List.map previewImage images)


previewImage : ImageRecord -> Html msg
previewImage image =
    img [ src image.storagePath ] []


previewDataset : Dataset -> Html msg
previewDataset dataset =
    case datasetType dataset of
        Just ImageDataset ->
            getImages dataset
                |> previewImages

        Just _ ->
            div [] [ text "Dataset Type not supported" ]

        Nothing ->
            div [] [ text "Internal Error - The dataset type is not recognized" ]
