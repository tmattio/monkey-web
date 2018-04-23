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
        , Datapoint(..)
        , DatasetType(..)
        , datasetType
        , getImages
        )


previewImages : List ImageRecord -> Html msg
previewImages images =
    div [ class "row text-center text-lg-left" ]
        (List.map previewImage images)


previewImage : ImageRecord -> Html msg
previewImage image =
    div [ class "col-lg-3 col-md-4 col-xs-6" ]
        [ img [ class "img-fluid img-thumbnail", src image.storagePath, alt "" ] []
        ]


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
