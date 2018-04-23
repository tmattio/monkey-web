module Page.Label.ImageClassification exposing (view, init, Model, update, Msg)

import Api.InputObject exposing (LabelInput, ImageClassInput, buildLabelInput, buildImageClassInput)
import Graphqelm.OptionalArgument exposing (OptionalArgument(Present))
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Data.Auth exposing (Session)
import Data.Dataset exposing (ImageRecord, ImageClassificationLabelRecord, ImageClassificationDefinitionRecord)
import Data.Label exposing (LabelInterfaceMsg(..))


type alias Model =
    { datapoint : ImageRecord
    , labelDefinition : ImageClassificationDefinitionRecord
    }


init : Session -> ImageRecord -> ImageClassificationDefinitionRecord -> Model
init session datapoint labelDefinition =
    { datapoint = datapoint
    , labelDefinition = labelDefinition
    }


type Msg
    = Label String


update : Msg -> Model -> ( ( Model, Cmd Msg ), LabelInterfaceMsg )
update msg model =
    case msg of
        Label label ->
            let
                labelInput =
                    buildLabelInput
                        (\optionals ->
                            { optionals
                                | imageClass =
                                    Present
                                        (buildImageClassInput { class = label })
                            }
                        )
            in
                ( ( model, Cmd.none ), LabelDatapoint labelInput )


view : Session -> Model -> Html Msg
view session model =
    div [ class "image-classification" ]
        [ viewImage model.datapoint.storagePath
        , viewLabels model.labelDefinition.classes
        ]


viewImage : String -> Html Msg
viewImage url =
    div [ class "image-viewer" ]
        [ img [ src url ] []
        ]


viewLabels : List String -> Html Msg
viewLabels labels =
    div [ class "labels" ] (List.map viewLabel labels)


viewLabel : String -> Html Msg
viewLabel label =
    button
        [ type_ "button"
        , class "btn btn-md btn-primary"
        , onClick (Label label)
        ]
        [ text label ]
