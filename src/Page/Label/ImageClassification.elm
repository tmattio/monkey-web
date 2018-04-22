module Page.Label.ImageClassification exposing (view, init, Model, update, Msg)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Data.Auth exposing (Session)
import Data.Dataset exposing (ImageRecord, ImageClassificationDefinitionRecord)
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
    = PointerDownAt ( Float, Float )
    | PointerMoveAt ( Float, Float )
    | PointerUp


update : Msg -> Model -> ( ( Model, Cmd Msg ), LabelInterfaceMsg )
update msg model =
    case msg of
        _ ->
            ( ( model, Cmd.none ), NoOp )


view : Session -> Model -> Html Msg
view session model =
    div [ class "image-classification" ]
        [ viewImage model.datapoint.storagePath
        , viewLabels model.labelDefinition.classes
        ]


viewImage : String -> Html msg
viewImage url =
    div [ class "image-viewer" ]
        [ img [ src url ] []
        ]


viewLabels : List String -> Html msg
viewLabels labels =
    div [ class "labels" ] (List.map text labels)
