module Page.Label.ObjectDetection exposing (view, init, Model, update, Msg)

import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Point as Point
import Annotation.Geometry.Types exposing (BoundingBox)
import Annotation.Svg as Svg
import Annotation.Viewer as Viewer exposing (Viewer)
import Html.Attributes exposing (id, attribute, style)
import Html exposing (Attribute)
import Html.Styled exposing (..)
import Pointer
import Svg exposing (..)
import Data.Auth exposing (Session)
import Data.Dataset exposing (ImageRecord, ObjectDetectionDefinitionRecord)
import Data.Label exposing (LabelInterfaceMsg(..))


type alias Model =
    { datapoint : ImageRecord
    , labelDefinition : ObjectDetectionDefinitionRecord
    , bbox : Maybe BoundingBox
    , dragState : DragState
    , viewer : Viewer
    }


type DragState
    = Up
    | DraggingFrom ( Float, Float )


init : Session -> ImageRecord -> ObjectDetectionDefinitionRecord -> Model
init session datapoint labelDefinition =
    Model datapoint labelDefinition Nothing Up Viewer.default


type Msg
    = PointerDownAt ( Float, Float )
    | PointerMoveAt ( Float, Float )
    | PointerUp


update : Msg -> Model -> ( ( Model, Cmd Msg ), LabelInterfaceMsg )
update msg model =
    case ( msg, model.dragState ) of
        ( PointerDownAt coordinates, _ ) ->
            let
                point =
                    Point.fromCoordinates coordinates

                bbox =
                    BoundingBox.fromPair ( point, point )
            in
                ( ( { model
                        | bbox = (Just bbox)
                        , dragState = (DraggingFrom coordinates)
                        , viewer = Viewer.default
                    }
                  , Cmd.none
                  )
                , NoOp
                )

        ( PointerMoveAt coordinates, DraggingFrom startCoordinates ) ->
            let
                ( startPoint, point ) =
                    ( Point.fromCoordinates startCoordinates
                    , Point.fromCoordinates coordinates
                    )

                bbox =
                    BoundingBox.fromPair ( startPoint, point )
            in
                ( ( { model | bbox = Just bbox }, Cmd.none ), NoOp )

        ( PointerUp, _ ) ->
            ( ( { model | dragState = Up }, Cmd.none ), NoOp )

        _ ->
            ( ( model, Cmd.none ), NoOp )


view : Session -> Model -> Html Msg
view session model =
    let
        htmlAttributes =
            [ Html.Attributes.id "viewer"

            -- use the elm-pep polyfill
            , Html.Attributes.attribute "elm-pep" "true"

            -- prevent default browser scrolls
            , Html.Attributes.style [ ( "touch-action", "none" ) ]
            ]
    in
        (Viewer.viewInWithDetails
            (htmlAttributes ++ pointerEvents)
            model.viewer
            (viewBBox model.bbox)
        )
            |> Html.Styled.fromUnstyled


viewBBox : Maybe BoundingBox -> Svg msg
viewBBox maybeBBox =
    maybeBBox
        |> Maybe.map Svg.boundingBox
        |> Maybe.withDefault (Svg.text "No bounding box")


pointerEvents : List (Html.Attribute Msg)
pointerEvents =
    [ Pointer.onDown (.pointer >> .offsetPos >> PointerDownAt)
    , Pointer.onMove (.pointer >> .offsetPos >> PointerMoveAt)
    , Pointer.onUp (always PointerUp)
    ]
