module Page.Dataset.Settings exposing (Model, Msg, init, update, view)

import Api.InputObject
import Api.Object
import Api.Object.Dataset
import Api.Query as Query
import Api.Mutation as Mutation
import Graphqelm.Http
import Graphqelm.OptionalArgument exposing (OptionalArgument(Present))
import Graphqelm.Operation exposing (RootMutation, RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, defaultValue, value, disabled, href, id, placeholder, type_)
import Html.Styled.Events exposing (onInput, onClick)
import RemoteData exposing (RemoteData)
import Validate exposing (Validator, ifBlank, validate)
import Views.Page as Page
import Page.Error exposing (PageLoadError, pageLoadError)
import Route exposing (Route)
import Task exposing (Task)
import Request.Helpers exposing (WebData, makeQuery, makeMutation)
import Data exposing (User, Session)
import Views.Form as Form


type alias DatasetResponse =
    { dataset : Dataset
    }


type alias Dataset =
    { name : String
    , description : Maybe String
    , slug : String
    , thumbnailUrl : Maybe String
    }


dataset : SelectionSet Dataset Api.Object.Dataset
dataset =
    Api.Object.Dataset.selection Dataset
        |> with Api.Object.Dataset.name
        |> with Api.Object.Dataset.description
        |> with Api.Object.Dataset.slug
        |> with Api.Object.Dataset.thumbnailUrl


query : Query.DatasetRequiredArguments -> SelectionSet DatasetResponse RootQuery
query datasetPayload =
    Query.selection DatasetResponse
        |> with (Query.dataset datasetPayload dataset |> Field.nonNullOrFail)


updateDatasetQuery : Mutation.UpdateDatasetRequiredArguments -> SelectionSet DatasetResponse RootMutation
updateDatasetQuery payload =
    Mutation.selection DatasetResponse
        |> with (Mutation.updateDataset payload dataset |> Field.nonNullOrFail)


deleteDatasetQuery : String -> String -> SelectionSet DatasetResponse RootMutation
deleteDatasetQuery owner datasetName =
    Mutation.selection DatasetResponse
        |> with (Mutation.deleteDataset { owner = owner, name = datasetName } dataset |> Field.nonNullOrFail)



-- MODEL --


type alias Model =
    { errors : List Error
    , datasetName : String
    , datasetOwner : String
    , name : String
    , description : String
    }


init : Session -> String -> String -> Task PageLoadError Model
init session owner datasetName =
    let
        handleLoadError _ =
            pageLoadError Page.Other "The datasests are currently unavailable."

        loadDataset =
            query { name = datasetName, owner = owner }
                |> makeQuery (Just session)
                |> Graphqelm.Http.toTask
                |> RemoteData.fromTask
    in
        loadDataset
            |> Task.mapError handleLoadError
            |> Task.map (decodeResponse datasetName owner)


decodeResponse : String -> String -> WebData DatasetResponse -> Model
decodeResponse datasetName owner response =
    case response of
        RemoteData.Success r ->
            { errors = []
            , datasetName = datasetName
            , datasetOwner = owner
            , name = r.dataset.slug
            , description = Maybe.withDefault "" r.dataset.description
            }

        _ ->
            { errors = []
            , datasetName = ""
            , datasetOwner = ""
            , name = ""
            , description = ""
            }



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "px-3 py-3 pt-md-5 pb-md-4" ]
                [ h1 [ class "display-4" ]
                    [ text "Dataset Settings" ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                    [ Form.viewErrors model.errors
                    , viewForm model
                    ]
                ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.Styled.form []
        [ fieldset []
            [ Form.input
                [ class "form-control-lg"
                , placeholder "Dataset Title"
                , onInput SetName
                , defaultValue model.name
                ]
                []
            , Form.input
                [ placeholder "What's this dataset about?"
                , onInput SetDescription
                , defaultValue model.description
                ]
                []
            , button [ class "btn btn-lg pull-xs-right btn-primary", onClick Save ]
                [ text "Update Dataset" ]
            , button
                [ class "btn btn-lg btn-danger", onClick DeleteDataset ]
                [ text "Delete Dataset" ]
            ]
        ]



-- UPDATE --


type Msg
    = Save
    | SetName String
    | SetDescription String
    | UpdateCompleted (WebData DatasetResponse)
    | DeleteDataset
    | DeleteDatasetCompleted (WebData DatasetResponse)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        Save ->
            case validate modelValidator model of
                [] ->
                    let
                        inputObject =
                            modelToMutationArguments model
                    in
                        ( { model | errors = [] }
                        , updateDatasetQuery inputObject
                            |> makeMutation (Just session)
                            |> Graphqelm.Http.send (RemoteData.fromResult >> UpdateCompleted)
                        )

                errors ->
                    ( { model | errors = errors }, Cmd.none )

        SetName name ->
            ( { model | name = name }, Cmd.none )

        SetDescription description ->
            ( { model | description = description }, Cmd.none )

        UpdateCompleted (RemoteData.Success response) ->
            ( model, Route.PreviewDataset model.datasetOwner response.dataset.slug |> Route.modifyUrl )

        UpdateCompleted _ ->
            ( { model | errors = model.errors ++ [ ( Form, "Server error while attempting to publish dataset" ) ] }, Cmd.none )

        DeleteDataset ->
            ( { model | errors = [] }
            , deleteDatasetQuery model.datasetOwner model.datasetName
                |> makeMutation (Just session)
                |> Graphqelm.Http.send (RemoteData.fromResult >> DeleteDatasetCompleted)
            )

        DeleteDatasetCompleted (RemoteData.Success r) ->
            ( model, Route.modifyUrl Route.Home )

        DeleteDatasetCompleted _ ->
            ( model, Cmd.none )



-- VALIDATION --


type Field
    = Form
    | NameField
    | DataTypeField
    | LabelTypeField


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .name ( NameField, "name can't be blank." )

        -- , ifBlank .dataType ( DataTypeField, "data type can't be blank." )
        -- , ifBlank .labelType ( LabelTypeField, "label type can't be blank." )
        ]



-- INTERNAL --


modelToMutationArguments : Model -> Mutation.UpdateDatasetRequiredArguments
modelToMutationArguments model =
    { name = model.datasetName
    , owner = model.datasetOwner
    , dataset =
        Api.InputObject.buildUpdateDatasetInput
            (\optionals -> { optionals | name = Present model.name, description = Present model.description })
    }
