module Page.Dataset.Create exposing (Model, Msg, init, update, view)

import Api.InputObject
import Api.Object
import Api.Object.Dataset
import Api.Object.DataType
import Api.Object.LabelType
import Api.Object.User
import Api.Query as Query
import Api.Mutation as Mutation
import Graphqelm.Http
import Graphqelm.OptionalArgument exposing (OptionalArgument(Present))
import Graphqelm.Operation exposing (RootMutation, RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, defaultValue, value, disabled, href, id, placeholder, type_)
import Html.Styled.Events exposing (onInput, onSubmit)
import RemoteData exposing (RemoteData)
import Validate exposing (Validator, ifBlank, validate)
import Views.Page as Page
import Page.Error exposing (PageLoadError, pageLoadError)
import Route exposing (Route)
import Task exposing (Task)
import Request.Helpers exposing (WebData, makeQuery, makeMutation)
import Data exposing (User, Session)
import Views.Form as Form


type alias Username =
    { username : String
    }


type alias Dataset =
    { user : Username
    , slug : String
    }


type alias Response =
    { dataset : Dataset
    }


type alias TypeResponse =
    { dataTypes : List DataType
    }


dataset : SelectionSet Dataset Api.Object.Dataset
dataset =
    Api.Object.Dataset.selection Dataset
        |> with (Api.Object.Dataset.owner user)
        |> with (Api.Object.Dataset.slug)


user : SelectionSet Username Api.Object.User
user =
    Api.Object.User.selection Username
        |> with (Api.Object.User.username)


query : Mutation.CreateDatasetRequiredArguments -> SelectionSet Response RootMutation
query datasetPayload =
    Mutation.selection Response
        |> with (Mutation.createDataset datasetPayload dataset |> Field.nonNullOrFail)


type alias DataType =
    { name : String
    , labelTypes : List LabelType
    }


type alias LabelType =
    { name : String
    }


labelType : SelectionSet LabelType Api.Object.LabelType
labelType =
    Api.Object.LabelType.selection LabelType
        |> with Api.Object.LabelType.name


dataType : SelectionSet DataType Api.Object.DataType
dataType =
    Api.Object.DataType.selection DataType
        |> with Api.Object.DataType.name
        |> with (Api.Object.DataType.labelTypes labelType)


typeQuery : SelectionSet TypeResponse RootQuery
typeQuery =
    Query.selection TypeResponse
        |> with (Query.dataTypes dataType)



-- MODEL --


type alias Model =
    { errors : List Error
    , name : String
    , description : String
    , dataTypes : WebData TypeResponse
    , dataType : Maybe String
    , labelType : Maybe String
    , labelDefinition : List String
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadTypes =
            typeQuery
                |> makeQuery (Just session)
                |> Graphqelm.Http.toTask
                |> RemoteData.fromTask

        handleLoadError _ =
            pageLoadError Page.Other "The datta are currently unavailable."
    in
        loadTypes
            |> Task.mapError handleLoadError
            |> Task.map
                (\types ->
                    { errors = []
                    , name = ""
                    , description = ""
                    , dataTypes = types
                    , dataType = Nothing
                    , labelType = Nothing
                    , labelDefinition = [ "test" ]
                    }
                )



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "px-3 py-3 pt-md-5 pb-md-4" ]
                [ h1 [ class "display-4" ]
                    [ text "Create a Dataset" ]
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
    let
        dataTypes =
            case model.dataTypes of
                RemoteData.Success response ->
                    response.dataTypes
                        |> List.map .name
                        |> List.map stringToOption

                _ ->
                    []

        labelTypes =
            case model.dataType of
                Nothing ->
                    []

                Just selectedDataType ->
                    case model.dataTypes of
                        RemoteData.Success response ->
                            labelTypesFromDataType response.dataTypes selectedDataType
                                |> List.map stringToOption

                        _ ->
                            []
    in
        Html.Styled.form [ onSubmit Save ]
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
                , Form.select [ onInput SetDataType ] ([ option [] [ text "Select Data Type" ] ] ++ dataTypes)
                , Form.select [ onInput SetLabelType ] ([ option [] [ text "Select Label Type" ] ] ++ labelTypes)
                , labelDefinitionInput model.labelType
                , button [ class "btn btn-lg pull-xs-right btn-primary" ]
                    [ text "Create Dataset" ]
                ]
            ]



-- UPDATE --


type Msg
    = Save
    | SetName String
    | SetDescription String
    | SetDataType String
    | SetLabelType String
    | CreateCompleted (WebData Response)


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
                        , query inputObject
                            |> makeMutation (Just session)
                            |> Graphqelm.Http.send (RemoteData.fromResult >> CreateCompleted)
                        )

                errors ->
                    ( { model | errors = errors }, Cmd.none )

        SetName name ->
            ( { model | name = name }, Cmd.none )

        SetDescription description ->
            ( { model | description = description }, Cmd.none )

        SetDataType dataType ->
            ( { model | dataType = dataTypeInModel model.dataTypes dataType }, Cmd.none )

        SetLabelType labelType ->
            ( { model | labelType = labelTypeInModel model.dataTypes labelType }, Cmd.none )

        CreateCompleted (RemoteData.Success response) ->
            ( model, Route.PreviewDataset response.dataset.user.username response.dataset.slug |> Route.modifyUrl )

        CreateCompleted _ ->
            ( { model | errors = model.errors ++ [ ( Form, "Server error while attempting to publish dataset" ) ] }, Cmd.none )



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


modelToMutationArguments : Model -> Mutation.CreateDatasetRequiredArguments
modelToMutationArguments model =
    { dataset =
        Api.InputObject.buildCreateDatasetInput
            { name = model.name
            , dataType = Maybe.withDefault "" model.dataType
            , labelType = Maybe.withDefault "" model.labelType
            , labelDefinition =
                Api.InputObject.buildLabelDefinitionInput
                    (\optionals -> { optionals | imageClassDefinition = Present { classes = model.labelDefinition } })
            }
            (\optionals -> { optionals | description = Present model.description })
    }


stringToOption : String -> Html Msg
stringToOption v =
    option [ value v ] [ text v ]


labelTypesFromDataType : List DataType -> String -> List String
labelTypesFromDataType dataTypes selectedType =
    let
        foundType =
            List.filter (\type_ -> type_.name == selectedType) dataTypes |> List.head
    in
        case foundType of
            Just t ->
                t.labelTypes |> List.map .name

            Nothing ->
                []


labelDefinitionInput : Maybe String -> Html Msg
labelDefinitionInput labelType =
    case labelType of
        Nothing ->
            div [] []

        Just type_ ->
            case type_ of
                "Image Classification" ->
                    Form.input
                        [ placeholder "Type the classes for the image classification"
                        ]
                        []

                "Image Object Detection" ->
                    Form.input
                        [ placeholder "Type the classes for the image detection"
                        ]
                        []

                _ ->
                    p [] [ text "Oops... Seems like we don't support this label type yet! Give us some time, we are working on it 😊" ]


dataTypeInModel : WebData TypeResponse -> String -> Maybe String
dataTypeInModel dataTypes dataType =
    case dataTypes of
        RemoteData.Success response ->
            findNameInList response.dataTypes dataType

        _ ->
            Nothing


labelTypeInModel : WebData TypeResponse -> String -> Maybe String
labelTypeInModel dataTypes labelType =
    case dataTypes of
        RemoteData.Success response ->
            let
                labelTypes =
                    List.foldl (\elem acc -> acc ++ elem.labelTypes) [] response.dataTypes
            in
                findNameInList labelTypes labelType

        _ ->
            Nothing


type alias Named a =
    { a | name : String }


findNameInList : List (Named a) -> String -> Maybe String
findNameInList namedList pattern =
    let
        foundType =
            namedList
                |> List.filter (\type_ -> type_.name == pattern)
                |> List.head
    in
        case foundType of
            Just t ->
                Just t.name

            Nothing ->
                Nothing
