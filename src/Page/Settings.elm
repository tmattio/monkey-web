module Page.Settings exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Api.Object
import Api.InputObject
import Api.Object.User as User
import Api.Query as Query
import Api.Mutation as Mutation
import Request.Helpers exposing (WebData, makeQuery, makeMutation, parseGraphQLError)
import Graphqelm.Http
import Graphqelm.OptionalArgument exposing (OptionalArgument(Present))
import Graphqelm.Operation exposing (RootMutation, RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Html.Styled exposing (Html, button, div, fieldset, h1, input, text, textarea, main_)
import Html.Styled.Attributes exposing (attribute, class, defaultValue, placeholder, type_)
import Html.Styled.Events exposing (onInput, onSubmit, onClick)
import Task exposing (Task)
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Data.Auth exposing (Session, User, storeSession)
import Page.Error exposing (PageLoadError, pageLoadError)
import Views.Page as Page
import Validate exposing (Validator, ifBlank, validate)
import Views.Form as Form
import Views.Title exposing (viewTitle)
import Ports


type alias User =
    { name : String
    , username : String
    , bio : String
    , email : String
    }


viewer : SelectionSet User Api.Object.User
viewer =
    User.selection User
        |> with User.name
        |> with User.username
        |> with (User.bio |> Field.map (Maybe.withDefault ""))
        |> with User.email


type alias Response =
    { viewer : User
    }


viewerQuery : SelectionSet Response RootQuery
viewerQuery =
    Query.selection Response
        |> with (Query.viewer viewer)


updateViewerQuery : Mutation.UpdateViewerRequiredArguments -> SelectionSet Response RootMutation
updateViewerQuery userPayload =
    Mutation.selection Response
        |> with (Mutation.updateViewer userPayload viewer |> Field.nonNullOrFail)


deleteViewerQuery : SelectionSet Response RootMutation
deleteViewerQuery =
    Mutation.selection Response
        |> with (Mutation.deleteViewer viewer |> Field.nonNullOrFail)



-- MODEL --


type alias Model =
    { errors : List Error
    , name : String
    , username : String
    , bio : String
    , email : String
    , password : Maybe String
    }


init : Session -> Task PageLoadError Model
init session =
    let
        handleLoadError _ =
            pageLoadError Page.Other "The data are currently unavailable."

        loadUser =
            viewerQuery
                |> makeQuery (Just session)
                |> Graphqelm.Http.toTask
                |> RemoteData.fromTask
                |> Task.mapError handleLoadError
    in
        loadUser
            |> Task.map decodeResponse


decodeResponse : WebData Response -> Model
decodeResponse response =
    case response of
        RemoteData.Success r ->
            let
                user =
                    r.viewer
            in
                { errors = []
                , name = user.name
                , username = user.username
                , bio = user.bio
                , email = user.email
                , password = Nothing
                }

        _ ->
            { errors = []
            , name = ""
            , username = ""
            , bio = ""
            , email = ""
            , password = Nothing
            }



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    main_ [ attribute "role" "main" ]
        [ viewTitle "Your Settings"
        , div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
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
                , placeholder "Username"
                , defaultValue model.username
                , onInput SetUsername
                ]
                []
            , Form.textarea
                [ class "form-control-lg"
                , placeholder "Short bio about you"
                , attribute "rows" "8"
                , defaultValue model.bio
                , onInput SetBio
                ]
                []
            , Form.input
                [ class "form-control-lg"
                , placeholder "Email"
                , defaultValue model.email
                , onInput SetEmail
                ]
                []
            , Form.password
                [ class "form-control-lg"
                , placeholder "Password"
                , defaultValue (Maybe.withDefault "" model.password)
                , onInput SetPassword
                ]
                []
            , button
                [ class "btn btn-lg btn-primary", onClick SubmitForm ]
                [ text "Update Settings" ]
            , button
                [ class "btn btn-lg btn-danger", onClick DeleteAccount ]
                [ text "Delete Account" ]
            ]
        ]



-- UPDATE --


type Msg
    = SubmitForm
    | SetEmail String
    | SetUsername String
    | SetPassword String
    | SetBio String
    | SaveCompleted (WebData Response)
    | DeleteAccount
    | DeleteAccountCompleted (WebData Response)


type ExternalMsg
    = NoOp
    | SetSession (Maybe Session)


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        SubmitForm ->
            case validate modelValidator model of
                [] ->
                    let
                        inputObject =
                            modelToMutationArguments model
                    in
                        ( ( { model | errors = [] }
                          , updateViewerQuery inputObject
                                |> makeMutation (Just session)
                                |> Graphqelm.Http.send (RemoteData.fromResult >> SaveCompleted)
                          )
                        , NoOp
                        )

                errors ->
                    ( ( { model | errors = errors }, Cmd.none ), NoOp )

        SetEmail email ->
            ( ( { model | email = email }, Cmd.none ), NoOp )

        SetUsername username ->
            ( ( { model | username = username }, Cmd.none ), NoOp )

        SetPassword passwordStr ->
            let
                password =
                    if String.isEmpty passwordStr then
                        Nothing
                    else
                        Just passwordStr
            in
                ( ( { model | password = password }, Cmd.none ), NoOp )

        SetBio bio ->
            ( ( { model | bio = bio }, Cmd.none ), NoOp )

        SaveCompleted (RemoteData.Failure error) ->
            let
                errors =
                    [ ( Form, parseGraphQLError error ) ]
            in
                ( ( { model | errors = errors }, Cmd.none ), NoOp )

        SaveCompleted (RemoteData.Success r) ->
            let
                newSession =
                    { session
                        | user =
                            { username = r.viewer.username
                            , name = r.viewer.name
                            , email = r.viewer.email
                            }
                    }
            in
                ( ( model
                  , Cmd.batch
                        [ storeSession newSession
                        , Route.modifyUrl Route.Home
                        ]
                  )
                , SetSession (Just newSession)
                )

        SaveCompleted _ ->
            ( ( model, Cmd.none ), NoOp )

        DeleteAccount ->
            ( ( { model | errors = [] }
              , deleteViewerQuery
                    |> makeMutation (Just session)
                    |> Graphqelm.Http.send (RemoteData.fromResult >> DeleteAccountCompleted)
              )
            , NoOp
            )

        DeleteAccountCompleted (RemoteData.Success r) ->
            ( ( model
              , Cmd.batch
                    [ Ports.storeSession Nothing
                    , Route.modifyUrl Route.Home
                    ]
              )
            , SetSession Nothing
            )

        DeleteAccountCompleted _ ->
            ( ( model, Cmd.none ), NoOp )



-- VALIDATION --


type Field
    = Form
    | Username
    | Email
    | Password
    | ImageUrl
    | Bio


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .username ( Username, "username can't be blank." )
        , ifBlank .email ( Email, "email can't be blank." )
        ]



-- INTERNALS --


modelToMutationArguments : Model -> Mutation.UpdateViewerRequiredArguments
modelToMutationArguments model =
    let
        optionals =
            case model.password of
                Just p ->
                    (\optionals -> { optionals | password = Present p })

                Nothing ->
                    identity
    in
        { user =
            Api.InputObject.buildUpdateUserInput
                (\optionals ->
                    { optionals
                        | name = Present model.name
                        , username = Present model.username
                        , bio = Present model.bio
                        , email = Present model.email
                    }
                )
        }
