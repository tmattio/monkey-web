module Page.Register exposing (ExternalMsg(..), view, Msg, Model, update, initialModel)

import Graphqelm.Http
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (..)
import Validate exposing (Validator, ifBlank, validate)
import RemoteData exposing (RemoteData)
import Request.Helpers exposing (WebData, makeMutation, parseGraphQLError)
import Request.Auth exposing (SessionResponse, register)
import Views.Form as Form
import Data.Auth exposing (Session, storeSession)
import Route exposing (Route)
import Stylesheets exposing (formSignin)


-- MODEL --


type alias Model =
    { errors : List Error
    , name : String
    , username : String
    , email : String
    , password : String
    , msg : String
    }


initialModel : Model
initialModel =
    { errors = []
    , name = ""
    , username = ""
    , email = ""
    , password = ""
    , msg = "Didn't receive anything yet"
    }



-- VIEW --


view : Maybe Session -> Model -> Html Msg
view session model =
    Html.Styled.form [ class "form-signin", css [ formSignin ] ]
        [ img [ class "mb-4", src "https://getbootstrap.com/assets/brand/bootstrap-solid.svg", alt "", width 72, height 72 ]
            []
        , h1 [ class "h3 mb-3 font-weight-normal" ]
            [ text "Sign up" ]
        , p [ class "text-xs-center" ]
            [ a [ Route.href Route.Login ]
                [ text "Already have an account?" ]
            ]
        , viewForm
        ]


viewForm : Html Msg
viewForm =
    Html.Styled.form [ onSubmit SubmitForm ]
        [ Form.input
            [ class "form-control-lg"
            , placeholder "Name"
            , onInput SetName
            ]
            []
        , Form.input
            [ class "form-control-lg"
            , placeholder "Username"
            , onInput SetUsername
            ]
            []
        , Form.input
            [ class "form-control-lg"
            , placeholder "Email"
            , onInput SetEmail
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , onInput SetPassword
            ]
            []
        , button [ class "btn btn-lg btn-primary btn-block", type_ "submit" ]
            [ text "Sign up" ]
        ]



-- UPDATE --


type Msg
    = SubmitForm
    | SetName String
    | SetUsername String
    | SetEmail String
    | SetPassword String
    | RegisterCompleted (WebData SessionResponse)


type ExternalMsg
    = NoOp
    | SetSession Session



-- TODO: Should deactivate the form while requesting the server, a user could change the field between the registration and the login


update : Maybe Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        SubmitForm ->
            case validate modelValidator model of
                [] ->
                    ( ( { model | errors = [] }
                      , { user = { email = model.email, username = model.username, name = model.name, password = model.password } }
                            |> register
                            |> makeMutation session
                            |> Graphqelm.Http.send (RemoteData.fromResult >> RegisterCompleted)
                      )
                    , NoOp
                    )

                errors ->
                    ( ( { model | errors = errors }
                      , Cmd.none
                      )
                    , NoOp
                    )

        SetName name ->
            ( ( { model | name = name }
              , Cmd.none
              )
            , NoOp
            )

        SetUsername username ->
            ( ( { model | username = username }
              , Cmd.none
              )
            , NoOp
            )

        SetEmail email ->
            ( ( { model | email = email }
              , Cmd.none
              )
            , NoOp
            )

        SetPassword password ->
            ( ( { model | password = password }
              , Cmd.none
              )
            , NoOp
            )

        RegisterCompleted (RemoteData.Failure error) ->
            ( ( { model | errors = [ ( Form, parseGraphQLError error ) ] }
              , Cmd.none
              )
            , NoOp
            )

        RegisterCompleted (RemoteData.Success response) ->
            ( ( model, Cmd.batch [ storeSession response.session, Route.modifyUrl Route.Home ] ), SetSession response.session )

        RegisterCompleted _ ->
            ( ( model, Cmd.none ), NoOp )



-- VALIDATION --


type Field
    = Form
    | Email
    | Password


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .email ( Email, "email can't be blank." )
        , ifBlank .password ( Password, "password can't be blank." )
        ]
