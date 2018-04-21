module Page.Login exposing (ExternalMsg(..), view, Msg, Model, update, initialModel)

import Graphqelm.Http
import Graphqelm.Http.GraphqlError
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (..)
import Validate exposing (Validator, ifBlank, validate)
import RemoteData exposing (RemoteData)
import Request.Helpers exposing (WebData, makeMutation, parseGraphQLError)
import Request.Auth exposing (SessionResponse, login)
import Data.Auth exposing (Session, User, storeSession)
import Views.Form as Form
import Route exposing (Route)
import Stylesheets exposing (formSignin)


-- MODEL --


type alias Model =
    { errors : List Error
    , username : String
    , password : String
    , msg : String
    }


initialModel : Model
initialModel =
    { errors = []
    , username = ""
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
            [ text "Please sign in" ]
        , p [ class "text-xs-center" ]
            [ a [ Route.href Route.Register ]
                [ text "Need an account?" ]
            ]
        , viewForm
        ]


viewForm : Html Msg
viewForm =
    Html.Styled.form [ onSubmit SubmitForm ]
        [ Form.input
            [ class "form-control-lg"
            , placeholder "Username"
            , onInput SetUsername
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , onInput SetPassword
            ]
            []
        , button [ class "btn btn-lg btn-primary btn-block", type_ "submit" ]
            [ text "Sign in" ]
        ]



-- UPDATE --


type Msg
    = SubmitForm
    | SetUsername String
    | SetPassword String
    | LoginCompleted (WebData SessionResponse)


type ExternalMsg
    = NoOp
    | SetSession Session


update : Maybe Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        SubmitForm ->
            case validate modelValidator model of
                [] ->
                    ( ( { model | errors = [] }
                      , { username = model.username, password = model.password }
                            |> login
                            |> makeMutation session
                            |> Graphqelm.Http.send (RemoteData.fromResult >> LoginCompleted)
                      )
                    , NoOp
                    )

                errors ->
                    ( ( { model | errors = errors }
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

        SetPassword password ->
            ( ( { model | password = password }
              , Cmd.none
              )
            , NoOp
            )

        LoginCompleted (RemoteData.Failure error) ->
            ( ( { model | errors = [ ( Form, parseGraphQLError error ) ] }
              , Cmd.none
              )
            , NoOp
            )

        LoginCompleted (RemoteData.Success response) ->
            ( ( model, Cmd.batch [ storeSession response.session, Route.modifyUrl Route.Home ] ), SetSession response.session )

        LoginCompleted _ ->
            ( ( model, Cmd.none ), NoOp )



-- VALIDATION --


type Field
    = Form
    | Username
    | Password


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .username ( Username, "username can't be blank." )
        , ifBlank .password ( Password, "password can't be blank." )
        ]
