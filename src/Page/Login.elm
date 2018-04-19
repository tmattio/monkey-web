module Page.Login exposing (ExternalMsg(..), view, Msg, Model, update, initialModel)

import Graphqelm.Http
import Graphqelm.Http.GraphqlError
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (..)
import Validate exposing (Validator, ifBlank, validate)
import RemoteData exposing (RemoteData)
import Request.Helpers exposing (WebData, makeMutation)
import Request.Auth exposing (SessionResponse, login)
import Data exposing (Session, User)
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
            let
                errorMessages =
                    case error of
                        Graphqelm.Http.GraphqlError possiblyParsedData errors ->
                            "GraphQL errors: \n"
                                ++ toString errors
                                ++ "\n\n"
                                ++ (case possiblyParsedData of
                                        Graphqelm.Http.GraphqlError.UnparsedData unparsed ->
                                            "Unable to parse data, got: " ++ toString unparsed

                                        Graphqelm.Http.GraphqlError.ParsedData parsedData ->
                                            "Parsed error data, so I can extract the name from the structured data..."
                                   )

                        Graphqelm.Http.HttpError httpError ->
                            "Http error " ++ toString httpError
            in
                ( ( { model | errors = [ ( Form, errorMessages ) ] }
                  , Cmd.none
                  )
                , NoOp
                )

        LoginCompleted (RemoteData.Success response) ->
            ( ( model, Cmd.batch [ Data.storeSession response.session, Route.modifyUrl Route.Home ] ), SetSession response.session )

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
