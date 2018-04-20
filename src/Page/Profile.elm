module Page.Profile exposing (view, update, Model, Msg, init)

import Api.Object
import Api.Object.User as User
import Api.Query as Query
import Graphqelm.Http
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Task exposing (Task)
import RemoteData exposing (RemoteData)
import Request.Helpers exposing (WebData, makeQuery)
import Data exposing (Session)
import Page.Error exposing (PageLoadError, pageLoadError)
import Views.Page as Page
import Views.Error exposing (viewWithError)
import Views.Title exposing (viewTitle)


-- DATA --


type alias Response =
    { user : User
    }


type alias User =
    { name : String
    , bio : Maybe String
    , avatarUrl : Maybe String
    }


user : SelectionSet User Api.Object.User
user =
    User.selection User
        |> with User.name
        |> with User.bio
        |> with User.avatarUrl


query : Query.UserRequiredArguments -> SelectionSet Response RootQuery
query username =
    Query.selection Response
        |> with (Query.user username user |> Field.nonNullOrFail)



---- MODEL ----


type alias Model =
    { user : WebData Response
    }


init : Maybe Session -> String -> Task PageLoadError Model
init session username =
    let
        loadUser =
            query { username = username }
                |> makeQuery session
                |> Graphqelm.Http.toTask
                |> RemoteData.fromTask

        handleLoadError _ =
            pageLoadError Page.Other "The datasests are currently unavailable."
    in
        Task.map Model loadUser
            |> Task.mapError handleLoadError



--  VIEW --


view : Maybe Session -> Model -> Html Msg
view session model =
    let
        successView r =
            viewUser r.user

        profileView =
            viewWithError model.user successView
    in
        main_ [ attribute "role" "main" ]
            [ viewTitle "Profile"
            , div [ class "container" ]
                [ profileView
                ]
            ]


viewUser : User -> Html Msg
viewUser user =
    let
        userImage =
            Maybe.withDefault "https://picsum.photos/286/180" user.avatarUrl

        userBio =
            Maybe.withDefault "" user.bio
    in
        div [ class "card mt-4" ]
            [ img [ class "card-img-top img-fluid", src userImage, alt "" ]
                []
            , div [ class "card-body" ]
                [ h3 [ class "card-title" ]
                    [ text user.name ]
                , p [ class "card-text" ]
                    [ text userBio ]
                ]
            ]



-- UPDATE --


type Msg
    = Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )
