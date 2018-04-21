module Page.Dashboard exposing (view, update, Model, Msg, init)

import Api.Object
import Api.Object.Dataset as Dataset
import Api.Object.User as User
import Api.Query as Query
import Request.Helpers exposing (WebData, makeQuery)
import Graphqelm.Http
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Task exposing (Task)
import RemoteData exposing (RemoteData)
import Data.Auth exposing (Session)
import Page.Error exposing (PageLoadError, pageLoadError)
import Views.Page as Page
import Views.Error exposing (viewWithError)
import Views.Title exposing (viewTitle)
import Route exposing (Route)


-- DATA --


type alias Response =
    { viewer : ViewerResponse
    }


type alias ViewerResponse =
    { datasets : List Dataset
    }


type alias Dataset =
    { name : String
    , description : Maybe String
    , slug : String
    , thumbnailUrl : Maybe String
    , owner : String
    }


dataset : SelectionSet Dataset Api.Object.Dataset
dataset =
    Dataset.selection Dataset
        |> with Dataset.name
        |> with Dataset.description
        |> with Dataset.slug
        |> with Dataset.thumbnailUrl
        |> with (Dataset.owner owner)


owner : SelectionSet String Api.Object.User
owner =
    User.selection identity
        |> with (User.username)


viewer : SelectionSet ViewerResponse Api.Object.User
viewer =
    User.selection ViewerResponse
        |> with (User.datasets dataset)


query : SelectionSet Response RootQuery
query =
    Query.selection Response
        |> with (Query.viewer viewer)



---- MODEL ----


type alias Model =
    { dataset : WebData Response
    }


init : Maybe Session -> Task PageLoadError Model
init session =
    let
        loadDataset =
            query
                |> makeQuery session
                |> Graphqelm.Http.toTask
                |> RemoteData.fromTask

        handleLoadError _ =
            pageLoadError Page.Other "The datasests are currently unavailable."
    in
        Task.map Model loadDataset
            |> Task.mapError handleLoadError



--  VIEW --


view : Maybe Session -> Model -> Html Msg
view session model =
    let
        successView r =
            div [ class "row" ] (List.map viewDataset r.viewer.datasets)

        datasetView =
            viewWithError model.dataset successView
    in
        main_ [ attribute "role" "main" ]
            [ viewTitle "My Datasets"
            , div [ class "container" ]
                [ datasetView
                ]
            ]


viewDataset : Dataset -> Html Msg
viewDataset dataset =
    let
        datasetImage =
            Maybe.withDefault "https://picsum.photos/286/180" dataset.thumbnailUrl
    in
        div [ class "col-md-4" ]
            [ div [ class "card mb-4 box-shadow" ]
                [ img [ class "card-img-top", src datasetImage, alt "Card image cap" ]
                    []
                , div [ class "card-body" ]
                    [ b [] [ text dataset.name ]
                    , p [ class "card-text" ]
                        [ text (Maybe.withDefault "" dataset.description) ]
                    , div [ class "d-flex justify-content-between align-items-center" ]
                        [ div [ class "btn-group" ]
                            [ button [ type_ "button", class "btn btn-sm btn-outline-secondary" ]
                                [ a [ Route.href (Route.PreviewDataset dataset.owner dataset.slug) ] [ text "View" ] ]
                            , button [ type_ "button", class "btn btn-sm btn-outline-secondary" ]
                                [ text "Star" ]
                            ]
                        , small [ class "text-muted" ]
                            [ text "127K samples" ]
                        ]
                    ]
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
