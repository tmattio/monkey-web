module Page.Explore exposing (view, update, Model, Msg, init)

import Api.Object
import Api.Object.Dataset as Dataset
import Api.Query as Query
import Request.Helpers exposing (WebData, makeQuery)
import Graphqelm.Http
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Task exposing (Task)
import RemoteData exposing (RemoteData)
import Data.Auth exposing (Session)
import Page.Error exposing (PageLoadError, pageLoadError)
import Views.Page as Page
import Views.Error exposing (viewWithError)
import Views.Title exposing (viewTitle)


-- DATA --


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
    Dataset.selection Dataset
        |> with Dataset.name
        |> with Dataset.description
        |> with Dataset.slug
        |> with Dataset.thumbnailUrl


query : Query.DatasetRequiredArguments -> SelectionSet DatasetResponse RootQuery
query datasetPayload =
    Query.selection DatasetResponse
        |> with (Query.dataset datasetPayload dataset |> Field.nonNullOrFail)



---- MODEL ----


type alias Model =
    { dataset : WebData DatasetResponse
    }


init : Maybe Session -> Task PageLoadError Model
init session =
    let
        loadDataset =
            { name = "dogs-and-cats", owner = "tmattio" }
                |> query
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
            div [ class "row" ]
                [ datasetCard r.dataset
                , datasetCard r.dataset
                , datasetCard r.dataset
                , datasetCard r.dataset
                , datasetCard r.dataset
                , datasetCard r.dataset
                ]

        datasetView =
            viewWithError model.dataset successView
    in
        main_ [ attribute "role" "main" ]
            [ viewTitle "Explore Dataset"
            , div [ class "container" ]
                [ datasetView
                ]
            ]


datasetCard : Dataset -> Html Msg
datasetCard dataset =
    let
        datasetImage =
            case dataset.thumbnailUrl of
                Just url ->
                    url

                Nothing ->
                    "https://picsum.photos/286/180"
    in
        div [ class "col-md-4" ]
            [ div [ class "card mb-4 box-shadow" ]
                [ img [ class "card-img-top", src datasetImage, alt "Card image cap" ]
                    []
                , div [ class "card-body" ]
                    [ p [ class "card-text" ]
                        [ text (Maybe.withDefault "" dataset.description) ]
                    , div [ class "d-flex justify-content-between align-items-center" ]
                        [ div [ class "btn-group" ]
                            [ button [ type_ "button", class "btn btn-sm btn-outline-secondary" ]
                                [ text "View" ]
                            , button [ type_ "button", class "btn btn-sm btn-outline-secondary" ]
                                [ text "Star" ]
                            ]
                        , small [ class "text-muted" ]
                            [ text "127K samples" ]
                        ]
                    ]
                ]
            ]


viewDataset : Dataset -> Html Msg
viewDataset dataset =
    let
        datasetImage =
            case dataset.thumbnailUrl of
                Just url ->
                    url

                Nothing ->
                    "https://picsum.photos/286/180"
    in
        div [ class "card", style [ ( "width", "18rem" ) ] ]
            [ img [ class "card-img-top", src datasetImage, alt "Card image cap" ]
                []
            , div [ class "card-body" ]
                [ h5 [ class "card-title" ]
                    [ text dataset.name ]
                , p [ class "card-text" ]
                    [ text (Maybe.withDefault "" dataset.description) ]
                , a [ href "#", class "btn btn-primary" ]
                    [ text "Go somewhere" ]
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
