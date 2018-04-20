module Views.Upload exposing (init, view, update, subscriptions, Model, Msg)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (src, title, class, id, type_, multiple)
import Html.Styled.Events exposing (on)
import Json.Decode as JD
import Ports exposing (FilePortData, fileSelected, fileContentRead)


type alias Model =
    { id : String
    , errors : List String
    , files : List File
    }


type alias File =
    { content : String
    , filename : String
    }


init : Model
init =
    { errors = []
    , id = "FileInputId"
    , files = []
    }



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "fileWrapper" ]
        [ input
            [ type_ "file"
            , id model.id
            , multiple True
            , on "change"
                (JD.succeed FileSelected)
            ]
            []
        ]



-- UPDATE --


type Msg
    = FileSelected
    | FileRead FilePortData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileSelected ->
            ( model
            , fileSelected model.id
            )

        FileRead data ->
            let
                newFile =
                    filePortDataToFile data
            in
                ( { model | files = newFile :: model.files }
                , Cmd.none
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileRead



-- INTENAL --


filePortDataToFile : FilePortData -> File
filePortDataToFile filePort =
    { content = filePort.content
    , filename = filePort.filename
    }
