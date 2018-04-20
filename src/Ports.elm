port module Ports exposing (onSessionChange, storeSession, FilePortData, fileSelected, fileContentRead)

import Json.Encode exposing (Value)


port storeSession : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg


type alias FilePortData =
    { content : String
    , filename : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FilePortData -> msg) -> Sub msg
