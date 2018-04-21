module Data.Auth
    exposing
        ( Session
        , User
        , sessionDecoder
        , userDecoder
        , userEncoder
        , storeSession
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Json.Decode.Pipeline exposing (decode, required)
import Ports


type alias Session =
    { user : User
    , token : String
    }


type alias User =
    { email : String
    , username : String
    , name : String
    }


sessionDecoder : Decoder Session
sessionDecoder =
    decode Session
        |> required "user" userDecoder
        |> required "token" Decode.string


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "email" Decode.string
        |> required "username" Decode.string
        |> required "name" Decode.string


sessionEncoder : Session -> Value
sessionEncoder session =
    Encode.object
        [ ( "user", userEncoder session.user )
        , ( "token", Encode.string session.token )
        ]


userEncoder : User -> Value
userEncoder user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "username", Encode.string user.username )
        , ( "name", Encode.string user.name )
        ]


storeSession : Session -> Cmd msg
storeSession session =
    sessionEncoder session
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession
