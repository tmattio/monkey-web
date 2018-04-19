module Request.Auth exposing (..)

import Graphqelm.Operation exposing (RootMutation)
import Graphqelm.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Graphqelm.Field as Field
import Api.Object
import Api.Object.User
import Api.Object.Session
import Api.Mutation as Mutation
import Data exposing (Session, User)


type alias SessionResponse =
    { session : Session
    }


session : SelectionSet Session Api.Object.Session
session =
    Api.Object.Session.selection Session
        |> with (Api.Object.Session.user user)
        |> with Api.Object.Session.token


user : SelectionSet User Api.Object.User
user =
    Api.Object.User.selection User
        |> with Api.Object.User.email
        |> with Api.Object.User.username
        |> with Api.Object.User.name


login : Mutation.LoginRequiredArguments -> SelectionSet SessionResponse RootMutation
login loginPayload =
    Mutation.selection SessionResponse
        |> with (Mutation.login loginPayload session |> Field.nonNullOrFail)


register : Mutation.RegisterRequiredArguments -> SelectionSet SessionResponse RootMutation
register userPayload =
    Mutation.selection SessionResponse
        |> with (Mutation.register userPayload session |> Field.nonNullOrFail)
