module Request.Helpers exposing (apiUrl, WebData, makeQuery, makeMutation)

import Graphqelm.Http
import Graphqelm.Operation exposing (RootQuery, RootMutation)
import Graphqelm.SelectionSet exposing (SelectionSet)
import RemoteData exposing (RemoteData)
import Data exposing (Session)


apiUrl : String
apiUrl =
    "http://localhost:4000/api"


makeQuery : Maybe Session -> SelectionSet a RootQuery -> Graphqelm.Http.Request a
makeQuery session query =
    query
        |> Graphqelm.Http.queryRequest apiUrl
        |> withAuthorization session


makeMutation : Maybe Session -> SelectionSet a RootMutation -> Graphqelm.Http.Request a
makeMutation session query =
    query
        |> Graphqelm.Http.mutationRequest apiUrl
        |> withAuthorization session


withAuthorization : Maybe Session -> Graphqelm.Http.Request a -> Graphqelm.Http.Request a
withAuthorization session request =
    case session of
        Just s ->
            request
                |> Graphqelm.Http.withHeader "authorization" ("Bearer " ++ s.token)

        Nothing ->
            request


type alias WebData a =
    RemoteData (Graphqelm.Http.Error a) a
