-- Do not manually edit this file, it was auto-generated by Graphqelm
-- https://github.com/dillonkearns/graphqelm


module Api.Object.PageInfo exposing (..)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.Union
import Graphqelm.Field as Field exposing (Field)
import Graphqelm.Internal.Builder.Argument as Argument exposing (Argument)
import Graphqelm.Internal.Builder.Object as Object
import Graphqelm.Internal.Encode as Encode exposing (Value)
import Graphqelm.OptionalArgument exposing (OptionalArgument(Absent))
import Graphqelm.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| Select fields to build up a SelectionSet for this object.
-}
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.PageInfo
selection constructor =
    Object.selection constructor


{-| When paginating forwards, the cursor to continue.
-}
endCursor : Field (Maybe String) Api.Object.PageInfo
endCursor =
    Object.fieldDecoder "endCursor" [] (Decode.string |> Decode.nullable)


{-| When paginating forwards, are there more items?
-}
hasNextPage : Field Bool Api.Object.PageInfo
hasNextPage =
    Object.fieldDecoder "hasNextPage" [] Decode.bool


{-| When paginating backwards, are there more items?
-}
hasPreviousPage : Field Bool Api.Object.PageInfo
hasPreviousPage =
    Object.fieldDecoder "hasPreviousPage" [] Decode.bool


{-| When paginating backwards, the cursor to continue.
-}
startCursor : Field (Maybe String) Api.Object.PageInfo
startCursor =
    Object.fieldDecoder "startCursor" [] (Decode.string |> Decode.nullable)
