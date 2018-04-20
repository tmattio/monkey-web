-- Do not manually edit this file, it was auto-generated by Graphqelm
-- https://github.com/dillonkearns/graphqelm


module Api.Object.Text exposing (..)

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
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.Text
selection constructor =
    Object.selection constructor


body : Field String Api.Object.Text
body =
    Object.fieldDecoder "body" [] Decode.string


id : Field Api.Scalar.Id Api.Object.Text
id =
    Object.fieldDecoder "id" [] (Decode.oneOf [ Decode.string, Decode.float |> Decode.map toString, Decode.int |> Decode.map toString, Decode.bool |> Decode.map toString ] |> Decode.map Api.Scalar.Id)


labels : SelectionSet decodesTo Api.Union.Label -> Field (List decodesTo) Api.Object.Text
labels object =
    Object.selectionField "labels" [] object (identity >> Decode.list)


length : Field Int Api.Object.Text
length =
    Object.fieldDecoder "length" [] Decode.int
