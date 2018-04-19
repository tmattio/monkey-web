-- Do not manually edit this file, it was auto-generated by Graphqelm
-- https://github.com/dillonkearns/graphqelm


module Api.Object.ImageBoundingBox exposing (..)

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
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.ImageBoundingBox
selection constructor =
    Object.selection constructor


class : Field (Maybe String) Api.Object.ImageBoundingBox
class =
    Object.fieldDecoder "class" [] (Decode.string |> Decode.nullable)


datapoint : SelectionSet decodesTo Api.Object.Image -> Field decodesTo Api.Object.ImageBoundingBox
datapoint object =
    Object.selectionField "datapoint" [] object identity


id : Field Api.Scalar.Id Api.Object.ImageBoundingBox
id =
    Object.fieldDecoder "id" [] (Decode.oneOf [ Decode.string, Decode.float |> Decode.map toString, Decode.int |> Decode.map toString, Decode.bool |> Decode.map toString ] |> Decode.map Api.Scalar.Id)


xMax : Field (Maybe Float) Api.Object.ImageBoundingBox
xMax =
    Object.fieldDecoder "xMax" [] (Decode.float |> Decode.nullable)


xMin : Field (Maybe Float) Api.Object.ImageBoundingBox
xMin =
    Object.fieldDecoder "xMin" [] (Decode.float |> Decode.nullable)


yMax : Field (Maybe Float) Api.Object.ImageBoundingBox
yMax =
    Object.fieldDecoder "yMax" [] (Decode.float |> Decode.nullable)


yMin : Field (Maybe Float) Api.Object.ImageBoundingBox
yMin =
    Object.fieldDecoder "yMin" [] (Decode.float |> Decode.nullable)
