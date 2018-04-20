-- Do not manually edit this file, it was auto-generated by Graphqelm
-- https://github.com/dillonkearns/graphqelm


module Api.Object.DatasetConnection exposing (..)

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
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.DatasetConnection
selection constructor =
    Object.selection constructor


edges : SelectionSet decodesTo Api.Object.DatasetEdge -> Field (Maybe (List (Maybe decodesTo))) Api.Object.DatasetConnection
edges object =
    Object.selectionField "edges" [] object (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


pageInfo : SelectionSet decodesTo Api.Object.PageInfo -> Field decodesTo Api.Object.DatasetConnection
pageInfo object =
    Object.selectionField "pageInfo" [] object identity
