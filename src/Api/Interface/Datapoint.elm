-- Do not manually edit this file, it was auto-generated by Graphqelm
-- https://github.com/dillonkearns/graphqelm


module Api.Interface.Datapoint exposing (..)

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
import Graphqelm.SelectionSet exposing (FragmentSelectionSet(FragmentSelectionSet), SelectionSet(SelectionSet))
import Json.Decode as Decode


{-| Select only common fields from the interface.
-}
commonSelection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Interface.Datapoint
commonSelection constructor =
    Object.selection constructor


{-| Select both common and type-specific fields from the interface.
-}
selection : (Maybe typeSpecific -> a -> constructor) -> List (FragmentSelectionSet typeSpecific Api.Interface.Datapoint) -> SelectionSet (a -> constructor) Api.Interface.Datapoint
selection constructor typeSpecificDecoders =
    Object.interfaceSelection typeSpecificDecoders constructor


onImage : SelectionSet decodesTo Api.Object.Image -> FragmentSelectionSet decodesTo Api.Interface.Datapoint
onImage (SelectionSet fields decoder) =
    FragmentSelectionSet "Image" fields decoder


onText : SelectionSet decodesTo Api.Object.Text -> FragmentSelectionSet decodesTo Api.Interface.Datapoint
onText (SelectionSet fields decoder) =
    FragmentSelectionSet "Text" fields decoder


onVideo : SelectionSet decodesTo Api.Object.Video -> FragmentSelectionSet decodesTo Api.Interface.Datapoint
onVideo (SelectionSet fields decoder) =
    FragmentSelectionSet "Video" fields decoder


id : Field Api.Scalar.Id Api.Interface.Datapoint
id =
    Object.fieldDecoder "id" [] (Decode.oneOf [ Decode.string, Decode.float |> Decode.map toString, Decode.int |> Decode.map toString, Decode.bool |> Decode.map toString ] |> Decode.map Api.Scalar.Id)


labels : SelectionSet decodesTo Api.Union.Label -> Field (List decodesTo) Api.Interface.Datapoint
labels object =
    Object.selectionField "labels" [] object (identity >> Decode.list)
