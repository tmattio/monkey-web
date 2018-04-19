-- Do not manually edit this file, it was auto-generated by Graphqelm
-- https://github.com/dillonkearns/graphqelm


module Api.InputObject exposing (..)

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


buildCreateDatasetInput : CreateDatasetInputRequiredFields -> (CreateDatasetInputOptionalFields -> CreateDatasetInputOptionalFields) -> CreateDatasetInput
buildCreateDatasetInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { description = Absent }
    in
    { dataType = required.dataType, description = optionals.description, labelDefinition = required.labelDefinition, labelType = required.labelType, name = required.name }


type alias CreateDatasetInputRequiredFields =
    { dataType : String, labelDefinition : LabelDefinitionInput, labelType : String, name : String }


type alias CreateDatasetInputOptionalFields =
    { description : OptionalArgument String }


{-| Type for the CreateDatasetInput input object.
-}
type alias CreateDatasetInput =
    { dataType : String, description : OptionalArgument String, labelDefinition : LabelDefinitionInput, labelType : String, name : String }


{-| Encode a CreateDatasetInput into a value that can be used as an argument.
-}
encodeCreateDatasetInput : CreateDatasetInput -> Value
encodeCreateDatasetInput input =
    Encode.maybeObject
        [ ( "dataType", Encode.string input.dataType |> Just ), ( "description", Encode.string |> Encode.optional input.description ), ( "labelDefinition", encodeLabelDefinitionInput input.labelDefinition |> Just ), ( "labelType", Encode.string input.labelType |> Just ), ( "name", Encode.string input.name |> Just ) ]


buildImageBoundingBoxDefinitionInput : ImageBoundingBoxDefinitionInputRequiredFields -> ImageBoundingBoxDefinitionInput
buildImageBoundingBoxDefinitionInput required =
    { classes = required.classes }


type alias ImageBoundingBoxDefinitionInputRequiredFields =
    { classes : List String }


{-| Type for the ImageBoundingBoxDefinitionInput input object.
-}
type alias ImageBoundingBoxDefinitionInput =
    { classes : List String }


{-| Encode a ImageBoundingBoxDefinitionInput into a value that can be used as an argument.
-}
encodeImageBoundingBoxDefinitionInput : ImageBoundingBoxDefinitionInput -> Value
encodeImageBoundingBoxDefinitionInput input =
    Encode.maybeObject
        [ ( "classes", (Encode.string |> Encode.list) input.classes |> Just ) ]


buildImageClassDefinitionInput : ImageClassDefinitionInputRequiredFields -> ImageClassDefinitionInput
buildImageClassDefinitionInput required =
    { classes = required.classes }


type alias ImageClassDefinitionInputRequiredFields =
    { classes : List String }


{-| Type for the ImageClassDefinitionInput input object.
-}
type alias ImageClassDefinitionInput =
    { classes : List String }


{-| Encode a ImageClassDefinitionInput into a value that can be used as an argument.
-}
encodeImageClassDefinitionInput : ImageClassDefinitionInput -> Value
encodeImageClassDefinitionInput input =
    Encode.maybeObject
        [ ( "classes", (Encode.string |> Encode.list) input.classes |> Just ) ]


buildLabelDefinitionInput : (LabelDefinitionInputOptionalFields -> LabelDefinitionInputOptionalFields) -> LabelDefinitionInput
buildLabelDefinitionInput fillOptionals =
    let
        optionals =
            fillOptionals
                { imageBoundingBoxDefinition = Absent, imageClassDefinition = Absent }
    in
    { imageBoundingBoxDefinition = optionals.imageBoundingBoxDefinition, imageClassDefinition = optionals.imageClassDefinition }


type alias LabelDefinitionInputOptionalFields =
    { imageBoundingBoxDefinition : OptionalArgument ImageBoundingBoxDefinitionInput, imageClassDefinition : OptionalArgument ImageClassDefinitionInput }


{-| Type for the LabelDefinitionInput input object.
-}
type alias LabelDefinitionInput =
    { imageBoundingBoxDefinition : OptionalArgument ImageBoundingBoxDefinitionInput, imageClassDefinition : OptionalArgument ImageClassDefinitionInput }


{-| Encode a LabelDefinitionInput into a value that can be used as an argument.
-}
encodeLabelDefinitionInput : LabelDefinitionInput -> Value
encodeLabelDefinitionInput input =
    Encode.maybeObject
        [ ( "imageBoundingBoxDefinition", encodeImageBoundingBoxDefinitionInput |> Encode.optional input.imageBoundingBoxDefinition ), ( "imageClassDefinition", encodeImageClassDefinitionInput |> Encode.optional input.imageClassDefinition ) ]


buildRegisterUserInput : RegisterUserInputRequiredFields -> RegisterUserInput
buildRegisterUserInput required =
    { email = required.email, name = required.name, password = required.password, username = required.username }


type alias RegisterUserInputRequiredFields =
    { email : String, name : String, password : String, username : String }


{-| Type for the RegisterUserInput input object.
-}
type alias RegisterUserInput =
    { email : String, name : String, password : String, username : String }


{-| Encode a RegisterUserInput into a value that can be used as an argument.
-}
encodeRegisterUserInput : RegisterUserInput -> Value
encodeRegisterUserInput input =
    Encode.maybeObject
        [ ( "email", Encode.string input.email |> Just ), ( "name", Encode.string input.name |> Just ), ( "password", Encode.string input.password |> Just ), ( "username", Encode.string input.username |> Just ) ]


buildUpdateDatasetInput : (UpdateDatasetInputOptionalFields -> UpdateDatasetInputOptionalFields) -> UpdateDatasetInput
buildUpdateDatasetInput fillOptionals =
    let
        optionals =
            fillOptionals
                { description = Absent, isArchived = Absent, isPrivate = Absent, license = Absent, name = Absent, tagList = Absent, thumbnailUrl = Absent }
    in
    { description = optionals.description, isArchived = optionals.isArchived, isPrivate = optionals.isPrivate, license = optionals.license, name = optionals.name, tagList = optionals.tagList, thumbnailUrl = optionals.thumbnailUrl }


type alias UpdateDatasetInputOptionalFields =
    { description : OptionalArgument String, isArchived : OptionalArgument Bool, isPrivate : OptionalArgument Bool, license : OptionalArgument String, name : OptionalArgument String, tagList : OptionalArgument (List (Maybe String)), thumbnailUrl : OptionalArgument String }


{-| Type for the UpdateDatasetInput input object.
-}
type alias UpdateDatasetInput =
    { description : OptionalArgument String, isArchived : OptionalArgument Bool, isPrivate : OptionalArgument Bool, license : OptionalArgument String, name : OptionalArgument String, tagList : OptionalArgument (List (Maybe String)), thumbnailUrl : OptionalArgument String }


{-| Encode a UpdateDatasetInput into a value that can be used as an argument.
-}
encodeUpdateDatasetInput : UpdateDatasetInput -> Value
encodeUpdateDatasetInput input =
    Encode.maybeObject
        [ ( "description", Encode.string |> Encode.optional input.description ), ( "isArchived", Encode.bool |> Encode.optional input.isArchived ), ( "isPrivate", Encode.bool |> Encode.optional input.isPrivate ), ( "license", Encode.string |> Encode.optional input.license ), ( "name", Encode.string |> Encode.optional input.name ), ( "tagList", (Encode.string |> Encode.maybe |> Encode.list) |> Encode.optional input.tagList ), ( "thumbnailUrl", Encode.string |> Encode.optional input.thumbnailUrl ) ]


buildUpdateUserInput : (UpdateUserInputOptionalFields -> UpdateUserInputOptionalFields) -> UpdateUserInput
buildUpdateUserInput fillOptionals =
    let
        optionals =
            fillOptionals
                { avatarUrl = Absent, bio = Absent, company = Absent, email = Absent, name = Absent, password = Absent, username = Absent, websiteUrl = Absent }
    in
    { avatarUrl = optionals.avatarUrl, bio = optionals.bio, company = optionals.company, email = optionals.email, name = optionals.name, password = optionals.password, username = optionals.username, websiteUrl = optionals.websiteUrl }


type alias UpdateUserInputOptionalFields =
    { avatarUrl : OptionalArgument String, bio : OptionalArgument String, company : OptionalArgument String, email : OptionalArgument String, name : OptionalArgument String, password : OptionalArgument String, username : OptionalArgument String, websiteUrl : OptionalArgument String }


{-| Type for the UpdateUserInput input object.
-}
type alias UpdateUserInput =
    { avatarUrl : OptionalArgument String, bio : OptionalArgument String, company : OptionalArgument String, email : OptionalArgument String, name : OptionalArgument String, password : OptionalArgument String, username : OptionalArgument String, websiteUrl : OptionalArgument String }


{-| Encode a UpdateUserInput into a value that can be used as an argument.
-}
encodeUpdateUserInput : UpdateUserInput -> Value
encodeUpdateUserInput input =
    Encode.maybeObject
        [ ( "avatarUrl", Encode.string |> Encode.optional input.avatarUrl ), ( "bio", Encode.string |> Encode.optional input.bio ), ( "company", Encode.string |> Encode.optional input.company ), ( "email", Encode.string |> Encode.optional input.email ), ( "name", Encode.string |> Encode.optional input.name ), ( "password", Encode.string |> Encode.optional input.password ), ( "username", Encode.string |> Encode.optional input.username ), ( "websiteUrl", Encode.string |> Encode.optional input.websiteUrl ) ]
