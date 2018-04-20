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


buildDatapointInput : (DatapointInputOptionalFields -> DatapointInputOptionalFields) -> DatapointInput
buildDatapointInput fillOptionals =
    let
        optionals =
            fillOptionals
                { remoteImage = Absent, text = Absent, uploadImage = Absent, video = Absent }
    in
    { remoteImage = optionals.remoteImage, text = optionals.text, uploadImage = optionals.uploadImage, video = optionals.video }


type alias DatapointInputOptionalFields =
    { remoteImage : OptionalArgument RemoteImageInput, text : OptionalArgument TextInput, uploadImage : OptionalArgument UploadImageInput, video : OptionalArgument VideoInput }


{-| Type for the DatapointInput input object.
-}
type alias DatapointInput =
    { remoteImage : OptionalArgument RemoteImageInput, text : OptionalArgument TextInput, uploadImage : OptionalArgument UploadImageInput, video : OptionalArgument VideoInput }


{-| Encode a DatapointInput into a value that can be used as an argument.
-}
encodeDatapointInput : DatapointInput -> Value
encodeDatapointInput input =
    Encode.maybeObject
        [ ( "remoteImage", encodeRemoteImageInput |> Encode.optional input.remoteImage ), ( "text", encodeTextInput |> Encode.optional input.text ), ( "uploadImage", encodeUploadImageInput |> Encode.optional input.uploadImage ), ( "video", encodeVideoInput |> Encode.optional input.video ) ]


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


buildImageBoundingBoxInput : (ImageBoundingBoxInputOptionalFields -> ImageBoundingBoxInputOptionalFields) -> ImageBoundingBoxInput
buildImageBoundingBoxInput fillOptionals =
    let
        optionals =
            fillOptionals
                { class = Absent, xMax = Absent, xMin = Absent, yMax = Absent, yMin = Absent }
    in
    { class = optionals.class, xMax = optionals.xMax, xMin = optionals.xMin, yMax = optionals.yMax, yMin = optionals.yMin }


type alias ImageBoundingBoxInputOptionalFields =
    { class : OptionalArgument String, xMax : OptionalArgument Float, xMin : OptionalArgument Float, yMax : OptionalArgument Float, yMin : OptionalArgument Float }


{-| Type for the ImageBoundingBoxInput input object.
-}
type alias ImageBoundingBoxInput =
    { class : OptionalArgument String, xMax : OptionalArgument Float, xMin : OptionalArgument Float, yMax : OptionalArgument Float, yMin : OptionalArgument Float }


{-| Encode a ImageBoundingBoxInput into a value that can be used as an argument.
-}
encodeImageBoundingBoxInput : ImageBoundingBoxInput -> Value
encodeImageBoundingBoxInput input =
    Encode.maybeObject
        [ ( "class", Encode.string |> Encode.optional input.class ), ( "xMax", Encode.float |> Encode.optional input.xMax ), ( "xMin", Encode.float |> Encode.optional input.xMin ), ( "yMax", Encode.float |> Encode.optional input.yMax ), ( "yMin", Encode.float |> Encode.optional input.yMin ) ]


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


buildImageClassInput : (ImageClassInputOptionalFields -> ImageClassInputOptionalFields) -> ImageClassInput
buildImageClassInput fillOptionals =
    let
        optionals =
            fillOptionals
                { class = Absent }
    in
    { class = optionals.class }


type alias ImageClassInputOptionalFields =
    { class : OptionalArgument String }


{-| Type for the ImageClassInput input object.
-}
type alias ImageClassInput =
    { class : OptionalArgument String }


{-| Encode a ImageClassInput into a value that can be used as an argument.
-}
encodeImageClassInput : ImageClassInput -> Value
encodeImageClassInput input =
    Encode.maybeObject
        [ ( "class", Encode.string |> Encode.optional input.class ) ]


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


buildLabelInput : (LabelInputOptionalFields -> LabelInputOptionalFields) -> LabelInput
buildLabelInput fillOptionals =
    let
        optionals =
            fillOptionals
                { imageBoundingBox = Absent, imageClass = Absent }
    in
    { imageBoundingBox = optionals.imageBoundingBox, imageClass = optionals.imageClass }


type alias LabelInputOptionalFields =
    { imageBoundingBox : OptionalArgument ImageBoundingBoxInput, imageClass : OptionalArgument ImageClassInput }


{-| Type for the LabelInput input object.
-}
type alias LabelInput =
    { imageBoundingBox : OptionalArgument ImageBoundingBoxInput, imageClass : OptionalArgument ImageClassInput }


{-| Encode a LabelInput into a value that can be used as an argument.
-}
encodeLabelInput : LabelInput -> Value
encodeLabelInput input =
    Encode.maybeObject
        [ ( "imageBoundingBox", encodeImageBoundingBoxInput |> Encode.optional input.imageBoundingBox ), ( "imageClass", encodeImageClassInput |> Encode.optional input.imageClass ) ]


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


buildRemoteImageInput : RemoteImageInputRequiredFields -> (RemoteImageInputOptionalFields -> RemoteImageInputOptionalFields) -> RemoteImageInput
buildRemoteImageInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { caption = Absent }
    in
    { caption = optionals.caption, compressionFormat = required.compressionFormat, depth = required.depth, filesize = required.filesize, height = required.height, storagePath = required.storagePath, width = required.width }


type alias RemoteImageInputRequiredFields =
    { compressionFormat : String, depth : Int, filesize : Int, height : Int, storagePath : String, width : Int }


type alias RemoteImageInputOptionalFields =
    { caption : OptionalArgument String }


{-| Type for the RemoteImageInput input object.
-}
type alias RemoteImageInput =
    { caption : OptionalArgument String, compressionFormat : String, depth : Int, filesize : Int, height : Int, storagePath : String, width : Int }


{-| Encode a RemoteImageInput into a value that can be used as an argument.
-}
encodeRemoteImageInput : RemoteImageInput -> Value
encodeRemoteImageInput input =
    Encode.maybeObject
        [ ( "caption", Encode.string |> Encode.optional input.caption ), ( "compressionFormat", Encode.string input.compressionFormat |> Just ), ( "depth", Encode.int input.depth |> Just ), ( "filesize", Encode.int input.filesize |> Just ), ( "height", Encode.int input.height |> Just ), ( "storagePath", Encode.string input.storagePath |> Just ), ( "width", Encode.int input.width |> Just ) ]


buildTextInput : TextInputRequiredFields -> TextInput
buildTextInput required =
    { content = required.content }


type alias TextInputRequiredFields =
    { content : String }


{-| Type for the TextInput input object.
-}
type alias TextInput =
    { content : String }


{-| Encode a TextInput into a value that can be used as an argument.
-}
encodeTextInput : TextInput -> Value
encodeTextInput input =
    Encode.maybeObject
        [ ( "content", Encode.string input.content |> Just ) ]


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


buildUploadImageInput : UploadImageInputRequiredFields -> (UploadImageInputOptionalFields -> UploadImageInputOptionalFields) -> UploadImageInput
buildUploadImageInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { caption = Absent }
    in
    { caption = optionals.caption, compressionFormat = required.compressionFormat, content = required.content }


type alias UploadImageInputRequiredFields =
    { compressionFormat : String, content : String }


type alias UploadImageInputOptionalFields =
    { caption : OptionalArgument String }


{-| Type for the UploadImageInput input object.
-}
type alias UploadImageInput =
    { caption : OptionalArgument String, compressionFormat : String, content : String }


{-| Encode a UploadImageInput into a value that can be used as an argument.
-}
encodeUploadImageInput : UploadImageInput -> Value
encodeUploadImageInput input =
    Encode.maybeObject
        [ ( "caption", Encode.string |> Encode.optional input.caption ), ( "compressionFormat", Encode.string input.compressionFormat |> Just ), ( "content", Encode.string input.content |> Just ) ]


buildVideoInput : VideoInputRequiredFields -> (VideoInputOptionalFields -> VideoInputOptionalFields) -> VideoInput
buildVideoInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { caption = Absent }
    in
    { caption = optionals.caption, content = required.content }


type alias VideoInputRequiredFields =
    { content : Api.Scalar.Upload }


type alias VideoInputOptionalFields =
    { caption : OptionalArgument String }


{-| Type for the VideoInput input object.
-}
type alias VideoInput =
    { caption : OptionalArgument String, content : Api.Scalar.Upload }


{-| Encode a VideoInput into a value that can be used as an argument.
-}
encodeVideoInput : VideoInput -> Value
encodeVideoInput input =
    Encode.maybeObject
        [ ( "caption", Encode.string |> Encode.optional input.caption ), ( "content", (\(Api.Scalar.Upload raw) -> Encode.string raw) input.content |> Just ) ]
