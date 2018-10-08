module CustomElement.FileListener exposing
    ( File
    , fileListener
    , fileId
    , onLoad
    , fileToDataUri, multipartFormContentType, multipartFormData
    )

{-| The Elm side of the custom element defined in `site/js/file-listener.js`.


# Types

@docs File


# HTML Elements

@docs fileListener


# Attributes

@docs fileId


# Events

@docs onLoad


# Convenience Functions

@docs fileToDataUri, multipartFormContentType, multipartFormData

-}

import Base64
import Char
import Html exposing (Attribute, Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| `onLoad` receives a `File` instance from the JS code.
-}
type alias File =
    { name : String
    , lastModified : Int
    , mimeType : String
    , data : String
    }


{-| The custom `file-listener` element.

It's invisible, but it adds an event listener to the asociated `<input type='file' id='fileId' />` element to fetch the contents of the file, and generate a `"load"` event containing the contents and other information.

-}
fileListener : List (Attribute msg) -> List (Html msg) -> Html msg
fileListener =
    Html.node "file-listener"


{-| You need to set the `fileId` attribute to the `id` of an `input` of type `file`.
-}
fileId : String -> Attribute msg
fileId value =
    property "fileId" <|
        JE.string value


{-| This is how you receive file content and other information.
-}
onLoad : (File -> msg) -> Attribute msg
onLoad tagger =
    on "load" <|
        JD.map tagger <|
            JD.at [ "target", "contents" ]
                fileDecoder


fileDecoder : Decoder File
fileDecoder =
    JD.map4 File
        (JD.field "name" JD.string)
        (JD.field "lastModified" JD.int)
        (JD.field "mimeType" JD.string)
        (JD.field "data" JD.string)


{-| Encode a `File` as a [Data URI](https://css-tricks.com/data-uris/).

Suitable as the `src` for an `img` element.

-}
fileToDataUri : File -> String
fileToDataUri file =
    let
        -- truqu/elm-base64's UTF-8 handling munges binary data,
        -- so we copy twice to get a list of `Int`s,
        -- and use waratuman/elm-coder to do the encoding.
        list =
            String.toList file.data
                |> List.map Char.toCode

        base64 =
            -- This can't error, so I don't know why it returns a `Result`
            Result.withDefault "" <| Base64.encode list
    in
    "data:" ++ file.mimeType ++ ";base64," ++ base64


crlf : String
crlf =
    -- elm-format rewrites "\r" or "\u{000d}" to "\x0D", and that doesn't compile.
    List.map Char.fromCode [ 13, 10 ]
        |> String.fromList


{-| Turn a `boundary` string into a `multipart/form-data` mime type.

This is suitable as the first parameter to `Http.stringBody`.

-}
multipartFormContentType : String -> String
multipartFormContentType boundary =
    "multipart/form-data; boundary=" ++ boundary


{-| Turn a `boundary` string and a `File` into the body of a multipart form post.

This is suitable as the second parameter to `Http.stringBody`.

-}
multipartFormData : String -> File -> String
multipartFormData boundary file =
    "--"
        ++ boundary
        ++ crlf
        ++ "Content-Disposition: form-data; name=\"file\"; filename=\""
        ++ file.name
        ++ "\""
        ++ crlf
        ++ "Content-Type: "
        ++ file.mimeType
        ++ crlf
        ++ crlf
        ++ file.data
        ++ crlf
        ++ "--"
        ++ boundary
        ++ "--"
