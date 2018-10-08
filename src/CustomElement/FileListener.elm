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


{-| Here's our custom element.
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


{-| `onLoad` receives a `File` instance from the JS code.
-}
type alias File =
    { name : String
    , lastModified : Int
    , mimeType : String
    , data : String
    }


fileDecoder : Decoder File
fileDecoder =
    JD.map4 File
        (JD.field "name" JD.string)
        (JD.field "lastModified" JD.int)
        (JD.field "mimeType" JD.string)
        (JD.field "data" JD.string)


{-| Encode a `File` as a [Data URI](https://css-tricks.com/data-uris/).
-}
fileToDataUri : File -> String
fileToDataUri file =
    "data:" ++ file.mimeType ++ ";base64," ++ Base64.encode file.data


{-| This is how you receive changes to the contents of the code editor.
-}
onLoad : (File -> msg) -> Attribute msg
onLoad tagger =
    on "load" <|
        JD.map tagger <|
            JD.at [ "target", "contents" ]
                fileDecoder


crlf : String
crlf =
    -- elm-format rewrites \r to \x0D, and that doesn't compile
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
