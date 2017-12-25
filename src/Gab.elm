----------------------------------------------------------------------
--
-- Gab.elm
-- Pure Elm client for the Gab.ai API.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Gab
    exposing
        ( bodyToString
        , gabApiUri
        , getParts
        , me
        , meParts
        , request
        , requestParts
        , userProfile
        , userProfileParts
        )

import Gab.EncodeDecode as ED
import Gab.Types exposing (HttpBody(..), RequestParts, User)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OAuth exposing (Token)


{-| The base URI for GAB API requests. Automatically prepended to `path` args.
-}
gabApiUri : String
gabApiUri =
    "https://api.gab.ai/v1.0/"


{-| General-purpose `Http.Request` constructor.

    request method headers token path body decoder

`method` is the Http method, e.g. "GET".

`headers` is a list of custom `Http.Header`s.

`token` is an OAuth token, often gotten with [`billstclair/elm-oauth-middleware`](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest).

`path` is the path to the API endpoint, with NO leading slash.

`decoder` is a JSON decoder for the result.

-}
requestParts : String -> List Http.Header -> HttpBody -> Decoder a -> Token -> String -> RequestParts a
requestParts method headers body decoder token path =
    { method = method
    , headers = OAuth.use token headers
    , url = gabApiUri ++ path
    , body = body
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }


{-| Debugging function for displaying a request body.
-}
bodyToString : Int -> HttpBody -> String
bodyToString indent body =
    case body of
        EmptyBody ->
            ""

        JsonBody value ->
            JE.encode indent value

        StringBody mimetype string ->
            mimetype ++ ": " ++ string

        OtherBody _ ->
            "<opaque>"


realizeBody : HttpBody -> Http.Body
realizeBody body =
    case body of
        EmptyBody ->
            Http.emptyBody

        JsonBody value ->
            Http.jsonBody value

        StringBody mimetype string ->
            Http.stringBody mimetype string

        OtherBody body ->
            body


request : RequestParts a -> Http.Request a
request parts =
    Http.request
        { method = parts.method
        , headers = parts.headers
        , url = parts.url
        , body = realizeBody parts.body
        , expect = parts.expect
        , timeout = parts.timeout
        , withCredentials = parts.withCredentials
        }


{-| Simple HTTP GET request. Empty body, no custom headers.

    get token path decoder

-}
getParts : Decoder a -> Token -> String -> RequestParts a
getParts =
    requestParts "GET" [] EmptyBody


{-| Return the logged-in user's profile information as a User record.
-}
me : Token -> Http.Request User
me token =
    meParts ED.userDecoder token
        |> request


{-| Return the logged-in user's profile information, using a custom decoder.
-}
meParts : Decoder a -> Token -> RequestParts a
meParts decoder token =
    getParts decoder token "me"


{-| Return the logged-in user's profile information as a User record.
-}
userProfile : Token -> String -> Http.Request User
userProfile token username =
    userProfileParts ED.userDecoder token username
        |> request


{-| Return the logged-in user's profile information, using a custom decoder.
-}
userProfileParts : Decoder a -> Token -> String -> RequestParts a
userProfileParts decoder token username =
    getParts decoder token <| "users/" ++ username
