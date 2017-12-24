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


module Gab exposing (gabApiUri, get, me, request)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
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
request : String -> List Http.Header -> Http.Body -> Token -> String -> Decoder a -> Http.Request a
request method headers body token path decoder =
    Http.request
        { method = method
        , headers = OAuth.use token headers
        , url = gabApiUri ++ path
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


{-| Simple HTTP GET request. Empty body, no custom headers.

    get token path decoder

-}
get : Token -> String -> Decoder a -> Http.Request a
get =
    request "GET" [] Http.emptyBody


{-| Return the logged-in user's profile information.

Currently not decoded, just a raw `Value`.

-}
me : Token -> Http.Request Value
me token =
    get token "me" JD.value
