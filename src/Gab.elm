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


module OAuthMiddleware exposing (gabApiUri, request)

import Http
import Json.Decode exposing (Decoder)
import OAuth exposing (Token)


{-| The base URI for GAB API requests. Automatically prepended to `path` args.
-}
gabApiUri : String
gabApiUri =
    "https://api.gab.ai/v1.0/"


{-| General-purpose `Http.Request` constructor.
-}
request : String -> List Http.Header -> Token -> String -> Http.Body -> Decoder a -> Http.Request a
request method headers token path body decoder =
    Http.request
        { method = method
        , headers = OAuth.use token headers
        , url = gabApiUri ++ path
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
