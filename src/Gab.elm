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
        , popularFeed
        , popularFeedParts
        , popularUsers
        , popularUsersParts
        , request
        , requestParts
        , userFollowers
        , userFollowersParts
        , userFollowing
        , userFollowingParts
        , userProfile
        , userProfileParts
        )

import Erl.Query
import Gab.EncodeDecode as ED
import Gab.Types exposing (HttpBody(..), PostList, RequestParts, User, UserList)
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


{-| Shared by userFollowersParts & userFollowingParts
-}
userXxxParts : String -> Decoder a -> Token -> String -> Int -> RequestParts a
userXxxParts xxx decoder token username before =
    let
        prefix =
            "users/" ++ username ++ xxx

        path =
            if before <= 0 then
                prefix
            else
                prefix ++ "?before=" ++ toString before
    in
    getParts decoder token path


{-| Return the logged-in user's followers as a UserList record.

    userFollowers token username before

`before` is the number of following users to skip before the listing starts. This enables scrolling through a long list.

-}
userFollowers : Token -> String -> Int -> Http.Request UserList
userFollowers token username before =
    userFollowersParts ED.userListDecoder token username before
        |> request


{-| Return the logged-in user's followers, using a custom decoder.

    userFollowersParts decoder token username before

`before` is the number of following users to skip before the listing starts. This enables scrolling through a long list.

-}
userFollowersParts : Decoder a -> Token -> String -> Int -> RequestParts a
userFollowersParts =
    userXxxParts "/followers"


{-| Return a list of users following the logged-in user, as a UserList record.

    userFollowing token username before

`before` is the number of followers to skip before the listing starts. This enables scrolling through a long list.

-}
userFollowing : Token -> String -> Int -> Http.Request UserList
userFollowing token username before =
    userFollowingParts ED.userListDecoder token username before
        |> request


{-| Return a list of users following the logged-in user, using a custom decoder.

    userFollowingParts decoder token username before

`before` is the number of followers to skip before the listing starts. This enables scrolling through a long list.

-}
userFollowingParts : Decoder a -> Token -> String -> Int -> RequestParts a
userFollowingParts =
    userXxxParts "/following"


{-| Return a list of popular users, as a UserList record.

    popularUsers token

-}
popularUsers : Token -> Http.Request UserList
popularUsers token =
    popularUsersParts ED.userListDecoder token
        |> request


{-| Return a list of popular users, using a custom decoder.

    popularUserParts decoder token

-}
popularUsersParts : Decoder a -> Token -> RequestParts a
popularUsersParts decoder token =
    getParts decoder token "popular/users"


{-| Shared by all the getters that take before and after dates.
-}
beforeAfterParts : String -> Decoder a -> Token -> String -> String -> RequestParts a
beforeAfterParts xxx decoder token before after =
    let
        prefix =
            xxx

        query =
            []
                |> (if before == "" then
                        identity
                    else
                        Erl.Query.add "before" before
                   )
                |> (if after == "" then
                        identity
                    else
                        Erl.Query.add "after" after
                   )

        path =
            prefix ++ Erl.Query.toString query
    in
    getParts decoder token path


{-| Return the posts in the "popular" feed, as a PostList.

    popularFeed token before after

The posts returned will have dates between `before` and `after`. Pass the empty string for either to not limit that end.

-}
popularFeed : Token -> String -> String -> Http.Request PostList
popularFeed token before after =
    popularFeedParts ED.postListDecoder token before after
        |> request


{-| Return the posts in the "popular" feed, using a custom decoder.

    popularFeedParts decoder token before after

The posts returned will have dates between `before` and `after`. Pass the empty string for either to not limit that end.

-}
popularFeedParts : Decoder a -> Token -> String -> String -> RequestParts a
popularFeedParts decoder token before after =
    beforeAfterParts "popular/feed" decoder token before after
