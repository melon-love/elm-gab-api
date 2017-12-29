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
        , dislikePost
        , dislikePostParts
        , doParts
        , doPostsParts
        , doUsersParts
        , followUser
        , followUserParts
        , gabApiUri
        , getParts
        , getPost
        , getPostParts
        , homeFeed
        , homeFeedParts
        , likePost
        , likePostParts
        , me
        , meParts
        , muteUser
        , muteUserParts
        , popularFeed
        , popularFeedParts
        , popularUsers
        , popularUsersParts
        , repost
        , repostParts
        , request
        , requestParts
        , userFeed
        , userFeedParts
        , userFollowers
        , userFollowersParts
        , userFollowing
        , userFollowingParts
        , userProfile
        , userProfileParts
        )

import Erl.Query
import Gab.EncodeDecode as ED
import Gab.Types exposing (ActivityLogList, HttpBody(..), Post, RequestParts, User, UserList)
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


{-| Follow or unfollow a user. Return value not interesting.

    followUser token username unfollow

-}
followUser : Token -> String -> Bool -> Http.Request Value
followUser token username unfollow =
    followUserParts JD.value token username unfollow
        |> request


{-| Mute or unmute a user. Return value not interesting.

    muteUser token username unmute

-}
muteUser : Token -> String -> Bool -> Http.Request Value
muteUser token username unlike =
    muteUserParts JD.value token username unlike
        |> request


{-| Shared by doUsersParts and doPostParts

    doParts prefix operation decoder token identifier undo

`prefix` can be "users" or "posts".

-}
doParts : String -> String -> Decoder a -> Token -> String -> Bool -> RequestParts a
doParts prefix operation decoder token identifier undo =
    let
        method =
            if undo then
                "DELETE"
            else
                "POST"

        path =
            prefix ++ "/" ++ identifier ++ "/" ++ operation
    in
    requestParts method [] EmptyBody decoder token path


{-| Shared by followUserParts and muteUserParts

    doUsersParts operation decoder token username undo

`operation` can be "follow" or "mute".

-}
doUsersParts : String -> Decoder a -> Token -> String -> Bool -> RequestParts a
doUsersParts =
    doParts "users"


{-| Follow or unfollow a user, with a custom decoder.

    followUserParts decoder token username unfollow

-}
followUserParts : Decoder a -> Token -> String -> Bool -> RequestParts a
followUserParts =
    doUsersParts "follow"


{-| Mute or unmute a user, with a custom decoder.

    muteUserParts decoder token username unmute

-}
muteUserParts : Decoder a -> Token -> String -> Bool -> RequestParts a
muteUserParts =
    doUsersParts "mute"


{-| Shared by all the getters that take before and after dates.

    beforeAfterParts prefix decoder token before after

-}
beforeAfterParts : String -> Decoder a -> Token -> String -> String -> RequestParts a
beforeAfterParts prefix decoder token before after =
    let
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


{-| Return the posts in the "popular" feed, as a ActivityLogList.

    popularFeed token before after

The posts returned will have dates between `before` and `after`. Pass the empty string for either to not limit that end.

-}
popularFeed : Token -> String -> String -> Http.Request ActivityLogList
popularFeed token before after =
    popularFeedParts ED.activityLogListDecoder token before after
        |> request


{-| Return the posts in the "popular" feed, using a custom decoder.

    popularFeedParts decoder token before after

The posts returned will have dates between `before` and `after`. Pass the empty string for either to not limit that end.

-}
popularFeedParts : Decoder a -> Token -> String -> String -> RequestParts a
popularFeedParts decoder token before after =
    beforeAfterParts "popular/feed" decoder token before after


{-| Return posts in the home feed.

    homeFeed token before after.

The posts returned will have dates between `before` and `after`. Pass the empty string for either to not limit that end.

-}
homeFeed : Token -> String -> String -> Http.Request ActivityLogList
homeFeed token before after =
    homeFeedParts ED.activityLogListDecoder token before after
        |> request


{-| Return posts in the home feed, using a custom encoder.

    homeFeedParts decoder token before after

The posts returned will have dates between `before` and `after`. Pass the empty string for either to not limit that end.

-}
homeFeedParts : Decoder a -> Token -> String -> String -> RequestParts a
homeFeedParts decoder token before after =
    beforeAfterParts "feed" decoder token before after


{-| Return posts for a user feed.

    userFeed token user before after.

The posts returned will have dates between `before` and `after`. Pass the empty string for either to not limit that end.

-}
userFeed : Token -> String -> String -> String -> Http.Request ActivityLogList
userFeed token user before after =
    userFeedParts ED.activityLogListDecoder token user before after
        |> request


{-| Return posts for a user feed, using a custom decoder.

    userFeedParts decoder user token before after

The posts returned will have dates between `before` and `after`. Pass the empty string for either to not limit that end.

-}
userFeedParts : Decoder a -> Token -> String -> String -> String -> RequestParts a
userFeedParts decoder token user before after =
    beforeAfterParts ("users/" ++ user ++ "/feed") decoder token before after


{-| Get a single post.

    getPost token postid

-}
getPost : Token -> String -> Http.Request Post
getPost token postid =
    getPostParts ED.postDecoder token postid
        |> request


{-| Get a single post, using a custom decoder.

    getPostParts decoder token postid

-}
getPostParts : Decoder a -> Token -> String -> RequestParts a
getPostParts decoder token postid =
    getParts decoder token <| "posts/" ++ postid


{-| Like or unlike a post. Return value not interesting.

    likePost token postid unlike

-}
likePost : Token -> String -> Bool -> Http.Request Value
likePost token postid unlike =
    likePostParts JD.value token postid unlike
        |> request


{-| Dislike or undislike a post. Return value not interesting.

    dislikePost token postid undislike

-}
dislikePost : Token -> String -> Bool -> Http.Request Value
dislikePost token postid undislike =
    dislikePostParts JD.value token postid undislike
        |> request


{-| Repost or unrepost. Return value not interesting.

    repost token postid unrepost

-}
repost : Token -> String -> Bool -> Http.Request Value
repost token postid unrepost =
    repostParts JD.value token postid unrepost
        |> request


{-| Shared by likePostParts, dislikePostParts, repostParts

    doPostsParts operation decoder token username undo

`operation` can be "like", "dislike" or "repost".

-}
doPostsParts : String -> Decoder a -> Token -> String -> Bool -> RequestParts a
doPostsParts =
    doParts "posts"


{-| Like or unlike a post, with a custom decoder.

    likePostParts decoder token postid unlike

-}
likePostParts : Decoder a -> Token -> String -> Bool -> RequestParts a
likePostParts =
    doPostsParts "like"


{-| Dislike or undislike a post, with a custom decoder.

    dislikePostParts decoder token postid undislike

-}
dislikePostParts : Decoder a -> Token -> String -> Bool -> RequestParts a
dislikePostParts =
    doPostsParts "dislike"


{-| Repost or unrepost, with a custom decoder.

    repostParts decoder token postid unrepost

-}
repostParts : Decoder a -> Token -> String -> Bool -> RequestParts a
repostParts =
    doPostsParts "repost"
