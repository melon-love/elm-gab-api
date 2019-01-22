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


module Gab exposing
    ( me, userProfile
    , userFollowers, userFollowing
    , followUser, muteUser
    , homeFeed
    , userFeed
    , groupFeed
    , popularFeed
    , popularUsers
    , notifications
    , getPost
    , upvotePost, downvotePost
    , repost
    , newPost
    , postImage
    , meParts, userProfileParts
    , userFollowersParts, userFollowingParts
    , followUserParts, muteUserParts
    , homeFeedParts
    , userFeedParts
    , groupFeedParts
    , popularFeedParts
    , popularUsersParts
    , notificationsParts
    , getPostParts
    , upvotePostParts, downvotePostParts
    , repostParts
    , newPostParts
    , postImageParts
    , doParts, doUsersParts, doPostsParts
    , savedTokenFromResponseToken
    , gabApiUri, request, getParts, requestParts
    , bodyToString
    )

{-| Client for the Gab.com API, documented at [developers.gab.com](https://developers.gab.com/).

This does NOT do authentication. You'll need to use [billstclair/elm-oauth-middleware](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest) to get a `Token`. See the `example` directory.

The requests all come in two flavors, one which has the decoder built in, and returns a `Cmd msg`, and one for which you provide your own decoder. E.g.:

    type Msg
       = ReceiveUser (Result Http.Error User)
       | ReceiveUserValue (Result Http.Error Value)
       ...

    getTorbaCmd : OAuth.Token -> Cmd Msg
    getTorbaCmd token =
        userProfile ReceiveUser token "a"

    getTorbaParts : OAuth.Token -> RequestParts Msg
    getTorbaParts token =
        userProfileParts Json.Decode.value ReceiveUserValue token "a"


# Normal, Auto-Decoding Functions

You'll usually use these, not the `Parts` functions below.


## Users

@docs me, userProfile
@docs userFollowers, userFollowing


## User Interaction

@docs followUser, muteUser


## Feeds

@docs homeFeed
@docs userFeed
@docs groupFeed
@docs popularFeed
@docs popularUsers


## Notifications

@docs notifications


## Posts

@docs getPost
@docs upvotePost, downvotePost
@docs repost


## New Posts

@docs newPost
@docs postImage


# Parts Functions The Return a Value

These are mostly for the example, but if you need to get your hands on the raw return `Value` from the API, use these intead of the auto-decoding versions.


## Users

@docs meParts, userProfileParts
@docs userFollowersParts, userFollowingParts


## User Interaction

@docs followUserParts, muteUserParts


## Feeds

@docs homeFeedParts
@docs userFeedParts
@docs groupFeedParts
@docs popularFeedParts
@docs popularUsersParts


## Notifications

@docs notificationsParts


## Posts

@docs getPostParts
@docs upvotePostParts, downvotePostParts
@docs repostParts


## New Posts

@docs newPostParts
@docs postImageParts


# Generic requests

These are low-level functions used to implement the others. You won't need them unless you need to implement new API functionality that isn't yet in this module.

@docs doParts, doUsersParts, doPostsParts


# Persistent tokens

@docs savedTokenFromResponseToken


# Low-level Http interface

@docs gabApiUri, request, getParts, requestParts


# Debugging

@docs bodyToString

-}

import Char
import File exposing (File)
import Gab.EncodeDecode as ED
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , HttpBody(..)
        , NotificationsLog
        , Post
        , PostForm
        , RequestParts
        , SavedToken
        , Success
        , User
        , UserList
        )
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OAuth exposing (Token)
import OAuthMiddleware.ResponseToken exposing (ResponseToken)
import Time exposing (Posix)
import Url.Builder as Builder


{-| The base URI for GAB API requests. Automatically prepended to `path` args.
-}
gabApiUri : String
gabApiUri =
    "https://api.gab.com/v1.0/"


{-| General-purpose `Http.Request` constructor.

    request method headers wrapper token path body decoder

`method` is the Http method, e.g. "GET".

`headers` is a list of custom `Http.Header`s.

`wrapper` maps a `Result` to a `msg`.

`token` is an OAuth token, often gotten with [`billstclair/elm-oauth-middleware`](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest).

`path` is the path to the API endpoint, with NO leading slash.

`decoder` is a JSON decoder for the result.

-}
requestParts : String -> List Http.Header -> HttpBody -> Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
requestParts method headers body decoder wrapper token path =
    { method = method
    , headers = OAuth.useToken token headers
    , url = gabApiUri ++ path
    , body = body
    , expect = Http.expectJson wrapper decoder
    , timeout = Nothing
    , tracker = Nothing
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
            "mimetype: " ++ mimetype ++ "\n\n" ++ string

        FileBody file ->
            "name: "
                ++ File.name file
                ++ ", mime: "
                ++ File.mime file
                ++ ", size: "
                ++ String.fromInt (File.size file)

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

        FileBody file ->
            Http.fileBody file

        OtherBody bod ->
            bod


{-| Turn parts into a ready-to-send `Cmd msg`.
-}
request : RequestParts msg -> Cmd msg
request parts =
    Http.request
        { method = parts.method
        , headers = parts.headers
        , url = parts.url
        , body = realizeBody parts.body
        , expect = parts.expect
        , timeout = parts.timeout
        , tracker = parts.tracker
        }


{-| Simple HTTP GET request. Empty body, no custom headers.

    get decoder wrapper token wrapper path

-}
getParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
getParts =
    requestParts "GET" [] EmptyBody


{-| Return the logged-in user's profile information as a User record.
-}
me : (Result Http.Error User -> msg) -> Token -> Cmd msg
me wrapper token =
    meParts ED.userDecoder wrapper token
        |> request


{-| Return the logged-in user's profile information, using a custom decoder.
-}
meParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> RequestParts msg
meParts decoder wrapper token =
    getParts decoder wrapper token "me/"


{-| Return the logged-in user's profile information as a User record.
-}
userProfile : (Result Http.Error User -> msg) -> Token -> String -> Cmd msg
userProfile wrapper token username =
    userProfileParts ED.userDecoder wrapper token username
        |> request


{-| Return the logged-in user's profile information, using a custom decoder.
-}
userProfileParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
userProfileParts decoder wrapper token username =
    getParts decoder wrapper token <| "users/" ++ username


{-| Shared by userFollowersParts & userFollowingParts
-}
userXxxParts : String -> Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Int -> RequestParts msg
userXxxParts xxx decoder wrapper token username before =
    let
        prefix =
            "users/" ++ username ++ xxx

        path =
            if before <= 0 then
                prefix

            else
                prefix ++ "?before=" ++ String.fromInt before
    in
    getParts decoder wrapper token path


{-| Return the logged-in user's followers as a UserList record.

    userFollowers wrapper token username before

`before` is the number of following users to skip before the listing starts. This enables scrolling through a long list.

-}
userFollowers : (Result Http.Error UserList -> msg) -> Token -> String -> Int -> Cmd msg
userFollowers wrapper token username before =
    userFollowersParts ED.userListDecoder wrapper token username before
        |> request


{-| Return the logged-in user's followers, using a custom decoder.

    userFollowersParts wrapper decoder token username before

`before` is the number of following users to skip before the listing starts. This enables scrolling through a long list.

-}
userFollowersParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Int -> RequestParts msg
userFollowersParts =
    userXxxParts "/followers"


{-| Return a list of users following the logged-in user, as a UserList record.

    userFollowing wrapper token username before

`before` is the number of followers to skip before the listing starts. This enables scrolling through a long list.

-}
userFollowing : (Result Http.Error UserList -> msg) -> Token -> String -> Int -> Cmd msg
userFollowing wrapper token username before =
    userFollowingParts ED.userListDecoder wrapper token username before
        |> request


{-| Return a list of users following the logged-in user, using a custom decoder.

    userFollowingParts wrapper decoder token username before

`before` is the number of followers to skip before the listing starts. This enables scrolling through a long list.

-}
userFollowingParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Int -> RequestParts msg
userFollowingParts =
    userXxxParts "/following"


{-| Return a list of popular users, as a UserList record.

    popularUsers wrapper token

-}
popularUsers : (Result Http.Error UserList -> msg) -> Token -> Cmd msg
popularUsers wrapper token =
    popularUsersParts ED.userListDecoder wrapper token
        |> request


{-| Return a list of popular users, using a custom decoder.

    popularUserParts wrapper decoder token

-}
popularUsersParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> RequestParts msg
popularUsersParts decoder wrapper token =
    getParts decoder wrapper token "popular/users"


{-| Follow or unfollow a user. Return value not interesting.

    followUser wrapper token username unfollow

-}
followUser : (Result Http.Error Success -> msg) -> Token -> String -> Bool -> Cmd msg
followUser wrapper token username unfollow =
    followUserParts ED.successDecoder wrapper token username unfollow
        |> request


{-| Mute or unmute a user. Return value not interesting.

    muteUser wrapper token username unmute

This isn't currently implemented by the API, but I expect that to change.

-}
muteUser : (Result Http.Error Success -> msg) -> Token -> String -> Bool -> Cmd msg
muteUser wrapper token username unmute =
    muteUserParts ED.successDecoder wrapper token username unmute
        |> request


{-| Shared by doUsersParts and doPostParts

    doParts prefix operation decoder wrapper token identifier undo

`prefix` can be "users" or "posts".

If `undo` is `True`, does a DELETE. Otherwise, does a POST.

-}
doParts : String -> String -> Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Bool -> RequestParts msg
doParts prefix operation decoder wrapper token identifier undo =
    let
        method =
            if undo then
                "DELETE"

            else
                "POST"

        path =
            prefix ++ "/" ++ identifier ++ "/" ++ operation
    in
    requestParts method [] EmptyBody decoder wrapper token path


{-| Shared by followUserParts and muteUserParts

    doUsersParts operation decoder wrapper token username undo

`operation` can be "follow" or "mute".

-}
doUsersParts : String -> Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Bool -> RequestParts msg
doUsersParts =
    doParts "users"


{-| Follow or unfollow a user, with a custom decoder.

    followUserParts decoder wrapper token username unfollow

-}
followUserParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Bool -> RequestParts msg
followUserParts =
    doUsersParts "follow"


{-| Mute or unmute a user, with a custom decoder.

    muteUserParts decoder wrapper token username unmute

This isn't currently implemented by the API, but I expect that to change.

-}
muteUserParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Bool -> RequestParts msg
muteUserParts =
    doUsersParts "mute"


{-| Shared by all the getters that take before dates.

    beforeAfterParts prefix decoder wrapper token before

-}
beforeAfterParts : String -> Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
beforeAfterParts prefix decoder wrapper token before =
    let
        queries =
            if before == "" then
                []

            else
                [ Builder.string "before" before ]

        path =
            prefix ++ Builder.toQuery queries
    in
    getParts decoder wrapper token path


{-| Return the posts in the "popular" feed, as a ActivityLogList.

    popularFeed wrapper token

-}
popularFeed : (Result Http.Error ActivityLogList -> msg) -> Token -> Cmd msg
popularFeed wrapper token =
    popularFeedParts ED.activityLogListDecoder wrapper token
        |> request


{-| Return the posts in the "popular" feed, using a custom decoder.

    popularFeedParts decoder wrapper token

-}
popularFeedParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> RequestParts msg
popularFeedParts decoder wrapper token =
    beforeAfterParts "popular/feed" decoder wrapper token ""


{-| Return posts in the home feed.

    homeFeed wrapper token before

The posts returned will have dates before `before`. Pass the empty string to get the beginning of the list.

-}
homeFeed : (Result Http.Error ActivityLogList -> msg) -> Token -> String -> Cmd msg
homeFeed wrapper token before =
    homeFeedParts ED.activityLogListDecoder wrapper token before
        |> request


{-| Return posts in the home feed, using a custom encoder.

    homeFeedParts decoder wrapper token before

The posts returned will have dates before `before`. Pass the empty string to get the beginning of the list.

-}
homeFeedParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
homeFeedParts decoder wrapper token before =
    beforeAfterParts "feed" decoder wrapper token before


{-| Return posts for a user feed.

    userFeed wrapper token user before

The posts returned will have dates before `before`. Pass the empty string to get the beginning of the list.

-}
userFeed : (Result Http.Error ActivityLogList -> msg) -> Token -> String -> String -> Cmd msg
userFeed wrapper token user before =
    userFeedParts ED.activityLogListDecoder wrapper token user before
        |> request


{-| Return posts for a user feed, using a custom decoder.

    userFeedParts decoder wrapper user token before

The posts returned will have dates before `before`. Pass the empty string to get the beginning of the list.

-}
userFeedParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> String -> RequestParts msg
userFeedParts decoder wrapper token user before =
    beforeAfterParts ("users/" ++ user ++ "/feed") decoder wrapper token before


{-| Return a list of popular groups.
-}
popularGroups : (Result Http.Error Value -> msg) -> Token -> Cmd msg
popularGroups wrapper token =
    popularGroupsParts JD.value wrapper token
        |> request


{-| Return a list of popular groups, using a custom decoder.
-}
popularGroupsParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> RequestParts msg
popularGroupsParts decoder wrapper token =
    getParts decoder wrapper token "groups"


{-| Return details for a particular group.

    groupDetails wrapper token groupid

-}
groupDetails : (Result Http.Error Value -> msg) -> Token -> String -> Cmd msg
groupDetails wrapper token groupid =
    groupDetailsParts JD.value wrapper token groupid
        |> request


{-| Return details for a particular group, using a custom decoder.

    groupDetails decoder wrapper token groupid

-}
groupDetailsParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
groupDetailsParts decoder wrapper token groupid =
    getParts decoder wrapper token <| "groups/" ++ groupid


{-| Return user for a group.

    userFeed wrapper token groupid before

The users are numbered from the first one to join. Pass 0 for `before` to get the beginning of the list.

-}
groupUsers : (Result Http.Error Value -> msg) -> Token -> String -> Int -> Cmd msg
groupUsers wrapper token groupid before =
    groupUsersParts JD.value wrapper token groupid before
        |> request


groupUsersParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Int -> RequestParts msg
groupUsersParts decoder wrapper token groupid before =
    beforeAfterParts ("groups/" ++ groupid ++ "/users")
        decoder
        wrapper
        token
    <|
        String.fromInt before


{-| Return the moderation logs for a group.

    groupModerationLogs wrapper token groupid

-}
groupModerationLogs : (Result Http.Error Value -> msg) -> Token -> String -> Cmd msg
groupModerationLogs wrapper token groupid =
    groupModerationLogsParts JD.value wrapper token groupid
        |> request


{-| Return the moderation logs for a group, using a custom decoder.

    groupModerationLogs decoder wrapper token groupid

-}
groupModerationLogsParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
groupModerationLogsParts decoder wrapper token groupid =
    getParts decoder wrapper token <| "groups/" ++ groupid ++ "/moderation-logs"


{-| Return posts for a group feed.

    groupFeed wrapper token group before

The posts returned will have dates before `before`. Pass the empty string to get the beginning of the list.

This is a guess at what this API command will look like. It doesn't yet exist.

-}
groupFeed : (Result Http.Error ActivityLogList -> msg) -> Token -> String -> String -> Cmd msg
groupFeed wrapper token groupid before =
    groupFeedParts ED.activityLogListDecoder wrapper token groupid before
        |> request


{-| Return posts for a group feed, using a custom decoder.

    groupFeedParts decoder wrapper token groupid before

The posts returned will have dates before `before`. Pass the empty string to get the beginning of the list.

This is a guess at what this API command will look like. It doesn't yet exist.

-}
groupFeedParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> String -> RequestParts msg
groupFeedParts decoder wrapper token groupid before =
    beforeAfterParts ("groups/" ++ groupid ++ "/feed") decoder wrapper token before


{-| Return notifications for the logged in user.

    notifications wrapper token before

For notifications, the `before` parameter is a notification ID, not a date.

-}
notifications : (Result Http.Error NotificationsLog -> msg) -> Token -> String -> Cmd msg
notifications wrapper token before =
    notificationsParts ED.notificationsLogDecoder wrapper token before
        |> request


{-| Return notifications, using a custom decoder.

    notificationsParts decoder wrapper token before

For notifications, the `before` parameter is a notification ID, not a date.

-}
notificationsParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
notificationsParts decoder wrapper token before =
    beforeAfterParts "notifications" decoder wrapper token before


{-| Get a single post.

    getPost wrapper token postid

-}
getPost : (Result Http.Error Post -> msg) -> Token -> String -> Cmd msg
getPost wrapper token postid =
    getPostParts ED.postDecoder wrapper token postid
        |> request


{-| Get a single post, using a custom decoder.

    getPostParts decoder wrapper token postid

-}
getPostParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> RequestParts msg
getPostParts decoder wrapper token postid =
    getParts decoder wrapper token <| "posts/" ++ postid


{-| Upvote or unupvote a post. Return value not interesting.

    upvotePost wrapper token postid unupvote

-}
upvotePost : (Result Http.Error Success -> msg) -> Token -> Int -> Bool -> Cmd msg
upvotePost wrapper token postid unupvote =
    upvotePostParts ED.successDecoder wrapper token postid unupvote
        |> request


{-| Downvote or undownvote a post. Return value not interesting.

    downvotePost wrapper token postid undownvote

This will return an Http `BadStatus` error if you you're not
authorized to downvote.

-}
downvotePost : (Result Http.Error Success -> msg) -> Token -> Int -> Bool -> Cmd msg
downvotePost wrapper token postid undownvote =
    downvotePostParts ED.successDecoder wrapper token postid undownvote
        |> request


{-| Repost or unrepost. Return value not interesting.

    repost wrapper token postid unrepost

-}
repost : (Result Http.Error Success -> msg) -> Token -> Int -> Bool -> Cmd msg
repost wrapper token postid unrepost =
    repostParts ED.successDecoder wrapper token postid unrepost
        |> request


{-| Shared by upvotePostParts, downvotePostParts, repostParts

    doPostsParts operation decoder wrapper token username undo

`operation` can be "upvote", "downvote" or "repost".

-}
doPostsParts : String -> Decoder a -> (Result Http.Error a -> msg) -> Token -> String -> Bool -> RequestParts msg
doPostsParts =
    doParts "posts"


{-| Upvote or unupvote a post, with a custom decoder.

    upvotePostParts decoder wrapper token postid unupvote

-}
upvotePostParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> Int -> Bool -> RequestParts msg
upvotePostParts decoder wrapper token postid =
    doPostsParts "upvote" decoder wrapper token <| String.fromInt postid


{-| Downvote or undownvote a post, with a custom decoder.

    downvotePostParts decoder wrapper token postid undownvote

-}
downvotePostParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> Int -> Bool -> RequestParts msg
downvotePostParts decoder wrapper token postid =
    doPostsParts "downvote" decoder wrapper token <| String.fromInt postid


{-| Repost or unrepost, with a custom decoder.

    repostParts decoder wrapper token postid unrepost

-}
repostParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> Int -> Bool -> RequestParts msg
repostParts decoder wrapper token postid =
    doPostsParts "repost" decoder wrapper token <| String.fromInt postid


{-| Posting uses JSON, which is not in the spec, but is what the web client does.
-}
newPost : (Result Http.Error ActivityLog -> msg) -> Token -> PostForm -> Cmd msg
newPost wrapper token postForm =
    newPostParts ED.activityLogDecoder wrapper token postForm
        |> request


{-| Create a new post with a custom decoder.
-}
newPostParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> PostForm -> RequestParts msg
newPostParts decoder wrapper token postForm =
    let
        method =
            "POST"

        path =
            "posts"

        body =
            postFormBody postForm
    in
    requestParts method [] body decoder wrapper token path


postFormBody : PostForm -> HttpBody
postFormBody postForm =
    JsonBody <| ED.postFormEncoder postForm


{-| Posting an image.

The `String` that comes back is a media ID, to be used in `PostForm.media_attachments`.

-}
postImage : (Result Http.Error String -> msg) -> Token -> File -> Cmd msg
postImage wrapper token file =
    postImageParts ED.mediaIdDecoder wrapper token file
        |> request


{-| Post an image with a custom decoder
-}
postImageParts : Decoder a -> (Result Http.Error a -> msg) -> Token -> File -> RequestParts msg
postImageParts decoder wrapper token file =
    let
        method =
            "POST"

        path =
            "media-attachments/images"

        body =
            FileBody file
    in
    requestParts method [] body decoder wrapper token path


{-| Convert an `OAuthMiddleWare.ResponseToken` to `Gab.Types.SavedToken`.

Use `Gab.EncodeDecode.savedTokenEncoder` and `Gab.EncodeDecode.savedTokenDecoder` to persist it.

-}
savedTokenFromResponseToken : Posix -> ResponseToken -> SavedToken
savedTokenFromResponseToken time token =
    { expiresAt =
        case token.expiresIn of
            Just expiresIn ->
                Just <| Time.posixToMillis time + (1000 * expiresIn)

            Nothing ->
                Nothing
    , refreshToken = token.refreshToken
    , scope = token.scope
    , token = token.token
    }
