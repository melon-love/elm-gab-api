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
    ( me, meParts, userProfile, userProfileParts
    , userFollowers, userFollowersParts, userFollowing, userFollowingParts
    , followUser, followUserParts, muteUser, muteUserParts
    , homeFeed, homeFeedParts
    , userFeed, userFeedParts
    , groupFeed, groupFeedParts
    , popularFeed, popularFeedParts
    , popularUsers, popularUsersParts
    , getPost, getPostParts
    , upvotePost, upvotePostParts, downvotePost, downvotePostParts
    , repost, repostParts
    , newPost, newPostParts
    , postImage, postImageParts
    , doParts, doUsersParts, doPostsParts
    , gabApiUri, request, getParts, requestParts
    , bodyToString
    )

{-| Client for the Gab.com API, documented at [developers.gab.com](https://developers.gab.com/).

This does NOT do authentication. You'll need to use [billstclair/elm-oauth-middleware](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest) for that. See the `example` directory.

The requests all come in two flavors, one which has the decoder built in, and returns an `Http.Request`, and one for which you provide your own decoder, and returns `RequestParts`. E.g.:

    getTorbaRequest : OAuth.Token -> Http.Request User
    getTorbaRequest token =
        userProfile token "a"

    getTorbaParts : OAuth.Token -> RequestParts Json.Decode.Value
    getTorbaParts token =
        userProfileParts Json.Decode.value token "a"

    getTorbaRequestFromParts : OAuth.Token -> Http.Request Json.Decode.Value
    getTorbaRequestFromParts token =
        getTorbaParts token
            |> request


# Users

@docs me, meParts, userProfile, userProfileParts
@docs userFollowers, userFollowersParts, userFollowing, userFollowingParts


# User modification

@docs followUser, followUserParts, muteUser, muteUserParts


# Feeds

@docs homeFeed, homeFeedParts
@docs userFeed, userFeedParts
@docs groupFeed, groupFeedParts
@docs popularFeed, popularFeedParts
@docs popularUsers, popularUsersParts


# Posts

@docs getPost, getPostParts
@docs upvotePost, upvotePostParts, downvotePost, downvotePostParts
@docs repost, repostParts


# New Posts

@docs newPost, newPostParts
@docs postImage, postImageParts


# Generic requests

@docs doParts, doUsersParts, doPostsParts


# Low-level Http interface

@docs gabApiUri, request, getParts, requestParts


# Debugging

@docs bodyToString

-}

import Char
import CustomElement.FileListener as File exposing (File)
import Gab.EncodeDecode as ED
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , HttpBody(..)
        , Post
        , PostForm
        , RequestParts
        , User
        , UserList
        )
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OAuth exposing (Token)
import Url.Builder as Builder


{-| The base URI for GAB API requests. Automatically prepended to `path` args.
-}
gabApiUri : String
gabApiUri =
    "https://api.gab.com/v1.0/"


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
    , headers = OAuth.useToken token headers
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

        OtherBody bod ->
            bod


{-| Turn parts into a ready-to-send `Http.Request`.
-}
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
    getParts decoder token "me/"


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
                prefix ++ "?before=" ++ String.fromInt before
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

This isn't currently implemented by the API, but I expect that to change.

-}
muteUser : Token -> String -> Bool -> Http.Request Value
muteUser token username unmute =
    muteUserParts JD.value token username unmute
        |> request


{-| Shared by doUsersParts and doPostParts

    doParts prefix operation decoder token identifier undo

`prefix` can be "users" or "posts".

If `undo` is `True`, does a DELETE. Otherwise, does a POST.

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

This isn't currently implemented by the API, but I expect that to change.

-}
muteUserParts : Decoder a -> Token -> String -> Bool -> RequestParts a
muteUserParts =
    doUsersParts "mute"


{-| Shared by all the getters that take before dates.

    beforeAfterParts prefix decoder token before

-}
beforeAfterParts : String -> Decoder a -> Token -> String -> RequestParts a
beforeAfterParts prefix decoder token before =
    let
        queries =
            if before == "" then
                []

            else
                [ Builder.string "before" before ]

        path =
            prefix ++ Builder.toQuery queries
    in
    getParts decoder token path


{-| Return the posts in the "popular" feed, as a ActivityLogList.

    popularFeed token

-}
popularFeed : Token -> Http.Request ActivityLogList
popularFeed token =
    popularFeedParts ED.activityLogListDecoder token
        |> request


{-| Return the posts in the "popular" feed, using a custom decoder.

    popularFeedParts decoder token

-}
popularFeedParts : Decoder a -> Token -> RequestParts a
popularFeedParts decoder token =
    beforeAfterParts "popular/feed" decoder token ""


{-| Return posts in the home feed.

    homeFeed token before

The posts returned will have dates before `before`. Pass the empty string for either to not limit that end.

-}
homeFeed : Token -> String -> Http.Request ActivityLogList
homeFeed token before =
    homeFeedParts ED.activityLogListDecoder token before
        |> request


{-| Return posts in the home feed, using a custom encoder.

    homeFeedParts decoder token before

The posts returned will have dates before `before`. Pass the empty string for either to not limit that end.

-}
homeFeedParts : Decoder a -> Token -> String -> RequestParts a
homeFeedParts decoder token before =
    beforeAfterParts "feed" decoder token before


{-| Return posts for a user feed.

    userFeed token user before

The posts returned will have dates before `before`. Pass the empty string for either to not limit that end.

-}
userFeed : Token -> String -> String -> Http.Request ActivityLogList
userFeed token user before =
    userFeedParts ED.activityLogListDecoder token user before
        |> request


{-| Return posts for a user feed, using a custom decoder.

    userFeedParts decoder user token before

The posts returned will have dates before `before`. Pass the empty string for either to not limit that end.

-}
userFeedParts : Decoder a -> Token -> String -> String -> RequestParts a
userFeedParts decoder token user before =
    beforeAfterParts ("users/" ++ user ++ "/feed") decoder token before


{-| Return posts for a group feed.

    groupFeed token group before

The posts returned will have dates before `before`. Pass the empty string for either to not limit that end.

This is a guess at what this API command will look like. It doesn't yet exist.

-}
groupFeed : Token -> String -> String -> Http.Request ActivityLogList
groupFeed token group before =
    groupFeedParts ED.activityLogListDecoder token group before
        |> request


{-| Return posts for a group feed, using a custom decoder.

    groupFeedParts decoder group token before

The posts returned will have dates before `before`. Pass the empty string for either to not limit that end.

This is a guess at what this API command will look like. It doesn't yet exist.

-}
groupFeedParts : Decoder a -> Token -> String -> String -> RequestParts a
groupFeedParts decoder token group before =
    beforeAfterParts ("groups/" ++ group ++ "/feed") decoder token before


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


{-| Upvote or unupvote a post. Return value not interesting.

    upvotePost token postid unupvote

-}
upvotePost : Token -> String -> Bool -> Http.Request Value
upvotePost token postid unupvote =
    upvotePostParts JD.value token postid unupvote
        |> request


{-| Downvote or undownvote a post. Return value not interesting.

    downvotePost token postid undownvote

-}
downvotePost : Token -> String -> Bool -> Http.Request Value
downvotePost token postid undownvote =
    downvotePostParts JD.value token postid undownvote
        |> request


{-| Repost or unrepost. Return value not interesting.

    repost token postid unrepost

-}
repost : Token -> String -> Bool -> Http.Request Value
repost token postid unrepost =
    repostParts JD.value token postid unrepost
        |> request


{-| Shared by upvotePostParts, downvotePostParts, repostParts

    doPostsParts operation decoder token username undo

`operation` can be "upvote", "downvote" or "repost".

-}
doPostsParts : String -> Decoder a -> Token -> String -> Bool -> RequestParts a
doPostsParts =
    doParts "posts"


{-| Upvote or unupvote a post, with a custom decoder.

    upvotePostParts decoder token postid unupvote

-}
upvotePostParts : Decoder a -> Token -> String -> Bool -> RequestParts a
upvotePostParts =
    doPostsParts "upvote"


{-| Downvote or undownvote a post, with a custom decoder.

    downvotePostParts decoder token postid undownvote

-}
downvotePostParts : Decoder a -> Token -> String -> Bool -> RequestParts a
downvotePostParts =
    doPostsParts "downvote"


{-| Repost or unrepost, with a custom decoder.

    repostParts decoder token postid unrepost

-}
repostParts : Decoder a -> Token -> String -> Bool -> RequestParts a
repostParts =
    doPostsParts "repost"



{-
   I copied this out of the Network tab of a browser debug window.

   Headers:

       Accept: application/json, text/plain, _/_
       content-type: application/json

   Payload:

       {"body": "<p>A test poll. Does Gab rock?</p>",
        "reply\_to": "",
        "is\_quote": "0",
        "is\_html": "1",
        "nsfw": "0",
        "is\_premium": "0",
        "_method": "post",
        "gif": "",
        "topic": null,
        "group": null,
        "share_facebook": null,
        "share\_twitter": null,
        "media\_attachments": [],
        "premium\_min\_tier": 0,
        "poll": "1",
        "poll\_option\_1": "Yes!",
        "poll\_option\_2": "See option 1",
        "poll\_option\_3": "Big time!"
        }
-}


{-| Posting uses JSON, which is not in the spec, but is what the web client does.
-}
newPost : Token -> PostForm -> Http.Request ActivityLog
newPost token postForm =
    newPostParts ED.activityLogDecoder token postForm
        |> request


{-| Create a new post with a custom decoder.
-}
newPostParts : Decoder a -> Token -> PostForm -> RequestParts a
newPostParts decoder token postForm =
    let
        method =
            "POST"

        path =
            "posts"

        body =
            postFormBody postForm
    in
    requestParts method [] body decoder token path


postFormBody : PostForm -> HttpBody
postFormBody postForm =
    JsonBody <| ED.postFormEncoder postForm



{-
      Also copied from the Network tab.

      Request headers:

          Accept: application/json, text/plain, _/_
          Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryzckh5jYHQWN5FetB
          Origin: <https://gab.com>
          Referer: <https://gab.com/notifications>
          User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10\_13\_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.81 Safari/537.36

      Form data:

          ------WebKitFormBoundaryzckh5jYHQWN5FetB
          Content-Disposition: form-data; name="file"; filename="new-gab-logo.jpg"
          Content-Type: image/jpeg

          ------WebKitFormBoundaryzckh5jYHQWN5FetB--

      Response:

          {"id":"285afec2-44f8-4d16-992a-c8522e9e5b8b"}

      Post contents:

          {
            "body": "<p>Test post with attached image.</p>",
            "reply_to": "",
            "is_quote": "0",
            "is_html": "1",
            "nsfw": "0",
            "is_premium": "0",
            "_method": "post",
            "gif": "",
            "topic": null,
            "group": null,
            "share_facebook": null,
            "share_twitter": null,
            "media_attachments": [
              "285afec2-44f8-4d16-992a-c8522e9e5b8b"
            ],
            "premium_min_tier": 0
          }

      Response:

          {"id":"2c02fd52-907e-4c55-bb57-0014056b8aa1",
           "published_at":"2018-10-06T09:38:11+00:00",
           "type":"post",
           "actuser":{"id":335174,
                      "name":"iMacPr0n",
                      "username":"imacpr0n",
                      "picture_url":"https:\/\/f002.backblazeb2.com\/file\/files-gab\/user\/5a2f2b3723a63.jpeg",
                      "verified":false,
                      "is_donor":false,
                      "is_investor":false,
                      "is_pro":false,
                      "is_private":false,
                      "is_premium":false
                     },
           "post":{"id":37647308,
                   "created_at":"2018-10-06T09:38:11+00:00",
                   "revised_at":null,
                   "edited":false,
                   "body":"Test post with attached image.",
                   "body_html":"<p>Test post with attached image.<\/p>",
                   "body_html_summary":"<p>Test post with attached image.<\/p>",
                   "body_html_summary_truncated":false,
                   "only_emoji":false,
                   "liked":false,
                   "disliked":false,
                   "bookmarked":false,
                   "repost":false,
                   "reported":false,
                   "score":0,
                   "like_count":0,
                   "dislike_count":0,
                   "reply_count":0,
                   "repost_count":0,
                   "is_quote":false,
                   "is_reply":false,
                   "is_replies_disabled":false,
                   "embed":{"html":null,"iframe":null},
                   "attachment":{"type":"media",
                                 "value":[{"id":"285afec2-44f8-4d16-992a-c8522e9e5b8b",
                                           "url_thumbnail":"https:\/\/f002.backblazeb2.com\/file\/files-gab\/image\/bb-5bb8815dc80d8.jpeg",
                                           "url_full":"https:\/\/f002.backblazeb2.com\/file\/files-gab\/image\/bb-5bb8815dc84d3.jpeg",
                                           "width":526,
                                           "height":52
                                          }]},
                   "category":null,
                   "category_details":null,
                   "language":"en",
                   "nsfw":false,
                   "is_premium":false,
                   "is_locked":false,
                   "premium_min_tier":0,
                   "current_tier":0,
                   "user":{"id":335174,
                           "name":"iMacPr0n",
                           "username":"imacpr0n",
                           "picture_url":"https:\/\/f002.backblazeb2.com\/file\/files-gab\/user\/5a2f2b3723a63.jpeg",
                           "verified":false,
                           "is_donor":false,
                           "is_investor":false,
                           "is_pro":false,
                           "is_private":false,
                           "is_premium":false
                          },
                   "replies":{"data":[]}
                  }
          }

   An [example](https://stackoverflow.com/questions/4238809/example-of-multipart-form-data/46150309#46150309) raw multipart form post:

       POST / HTTP/1.1
       HOST: host.example.com
       Cookie: some_cookies...
       Connection: Keep-Alive
       Content-Type: multipart/form-data; boundary=12345

       --12345
       Content-Disposition: form-data; name="sometext"

       some text that you wrote in your html form ...
       --12345
       Content-Disposition: form-data; name="name_of_post_request" filename="filename.xyz"

       content of filename.xyz that you upload in your form with input[type=file]
       --12345
       Content-Disposition: form-data; name="image" filename="picture_of_sunset.jpg"

       content of picture_of_sunset.jpg ...
       --12345--
-}


{-| Convenience type.
-}
type alias FileName =
    String


{-| Convenience type.
-}
type alias ContentType =
    String


{-| Convenience type.

It would be nice to have a real 8-bit byte type here. Evan is working on it.

-}
type alias Bytes =
    String


{-| Posting an image.

The `String` that comes back is a media ID, to be used in `PostForm.media_attachments`.

-}
postImage : Token -> File -> Http.Request String
postImage token file =
    postImageParts ED.mediaIdDecoder token file
        |> request


{-| Post an image with a custom decoder
-}
postImageParts : Decoder a -> Token -> File -> RequestParts a
postImageParts decoder token file =
    let
        method =
            "POST"

        path =
            "media-attachments/images"

        body =
            imageBody file
    in
    requestParts method [] body decoder token path


boundary : String
boundary =
    "elm-gab-api-23skidoo"


multipartFormContentType : String
multipartFormContentType =
    File.multipartFormContentType boundary


imageBody : File -> HttpBody
imageBody file =
    File.multipartFormData boundary file
        |> StringBody multipartFormContentType
