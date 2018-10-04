----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON encoders and decoders
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Gab.EncodeDecode exposing
    ( activityLogDecoder
    , activityLogEncoder
    , activityLogListDecoder
    , activityLogListEncoder
    , postDecoder
    , postEncoder
    , postListDecoder
    , postListEncoder
    , userDecoder
    , userEncoder
    , userListDecoder
    , userListEncoder
    )

import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , Attachment(..)
        , CategoryDetails
        , Embed
        , MediaRecord
        , Post
        , PostList
        , RelatedPosts(..)
        , Topic
        , UnknownAttachmentRecord
        , UrlRecord
        , User
        , UserList
        )
import Json.Decode as JD exposing (Decoder, bool, float, int, maybe, string)
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as JE exposing (Value)


{-| Decode a User
-}
userDecoder : Decoder User
userDecoder =
    JD.succeed User
        |> required "id" int
        |> required "name" string
        |> required "username" string
        |> required "picture_url" string
        |> required "verified" bool
        |> optional "is_investor" bool False
        |> required "is_pro" bool
        |> required "is_private" bool
        |> required "is_premium" bool
        |> optional "created_at_month_label" (maybe string) Nothing
        |> optional "follower_count" (maybe int) Nothing
        |> optional "following_count" (maybe int) Nothing
        |> optional "post_count" (maybe int) Nothing
        |> optional "picture_url_full" (maybe string) Nothing
        |> optional "following" bool False
        |> optional "followed" bool False
        |> optional "is_donor" bool False
        |> optional "is_tippable" bool False
        |> optional "premium_price" (maybe float) Nothing
        |> optional "is_accessible" bool False
        |> optional "follow_pending" bool False
        |> optional "unread_notification_count" (maybe int) Nothing
        |> optional "stream" bool False
        |> optional "bio" (maybe string) Nothing
        |> optional "cover_url" (maybe string) Nothing
        |> optional "show_replies" bool False
        |> optional "sound_alerts" bool False
        |> optional "email" (maybe string) Nothing
        |> optional "notify_followers" bool False
        |> optional "notify_mentions" bool False
        |> optional "notify_likes" bool False
        |> optional "notify_reposts" bool False
        |> optional "score" (maybe int) Nothing
        |> optional "broadcast_channel" (maybe string) Nothing
        |> optional "exclusive_features" bool False
        |> optional "social_facebook" bool False
        |> optional "social_twitter" bool False
        |> optional "is_pro_overdue" bool False
        |> optional "pro_expires_at" (maybe string) Nothing
        |> optional "has_chat" bool False
        |> optional "has_chat_unread" bool False
        |> optional "germany_law" bool False
        |> optional "language" (maybe string) Nothing
        |> optional "pinned_post_id" (maybe string) Nothing
        |> optional "nsfw_filter" bool False
        |> optional "hide_premium_content" bool False
        |> optional "video_count" (maybe int) Nothing
        |> optional "can_downvote" bool False


filterNulls : List ( String, Value ) -> List ( String, Value )
filterNulls list =
    List.filter (\( _, v ) -> v /= JE.null) list


maybeEncoder : (a -> Value) -> Maybe a -> Value
maybeEncoder encoder value =
    case value of
        Nothing ->
            JE.null

        Just a ->
            encoder a


maybeInt : Maybe Int -> Value
maybeInt =
    maybeEncoder JE.int


maybeFloat : Maybe Float -> Value
maybeFloat =
    maybeEncoder JE.float


maybeString : Maybe String -> Value
maybeString =
    maybeEncoder JE.string


maybeBool : Bool -> Value
maybeBool mx =
    if mx then
        JE.bool mx

    else
        JE.null


{-| Encode a User
-}
userEncoder : User -> Value
userEncoder user =
    JE.object <|
        filterNulls
            [ ( "id", JE.int user.id )
            , ( "name", JE.string user.name )
            , ( "username", JE.string user.username )
            , ( "picture_url", JE.string user.picture_url )
            , ( "verified", JE.bool user.verified )
            , ( "is_investor", JE.bool user.is_investor )
            , ( "is_pro", JE.bool user.is_pro )
            , ( "is_private", JE.bool user.is_private )
            , ( "is_premium", JE.bool user.is_premium )
            , ( "created_at_month_label", maybeString user.created_at_month_label )
            , ( "follower_count", maybeInt user.follower_count )
            , ( "following_count", maybeInt user.following_count )
            , ( "post_count", maybeInt user.post_count )
            , ( "picture_url_full", maybeString user.picture_url_full )
            , ( "following", maybeBool user.following )
            , ( "followed", maybeBool user.followed )
            , ( "is_donor", maybeBool user.is_donor )
            , ( "is_tippable", maybeBool user.is_tippable )
            , ( "premium_price", maybeFloat user.premium_price )
            , ( "is_accessible", maybeBool user.is_accessible )
            , ( "follow_pending", maybeBool user.follow_pending )
            , ( "unread_notification_count", maybeInt user.unread_notification_count )
            , ( "stream", maybeBool user.stream )
            , ( "bio", maybeString user.bio )
            , ( "cover_url", maybeString user.cover_url )
            , ( "show_replies", maybeBool user.show_replies )
            , ( "sound_alerts", maybeBool user.sound_alerts )
            , ( "email", maybeString user.email )
            , ( "notify_followers", maybeBool user.notify_followers )
            , ( "notify_mentions", maybeBool user.notify_mentions )
            , ( "notify_likes", maybeBool user.notify_likes )
            , ( "notify_reposts", maybeBool user.notify_reposts )
            , ( "score", maybeInt user.score )
            , ( "broadcast_channel", maybeString user.broadcast_channel )
            , ( "exclusive_features", maybeBool user.exclusive_features )
            , ( "social_facebook", maybeBool user.social_facebook )
            , ( "social_twitter", maybeBool user.social_twitter )
            , ( "is_pro_overdue", maybeBool user.is_pro_overdue )
            , ( "pro_expires_at", maybeString user.pro_expires_at )
            , ( "has_chat", maybeBool user.has_chat )
            , ( "has_chat_unread", maybeBool user.has_chat_unread )
            , ( "germany_law", maybeBool user.germany_law )
            , ( "language", maybeString user.language )
            , ( "pinned_post_id", maybeString user.pinned_post_id )
            , ( "nsfw_filter", maybeBool user.nsfw_filter )
            , ( "hide_premium_content", maybeBool user.hide_premium_content )
            , ( "video_count", maybeInt user.video_count )
            , ( "can_downvote", maybeBool user.can_downvote )
            ]


userListDecoder : Decoder UserList
userListDecoder =
    JD.succeed UserList
        |> required "data" (JD.list userDecoder)
        |> optional "no-more" bool True


userListEncoder : UserList -> Value
userListEncoder userList =
    JE.object
        [ ( "data", JE.list userEncoder userList.data )
        , ( "no-more", JE.bool userList.no_more )
        ]


recursivePostDecoder : Decoder Post
recursivePostDecoder =
    JD.lazy (\_ -> postDecoder)


recursivePostListDecoder : Decoder PostList
recursivePostListDecoder =
    JD.field "data" <| JD.list recursivePostDecoder


makePost : Int -> String -> Maybe String -> Bool -> String -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> Int -> Int -> Int -> Int -> Bool -> Bool -> Bool -> Maybe Embed -> Attachment -> Maybe Int -> Maybe CategoryDetails -> Maybe String -> Bool -> Bool -> Bool -> User -> Maybe Topic -> Maybe Post -> PostList -> Post
makePost id created_at revised_at edited body only_emoji liked disliked bookmarked repost reported score like_count dislike_count reply_count repost_count is_quote is_reply is_replies_disabled embed attachment category category_details language nsfw is_premium is_locked user topic parent replies =
    { id = id
    , created_at = created_at
    , revised_at = revised_at
    , edited = edited
    , body = body
    , only_emoji = only_emoji
    , liked = liked
    , disliked = disliked
    , bookmarked = bookmarked
    , repost = repost
    , reported = reported
    , score = score
    , like_count = like_count
    , dislike_count = dislike_count
    , reply_count = reply_count
    , repost_count = repost_count
    , is_quote = is_quote
    , is_reply = is_reply
    , is_replies_disabled = is_replies_disabled
    , embed = embed
    , attachment = attachment
    , category = category
    , category_details = category_details
    , language = language
    , nsfw = nsfw
    , is_premium = is_premium
    , is_locked = is_locked
    , user = user
    , topic = topic
    , related = RelatedPosts { parent = parent, replies = replies }
    }


postDecoder : Decoder Post
postDecoder =
    JD.succeed makePost
        |> required "id" int
        |> required "created_at" string
        |> optional "revised_at" (JD.nullable string) Nothing
        |> required "edited" bool
        |> required "body" string
        |> required "only_emoji" bool
        |> required "liked" bool
        |> required "disliked" bool
        |> required "bookmarked" bool
        |> required "repost" bool
        |> required "reported" bool
        |> required "score" int
        |> required "like_count" int
        |> required "dislike_count" int
        |> required "reply_count" int
        |> required "repost_count" int
        |> required "is_quote" bool
        |> required "is_reply" bool
        |> required "is_replies_disabled" bool
        |> optional "embed" maybeEmbedDecoder Nothing
        |> required "attachment" attachmentDecoder
        |> required "category" (JD.nullable int)
        |> optional "category_details" (JD.nullable categoryDetailsDecoder) Nothing
        |> optional "language" (JD.nullable string) Nothing
        |> required "nsfw" bool
        |> required "is_premium" bool
        |> required "is_locked" bool
        |> required "user" userDecoder
        |> optional "topic" (JD.nullable topicDecoder) Nothing
        |> optional "parent"
            (JD.nullable (JD.lazy (\_ -> recursivePostDecoder)))
            Nothing
        |> required "replies" (JD.lazy (\_ -> recursivePostListDecoder))


postEncoder : Post -> Value
postEncoder post =
    JE.object <|
        List.concat
            [ [ ( "id", JE.int post.id )
              , ( "created_at", JE.string post.created_at )
              , ( "revised_at", maybeString post.revised_at )
              , ( "edited", JE.bool post.edited )
              , ( "body", JE.string post.body )
              , ( "only_emoji", JE.bool post.only_emoji )
              , ( "liked", JE.bool post.liked )
              , ( "disliked", JE.bool post.disliked )
              , ( "bookmarked", JE.bool post.bookmarked )
              , ( "repost", JE.bool post.repost )
              , ( "reported", JE.bool post.reported )
              , ( "score", JE.int post.score )
              , ( "like_count", JE.int post.like_count )
              , ( "dislike_count", JE.int post.dislike_count )
              , ( "reply_count", JE.int post.reply_count )
              , ( "repost_count", JE.int post.repost_count )
              , ( "is_quote", JE.bool post.is_quote )
              , ( "is_reply", JE.bool post.is_reply )
              , ( "is_replies_disabled", JE.bool post.is_replies_disabled )
              , ( "embed", maybeEmbedEncoder post.embed )
              , ( "attachment", attachmentEncoder post.attachment )
              , ( "category", maybeInt post.category )
              , ( "category_details"
                , maybeEncoder categoryDetailsEncoder post.category_details
                )
              , ( "language", maybeString post.language )
              , ( "nsfw", JE.bool post.nsfw )
              , ( "is_premium", JE.bool post.is_premium )
              , ( "is_locked", JE.bool post.is_locked )
              , ( "user", userEncoder post.user )
              , ( "topic", maybeEncoder topicEncoder post.topic )
              ]
            , relatedPostsFields post.related
            ]


relatedPostsFields : RelatedPosts -> List ( String, Value )
relatedPostsFields related =
    case related of
        RelatedPosts { parent, replies } ->
            List.concat
                [ case parent of
                    Nothing ->
                        []

                    Just post ->
                        [ ( "parent", postEncoder post ) ]
                , [ ( "replies"
                    , JE.object
                        [ ( "data", JE.list postEncoder replies ) ]
                    )
                  ]
                ]


maybeEmbed : Maybe String -> Maybe Bool -> Maybe Embed
maybeEmbed html iframe =
    case ( html, iframe ) of
        ( Just h, Just i ) ->
            Just { html = h, iframe = i }

        _ ->
            Nothing


maybeEmbedDecoder : Decoder (Maybe Embed)
maybeEmbedDecoder =
    JD.map2 maybeEmbed
        (JD.field "html" <| JD.nullable string)
        (JD.field "iframe" <| JD.nullable bool)


embedEncoder : Embed -> Value
embedEncoder embed =
    JE.object
        [ ( "html", JE.string embed.html )
        , ( "iframe", JE.bool embed.iframe )
        ]


maybeEmbedEncoder : Maybe Embed -> Value
maybeEmbedEncoder embed =
    case embed of
        Just e ->
            embedEncoder e

        Nothing ->
            JE.object
                [ ( "html", JE.null )
                , ( "iframe", JE.null )
                ]


type alias RawAttachment =
    { type_ : Maybe String
    , value : Value
    }


urlAttachmentDecoder : Decoder Attachment
urlAttachmentDecoder =
    JD.succeed UrlRecord
        |> required "image" string
        |> required "title" (JD.nullable string)
        |> required "description" (JD.nullable string)
        |> required "url" string
        |> required "source" string
        |> JD.map UrlAttachment


mediaAttachmentDecoder : Decoder Attachment
mediaAttachmentDecoder =
    JD.succeed MediaRecord
        |> required "id" string
        |> required "url_thumbnail" string
        |> required "url_full" string
        |> required "width" int
        |> required "height" int
        |> JD.list
        |> JD.map MediaAttachment


tryDecoder : Decoder a -> Value -> (a -> b) -> Decoder b
tryDecoder decoder value wrapper =
    case JD.decodeValue decoder value of
        Ok a ->
            JD.succeed <| wrapper a

        Err msg ->
            JD.fail <| JD.errorToString msg


rawAttachmentDecoder : RawAttachment -> Decoder Attachment
rawAttachmentDecoder { type_, value } =
    case type_ of
        Nothing ->
            JD.succeed NoAttachment

        Just t ->
            case t of
                "url" ->
                    tryDecoder urlAttachmentDecoder value identity

                "media" ->
                    tryDecoder mediaAttachmentDecoder value identity

                "youtube" ->
                    tryDecoder string value YoutubeAttachment

                "giphy" ->
                    tryDecoder string value GiphyAttachment

                _ ->
                    JD.succeed <|
                        UnknownAttachment
                            { type_ = t
                            , value = value
                            }


attachmentDecoder : Decoder Attachment
attachmentDecoder =
    JD.map2 RawAttachment
        (JD.field "type" <| JD.nullable string)
        (JD.field "value" JD.value)
        |> JD.andThen rawAttachmentDecoder


attachmentEncoder : Attachment -> Value
attachmentEncoder attachment =
    case attachment of
        NoAttachment ->
            JE.object
                [ ( "type", JE.null )
                , ( "value", JE.null )
                ]

        UrlAttachment url ->
            JE.object
                [ ( "type", JE.string "url" )
                , ( "value", urlRecordEncoder url )
                ]

        MediaAttachment media ->
            JE.object
                [ ( "type", JE.string "media" )
                , ( "value"
                  , JE.list mediaRecordEncoder media
                  )
                ]

        YoutubeAttachment value ->
            JE.object
                [ ( "type", JE.string "youtube" )
                , ( "value", JE.string value )
                ]

        GiphyAttachment value ->
            JE.object
                [ ( "type", JE.string "giphy" )
                , ( "value", JE.string value )
                ]

        UnknownAttachment { type_, value } ->
            JE.object
                [ ( "type", JE.string type_ )
                , ( "value", value )
                ]


urlRecordEncoder : UrlRecord -> Value
urlRecordEncoder record =
    JE.object
        [ ( "image", JE.string record.image )
        , ( "title", maybeString record.title )
        , ( "description", maybeString record.description )
        , ( "url", JE.string record.url )
        , ( "source", JE.string record.source )
        ]


mediaRecordEncoder : MediaRecord -> Value
mediaRecordEncoder record =
    JE.object
        [ ( "id", JE.string record.id )
        , ( "url_thumbnail", JE.string record.url_thumbnail )
        , ( "url_full", JE.string record.url_full )
        , ( "width", JE.int record.width )
        , ( "height", JE.int record.height )
        ]


categoryDetailsDecoder : Decoder CategoryDetails
categoryDetailsDecoder =
    JD.succeed CategoryDetails
        |> required "title" string
        |> required "slug" string
        |> required "value" int
        |> required "emoji" string


categoryDetailsEncoder : CategoryDetails -> Value
categoryDetailsEncoder details =
    JE.object
        [ ( "title", JE.string details.title )
        , ( "slug", JE.string details.slug )
        , ( "value", JE.int details.value )
        , ( "emoji", JE.string details.emoji )
        ]


topicDecoder : Decoder Topic
topicDecoder =
    JD.succeed Topic
        |> required "id" string
        |> required "created_at" string
        |> required "is_featured" bool
        |> required "title" string
        |> required "category" int
        |> optional "user" (JD.nullable userDecoder) Nothing


topicEncoder : Topic -> Value
topicEncoder topic =
    JE.object <|
        List.concat
            [ [ ( "id", JE.string topic.id )
              , ( "created_at", JE.string topic.created_at )
              , ( "is_featured", JE.bool topic.is_featured )
              , ( "title", JE.string topic.title )
              , ( "category", JE.int topic.category )
              ]
            , case topic.user of
                Nothing ->
                    []

                Just u ->
                    [ ( "user", userEncoder u ) ]
            ]


postListDecoder : Decoder PostList
postListDecoder =
    JD.list postDecoder


postListEncoder : PostList -> Value
postListEncoder postList =
    JE.list postEncoder postList


activityLogDecoder : Decoder ActivityLog
activityLogDecoder =
    JD.succeed ActivityLog
        |> required "id" string
        |> required "published_at" string
        |> required "type" string
        |> required "actuser" userDecoder
        |> required "post" (JD.lazy (\_ -> recursivePostDecoder))


activityLogEncoder : ActivityLog -> Value
activityLogEncoder log =
    JE.object
        [ ( "id", JE.string log.id )
        , ( "published_at", JE.string log.published_at )
        , ( "type", JE.string log.type_ )
        , ( "actuser", userEncoder log.actuser )
        , ( "post", postEncoder log.post )
        ]


activityLogListDecoder : Decoder ActivityLogList
activityLogListDecoder =
    JD.succeed ActivityLogList
        |> required "data" (JD.list activityLogDecoder)
        |> optional "no-more" JD.bool True


activityLogListEncoder : ActivityLogList -> Value
activityLogListEncoder list =
    JE.object
        [ ( "data", JE.list activityLogEncoder list.data )
        , ( "no-more", JE.bool list.no_more )
        ]
