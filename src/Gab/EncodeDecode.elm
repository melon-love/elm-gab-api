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


module Gab.EncodeDecode
    exposing
        ( userDecoder
        , userEncoder
        )

import Gab.Types exposing (User)
import Json.Decode as JD exposing (Decoder, bool, int, maybe, string)
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as JE exposing (Value)


{-| Decode a User
-}
userDecoder : Decoder User
userDecoder =
    DP.decode User
        |> required "id" int
        |> required "name" string
        |> required "username" string
        |> required "picture_url" string
        |> required "verified" bool
        |> optional "is_investor" (maybe bool) Nothing
        |> required "is_pro" bool
        |> required "is_private" bool
        |> required "is_premium" bool
        |> optional "created_at_month_label" (maybe string) Nothing
        |> optional "follower_count" (maybe int) Nothing
        |> optional "following_count" (maybe int) Nothing
        |> optional "post_count" (maybe int) Nothing
        |> optional "picture_url_full" (maybe string) Nothing
        |> optional "following" (maybe bool) Nothing
        |> optional "followed" (maybe bool) Nothing
        |> optional "is_donor" (maybe bool) Nothing
        |> optional "is_tippable" (maybe bool) Nothing
        |> optional "premium_price" (maybe string) Nothing
        |> optional "is_accessible" (maybe bool) Nothing
        |> optional "follow_pending" (maybe bool) Nothing
        |> optional "unread_notification_count" (maybe int) Nothing
        |> optional "stream" (maybe bool) Nothing
        |> optional "bio" (maybe string) Nothing
        |> optional "cover_url" (maybe string) Nothing
        |> optional "show_replies" (maybe bool) Nothing
        |> optional "sound_alerts" (maybe bool) Nothing
        |> optional "email" (maybe string) Nothing
        |> optional "notify_followers" (maybe bool) Nothing
        |> optional "notify_mentions" (maybe bool) Nothing
        |> optional "notify_likes" (maybe bool) Nothing
        |> optional "notify_reposts" (maybe bool) Nothing
        |> optional "score" (maybe int) Nothing
        |> optional "broadcast_channel" (maybe string) Nothing
        |> optional "exclusive_features" (maybe bool) Nothing
        |> optional "social_facebook" (maybe bool) Nothing
        |> optional "social_twitter" (maybe bool) Nothing
        |> optional "is_pro_overdue" (maybe bool) Nothing
        |> optional "pro_expires_at" (maybe string) Nothing
        |> optional "has_chat" (maybe bool) Nothing
        |> optional "has_chat_unread" (maybe bool) Nothing
        |> optional "germany_law" (maybe bool) Nothing
        |> optional "language" (maybe string) Nothing
        |> optional "pinned_post_id" (maybe string) Nothing
        |> optional "nsfw_filter" (maybe bool) Nothing
        |> optional "hide_premium_content" (maybe bool) Nothing
        |> optional "video_count" (maybe int) Nothing
        |> optional "can_downvote" (maybe bool) Nothing


filterNulls : List ( String, Value ) -> List ( String, Value )
filterNulls list =
    List.filter (\( _, v ) -> v /= JE.null) list


maybeInt : Maybe Int -> Value
maybeInt mx =
    case mx of
        Nothing ->
            JE.null

        Just x ->
            JE.int x


maybeString : Maybe String -> Value
maybeString mx =
    case mx of
        Nothing ->
            JE.null

        Just x ->
            JE.string x


maybeBool : Maybe Bool -> Value
maybeBool mx =
    case mx of
        Nothing ->
            JE.null

        Just x ->
            JE.bool x


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
            , ( "is_investor", maybeBool user.is_investor )
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
            , ( "premium_price", maybeString user.premium_price )
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
