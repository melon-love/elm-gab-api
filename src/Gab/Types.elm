----------------------------------------------------------------------
--
-- Types.elm
-- Shared type definitions.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Gab.Types exposing (HttpBody(..), RequestParts, User)

import Http
import Json.Encode exposing (Value)
import Time exposing (Time)


{-| A union type for request bodies
-}
type HttpBody
    = EmptyBody
    | JsonBody Value
    | StringBody String String
    | OtherBody Http.Body


{-| A non-opaque representation of an Http.Request
-}
type alias RequestParts a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : HttpBody
    , expect : Http.Expect a
    , timeout : Maybe Time
    , withCredentials : Bool
    }


type alias User =
    { id : Int
    , name : String
    , username : String
    , picture_url : String
    , verified : Bool
    , is_investor : Bool --not really required
    , is_pro : Bool
    , is_private : Bool
    , is_premium : Bool

    -- Optional
    , created_at_month_label : Maybe String
    , follower_count : Maybe Int
    , following_count : Maybe Int
    , post_count : Maybe Int
    , picture_url_full : Maybe String
    , following : Bool
    , followed : Bool
    , is_donor : Bool
    , is_tippable : Bool
    , premium_price : Maybe String
    , is_accessible : Bool
    , follow_pending : Bool
    , unread_notification_count : Maybe Int
    , stream : Bool
    , bio : Maybe String
    , cover_url : Maybe String
    , show_replies : Bool
    , sound_alerts : Bool
    , email : Maybe String
    , notify_followers : Bool
    , notify_mentions : Bool
    , notify_likes : Bool
    , notify_reposts : Bool
    , score : Maybe Int
    , broadcast_channel : Maybe String
    , exclusive_features : Bool
    , social_facebook : Bool
    , social_twitter : Bool
    , is_pro_overdue : Bool
    , pro_expires_at : Maybe String
    , has_chat : Bool
    , has_chat_unread : Bool
    , germany_law : Bool
    , language : Maybe String
    , pinned_post_id : Maybe String
    , nsfw_filter : Bool
    , hide_premium_content : Bool
    , video_count : Maybe Int
    , can_downvote : Bool
    }
