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


module Gab.Types exposing (User)


type alias User =
    { id : Int
    , name : String
    , username : String
    , picture_url : String
    , verified : Bool
    , is_investor : Maybe Bool --not really required
    , is_pro : Bool
    , is_private : Bool
    , is_premium : Bool

    -- Optional
    , created_at_month_label : Maybe String
    , follower_count : Maybe Int
    , following_count : Maybe Int
    , post_count : Maybe Int
    , picture_url_full : Maybe String
    , following : Maybe Bool
    , followed : Maybe Bool
    , is_donor : Maybe Bool
    , is_tippable : Maybe Bool
    , premium_price : Maybe String
    , is_accessible : Maybe Bool
    , follow_pending : Maybe Bool
    , unread_notification_count : Maybe Int
    , stream : Maybe Bool
    , bio : Maybe String
    , cover_url : Maybe String
    , show_replies : Maybe Bool
    , sound_alerts : Maybe Bool
    , email : Maybe String
    , notify_followers : Maybe Bool
    , notify_mentions : Maybe Bool
    , notify_likes : Maybe Bool
    , notify_reposts : Maybe Bool
    , score : Maybe Int
    , broadcast_channel : Maybe String
    , exclusive_features : Maybe Bool
    , social_facebook : Maybe Bool
    , social_twitter : Maybe Bool
    , is_pro_overdue : Maybe Bool
    , pro_expires_at : Maybe String
    , has_chat : Maybe Bool
    , has_chat_unread : Maybe Bool
    , germany_law : Maybe Bool
    , language : Maybe String
    , pinned_post_id : Maybe String
    , nsfw_filter : Maybe Bool
    , hide_premium_content : Maybe Bool
    , video_count : Maybe Int
    , can_downvote : Maybe Bool
    }
