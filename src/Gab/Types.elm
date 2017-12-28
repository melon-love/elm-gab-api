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


module Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , Attachment(..)
        , CategoryDetails
        , Embed
        , HttpBody(..)
        , MediaRecord
        , Post
        , PostList
        , RelatedPosts(..)
        , RequestParts
        , Topic
        , UnknownAttachmentRecord
        , UrlRecord
        , User
        , UserList
        )

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

    -- optional fields, according to the JSON spec.
    , created_at_month_label : Maybe String
    , follower_count : Maybe Int
    , following_count : Maybe Int
    , post_count : Maybe Int
    , picture_url_full : Maybe String
    , following : Bool
    , followed : Bool
    , is_donor : Bool
    , is_tippable : Bool
    , premium_price : Maybe Float
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


type alias UserList =
    { data : List User
    , no_more : Bool
    }


type RelatedPosts
    = RelatedPosts
        { parent : Maybe Post
        , replies : PostList
        }


type alias Post =
    { id : Int
    , created_at : String
    , revised_at : Maybe String
    , edited : Bool
    , body : String
    , only_emoji : Bool
    , liked : Bool
    , disliked : Bool
    , bookmarked : Bool
    , repost : Bool
    , reported : Bool
    , score : Int
    , like_count : Int
    , dislike_count : Int
    , reply_count : Int
    , repost_count : Int
    , is_quote : Bool
    , is_reply : Bool
    , is_replies_disabled : Bool
    , embed : Maybe Embed
    , attachment : Attachment
    , category : Maybe Int
    , category_details : Maybe CategoryDetails
    , language : Maybe String
    , nsfw : Bool
    , is_premium : Bool
    , is_locked : Bool
    , user : User
    , topic : Maybe Topic
    , related : RelatedPosts
    }


type alias UrlRecord =
    { image : String
    , title : Maybe String
    , description : Maybe String
    , url : String
    , source : String
    }


type alias MediaRecord =
    { id : String
    , url_thumbnail : String
    , url_full : String
    , width : Int
    , height : Int
    }


type alias UnknownAttachmentRecord =
    { type_ : String
    , value : Value
    }


type Attachment
    = NoAttachment --type == null
      --type == "url"
    | UrlAttachment UrlRecord
      --type == "media"
    | MediaAttachment (List MediaRecord)
      -- type == "youtube"
    | YoutubeAttachment String
      -- type == "giphy"
    | GiphyAttachment String
      -- There may be types I haven't seen yet.
    | UnknownAttachment UnknownAttachmentRecord


type alias CategoryDetails =
    { title : String
    , slug : String
    , value : Int
    , emoji : String
    }


type alias Embed =
    { html : String
    , iframe : Bool
    }


type alias Topic =
    { id : String
    , created_at : String
    , is_featured : Bool
    , title : String
    , category : Int
    , user : Maybe User
    }


{-| A list of Post.
-}
type alias PostList =
    List Post


type alias ActivityLog =
    { id : String
    , published_at : String
    , type_ : String
    , actuser : User
    , post : Post
    }


type alias ActivityLogList =
    { data : List ActivityLog
    , no_more : Bool
    }
