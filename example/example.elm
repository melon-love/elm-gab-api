----------------------------------------------------------------------
--
-- example.elm
-- Example of using the Gab API client.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module GabAPIExample exposing (..)

import Char
import Dict exposing (Dict)
import Gab
import Gab.EncodeDecode as ED
import Gab.Types exposing (Post, RequestParts, User)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h2
        , h3
        , img
        , input
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , tr
        )
import Html.Attributes
    exposing
        ( alt
        , checked
        , colspan
        , disabled
        , height
        , href
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Navigation exposing (Location)
import OAuth exposing (Token(..))
import OAuthMiddleware
    exposing
        ( Authorization
        , ResponseToken
        , TokenAuthorization
        , TokenState(..)
        , authorize
        , getAuthorization
        , locationToRedirectBackUri
        , receiveTokenAndState
        , use
        )
import OAuthMiddleware.EncodeDecode
    exposing
        ( authorizationEncoder
        , responseTokenEncoder
        )
import String
import String.Extra as SE


type Thing
    = ValueThing Value
    | UserThing Value
    | UserListThing Value
    | ActivityLogListThing Value
    | PostThing Value


nullThing : Thing
nullThing =
    ValueThing JE.null


type alias Model =
    { authorization : Maybe Authorization
    , token : Maybe ResponseToken
    , state : Maybe String
    , msg : Maybe String
    , request : Maybe (RequestParts JD.Value)
    , loggedInUser : Maybe String
    , replyType : String
    , replyThing : Thing
    , reply : Maybe Value
    , redirectBackUri : String
    , authorization : Maybe Authorization
    , tokenAuthorization : Maybe TokenAuthorization
    , prettify : Bool
    , username : String
    , userBefore : Int
    , userProfile : Maybe User
    , postUser : String
    , postBefore : String
    , postAfter : String
    , postId : String
    , post : Maybe Post
    }


type Msg
    = ReceiveLocation Location
    | ReceiveAuthorization (Result Http.Error Authorization)
    | ReceiveLoggedInUser (Result Http.Error User)
    | Login
    | GetMe
    | GetUserProfile
    | GetUserFollowers
    | GetUserFollowing
    | GetPopularUsers
    | GetHomeFeed
    | GetUserFeed
    | GetPopularFeed
    | DoOperation String String String Bool
    | ReceiveUser Bool (Result Http.Error Value)
    | ReceiveUserList (Result Http.Error Value)
    | ReceiveActivityLogList (Result Http.Error Value)
    | ReceiveValue (Result Http.Error Value)
    | ReceivePost (Result Http.Error Value)
    | SetUsername String
    | SetUserBefore String
    | SetPostBefore String
    | SetPostAfter String
    | SetPostUser String
    | SetPostId String
    | GetPost
    | TogglePrettify


{-| GitHub requires the "User-Agent" header.
-}
userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "Xossbow"


type alias Api =
    { getUser : String
    }


apis : Dict String Api
apis =
    Dict.fromList
        [ ( "GitHub"
          , { getUser = "user"
            }
          )
        , ( "Gmail"
          , { getUser = "me/profile"
            }
          )
        , ( "Facebook"
          , { getUser = "me"
            }
          )
        , ( "Gab"
          , { getUser = "me" -- "users/{username}"
            }
          )
        ]


main =
    Navigation.program
        ReceiveLocation
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( token, state, msg ) =
            case receiveTokenAndState location of
                TokenAndState tok stat ->
                    ( Just tok, stat, Nothing )

                TokenErrorAndState m stat ->
                    ( Nothing, stat, Just m )

                TokenDecodeError m ->
                    ( Nothing, Nothing, Just m )

                NoToken ->
                    ( Nothing, Nothing, Nothing )
    in
    { token = token
    , state = state
    , msg = msg
    , request = Nothing
    , loggedInUser = Nothing
    , replyType = "Token"
    , replyThing =
        ValueThing JE.null
    , reply =
        case token of
            Nothing ->
                Nothing

            Just tok ->
                Just <| responseTokenEncoder tok
    , redirectBackUri = Debug.log "redirectBackUri" <| locationToRedirectBackUri location
    , authorization = Nothing
    , tokenAuthorization = Nothing
    , prettify = True
    , username = "xossbow"
    , userBefore = 0
    , userProfile = Nothing
    , postUser = "xossbow"
    , postBefore = ""
    , postAfter = ""
    , postId = ""
    , post = Nothing
    }
        ! [ Http.send ReceiveAuthorization <|
                getAuthorization False "authorization.json"
          , Navigation.modifyUrl "#"
          ]


get : Model -> (Result Http.Error Value -> Msg) -> (OAuth.Token -> RequestParts Value) -> ( Model, Cmd Msg )
get model receiver makeRequest =
    case model.token of
        Nothing ->
            { model
                | msg = Just "You must login before getting logged-in user information."
            }
                ! []

        Just token ->
            case model.authorization of
                Just auth ->
                    let
                        req =
                            makeRequest token.token
                    in
                    { model
                        | request = Just req
                        , replyThing = nullThing
                        , reply = Nothing
                    }
                        ! [ Http.send receiver <| Gab.request req ]

                _ ->
                    { model | msg = Just "No authorization loaded." }
                        ! []


getMe : Model -> ( Model, Cmd Msg )
getMe model =
    get model (ReceiveUser False) <|
        \token -> Gab.meParts JD.value token


getUserProfile : Model -> String -> ( Model, Cmd Msg )
getUserProfile model username =
    get model (ReceiveUser True) <|
        \token -> Gab.userProfileParts JD.value token username


getUserFollowers : Model -> String -> Int -> ( Model, Cmd Msg )
getUserFollowers model username before =
    get model ReceiveUserList <|
        \token -> Gab.userFollowersParts JD.value token username before


getUserFollowing : Model -> String -> Int -> ( Model, Cmd Msg )
getUserFollowing model username before =
    get model ReceiveUserList <|
        \token -> Gab.userFollowingParts JD.value token username before


getPopularUsers : Model -> ( Model, Cmd Msg )
getPopularUsers model =
    get model ReceiveUserList <|
        \token -> Gab.popularUsersParts JD.value token


getHomeFeed : Model -> String -> String -> ( Model, Cmd Msg )
getHomeFeed model before after =
    get model ReceiveActivityLogList <|
        \token -> Gab.homeFeedParts JD.value token before after


getUserFeed : Model -> String -> String -> String -> ( Model, Cmd Msg )
getUserFeed model user before after =
    get model ReceiveActivityLogList <|
        \token -> Gab.userFeedParts JD.value token user before after


getPopularFeed : Model -> ( Model, Cmd Msg )
getPopularFeed model =
    get model ReceiveActivityLogList <|
        \token -> Gab.popularFeedParts JD.value token "" ""


doOperation : String -> String -> String -> Bool -> Model -> ( Model, Cmd Msg )
doOperation prefix operation identifier undo model =
    let
        mod =
            case prefix of
                "users" ->
                    { model | userProfile = Nothing }

                "posts" ->
                    { model | post = Nothing }

                _ ->
                    model
    in
    get mod ReceiveValue <|
        \token -> Gab.doParts prefix operation JD.value token identifier undo


getPost : Model -> ( Model, Cmd Msg )
getPost model =
    get model ReceivePost <|
        \token -> Gab.getPostParts JD.value token model.postId


{-| Should add checkboxes to select scopes.
-}
lookupProvider : Model -> Model
lookupProvider model =
    case model.authorization of
        Nothing ->
            model

        Just auth ->
            { model
                | tokenAuthorization =
                    Just
                        { authorization = auth
                        , scope = List.map Tuple.second <| Dict.toList auth.scopes
                        , state = Nothing
                        , redirectBackUri = model.redirectBackUri
                        }
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername username ->
            { model
                | username = username
                , userProfile = Nothing
            }
                ! []

        SetUserBefore before ->
            case String.toInt before of
                Err _ ->
                    if before == "" then
                        { model | userBefore = 0 } ! []
                    else
                        model ! []

                Ok a ->
                    { model | userBefore = a } ! []

        SetPostUser postUser ->
            { model | postUser = postUser } ! []

        SetPostBefore before ->
            { model | postBefore = before } ! []

        SetPostAfter after ->
            { model | postAfter = after } ! []

        SetPostId id ->
            { model
                | postId = id
                , post = Nothing
            }
                ! []

        TogglePrettify ->
            { model | prettify = not model.prettify } ! []

        ReceiveLocation _ ->
            model ! []

        ReceiveAuthorization result ->
            case result of
                Err err ->
                    { model | msg = Just <| toString err }
                        ! []

                Ok authorization ->
                    let
                        ( replyType, reply, replyThing ) =
                            case ( model.token, model.msg ) of
                                ( Nothing, Nothing ) ->
                                    ( "Authorization"
                                    , Just <|
                                        authorizationEncoder
                                            { authorization
                                                | clientId = "not telling"
                                                , redirectUri = "don't ask"
                                            }
                                    , ValueThing JE.null
                                    )

                                _ ->
                                    ( model.replyType
                                    , model.reply
                                    , model.replyThing
                                    )
                    in
                    lookupProvider
                        { model
                            | authorization = Just authorization
                            , request = Nothing
                            , replyType = replyType
                            , replyThing = replyThing
                            , reply = reply
                        }
                        ! (case model.token of
                            Nothing ->
                                []

                            Just token ->
                                [ Http.send ReceiveLoggedInUser <|
                                    Gab.me token.token
                                ]
                          )

        ReceiveLoggedInUser result ->
            case result of
                Err _ ->
                    { model | msg = Just "Error getting logged-in user name." }
                        ! []

                Ok user ->
                    { model | loggedInUser = Just user.username }
                        ! []

        Login ->
            case model.tokenAuthorization of
                Nothing ->
                    { model | msg = Just "No provider selected." }
                        ! []

                Just authorization ->
                    model
                        ! [ authorize authorization ]

        GetMe ->
            getMe model

        GetUserProfile ->
            getUserProfile model model.username

        GetUserFollowers ->
            getUserFollowers model model.username model.userBefore

        GetUserFollowing ->
            getUserFollowing model model.username model.userBefore

        GetPopularUsers ->
            getPopularUsers model

        GetHomeFeed ->
            getHomeFeed model model.postBefore model.postAfter

        GetUserFeed ->
            getUserFeed model model.postUser model.postBefore model.postAfter

        GetPopularFeed ->
            getPopularFeed model

        DoOperation prefix operation identifier undo ->
            doOperation prefix operation identifier undo model

        GetPost ->
            getPost model

        ReceiveUser save result ->
            receiveUserThing save result model

        ReceiveUserList result ->
            receiveThing UserListThing result model

        ReceiveActivityLogList result ->
            receiveThing ActivityLogListThing result model

        ReceiveValue result ->
            receiveThing ValueThing result model

        ReceivePost result ->
            receivePost result model


receiveUserThing : Bool -> Result Http.Error Value -> Model -> ( Model, Cmd Msg )
receiveUserThing save result model =
    let
        mod =
            if not save then
                model
            else
                case result of
                    Err _ ->
                        model

                    Ok value ->
                        case JD.decodeValue ED.userDecoder value of
                            Err _ ->
                                model

                            Ok user ->
                                { model | userProfile = Just user }
    in
    receiveThing UserThing result mod


receivePost : Result Http.Error Value -> Model -> ( Model, Cmd Msg )
receivePost result model =
    let
        mod =
            case result of
                Err _ ->
                    model

                Ok value ->
                    case JD.decodeValue ED.postDecoder value of
                        Err _ ->
                            model

                        Ok post ->
                            { model | post = Just post }
    in
    receiveThing PostThing result mod


receiveThing : (Value -> Thing) -> Result Http.Error Value -> Model -> ( Model, Cmd Msg )
receiveThing thingTag result model =
    case result of
        Err err ->
            { model
                | reply = Nothing
                , msg = Just <| toString err
            }
                ! []

        Ok reply ->
            { model
                | replyType = "API Response"
                , reply = Just reply
                , replyThing = thingTag reply
                , msg = Nothing
            }
                ! []


doDecodeEncode : Decoder a -> (a -> Value) -> Value -> Value
doDecodeEncode decoder encoder value =
    case JD.decodeValue decoder value of
        Err msg ->
            JE.string msg

        Ok thing ->
            encoder thing


decodeEncode : Thing -> Value
decodeEncode thing =
    case thing of
        ValueThing value ->
            value

        UserThing value ->
            doDecodeEncode ED.userDecoder ED.userEncoder value

        UserListThing value ->
            doDecodeEncode ED.userListDecoder ED.userListEncoder value

        ActivityLogListThing value ->
            doDecodeEncode ED.activityLogListDecoder ED.activityLogListEncoder value

        PostThing value ->
            doDecodeEncode ED.postDecoder ED.postEncoder value


link : String -> String -> Html Msg
link txt url =
    a
        [ href url
        , target "_blank"
        ]
        [ text txt ]


maybeLink : String -> Maybe String -> Html Msg
maybeLink txt url =
    case url of
        Nothing ->
            text txt

        Just u ->
            link txt u


userUrl : Maybe String -> Maybe String
userUrl user =
    case user of
        Nothing ->
            Nothing

        Just u ->
            Just <| "https://gab.ai/" ++ u


loggedInUserUrl : Model -> Maybe String
loggedInUserUrl model =
    userUrl model.loggedInUser


usernameUrl : Model -> Maybe String
usernameUrl model =
    userUrl <|
        if model.username == "" then
            Nothing
        else
            Just model.username


postUserUrl : Model -> Maybe String
postUserUrl model =
    userUrl <|
        if model.postUser == "" then
            Nothing
        else
            Just model.postUser


view : Model -> Html Msg
view model =
    div
        [ style [ ( "margin-left", "3em" ) ]
        ]
        [ div []
            [ h2 [] [ text "Gab API Example" ]
            , p []
                [ button [ onClick Login ]
                    [ text "Login" ]
                ]
            , if model.token == Nothing then
                text ""
              else
                p []
                    [ h3 [] [ text "User Information" ]
                    , table []
                        [ tr []
                            [ td []
                                [ b
                                    [ maybeLink "Logged-in User:" <| loggedInUserUrl model
                                    ]
                                ]
                            , td []
                                [ button [ onClick GetMe ]
                                    [ text "Get Profile" ]
                                ]
                            ]
                        , tr []
                            [ td []
                                [ b
                                    [ maybeLink "Username:" <| usernameUrl model
                                    , text " "
                                    ]
                                , input
                                    [ size 20
                                    , onInput SetUsername
                                    , value model.username
                                    ]
                                    []
                                ]
                            , td []
                                [ button
                                    [ onClick GetUserProfile
                                    , disabled <| model.username == ""
                                    ]
                                    [ text "Get Profile" ]
                                ]
                            ]
                        , tr []
                            [ td []
                                [ b [ text <| nbsp ++ nbsp ++ "before: " ]
                                , input
                                    [ size 3
                                    , onInput SetUserBefore
                                    , value <| toString model.userBefore
                                    ]
                                    []
                                ]
                            , td []
                                [ text <| nbsp ++ nbsp
                                , button
                                    [ onClick GetUserFollowers
                                    , disabled <| model.username == ""
                                    ]
                                    [ text "Followers" ]
                                , text " "
                                , button
                                    [ onClick GetUserFollowing
                                    , disabled <| model.username == ""
                                    ]
                                    [ text "Following" ]
                                ]
                            ]
                        , tr [] <|
                            let
                                ( isDisabled, theTitle, unfollow, followText, unmute, muteText ) =
                                    case model.userProfile of
                                        Nothing ->
                                            ( True
                                            , "Click \"Get Profile\" to enable."
                                            , True
                                            , "(Un)Follow"
                                            , True
                                            , "(Un)Mute"
                                            )

                                        Just user ->
                                            let
                                                following =
                                                    user.following || user.follow_pending
                                            in
                                            ( False
                                            , ""
                                            , following
                                            , if following then
                                                "Unfollow"
                                              else
                                                "Follow"
                                            , True
                                            , "Mute"
                                            )
                            in
                            [ td [ colspan 2 ]
                                [ text <| nbsp ++ nbsp
                                , button
                                    [ disabled isDisabled
                                    , title theTitle
                                    , onClick <|
                                        DoOperation "users" "follow" model.username unfollow
                                    ]
                                    [ text followText ]
                                , text " "
                                , button
                                    [ disabled isDisabled
                                    , title theTitle
                                    , onClick <|
                                        DoOperation "users" "mute" model.username False
                                    ]
                                    [ text muteText ]
                                , if isDisabled then
                                    text ""
                                  else
                                    span []
                                        [ text " "
                                        , button
                                            [ onClick <|
                                                DoOperation "users" "mute" model.username True
                                            ]
                                            [ text "Unmute" ]
                                        ]
                                ]
                            ]
                        , tr []
                            [ td []
                                [ b [ text "Popular Users:" ] ]
                            , td []
                                [ button [ onClick GetPopularUsers ]
                                    [ text "Get List" ]
                                ]
                            ]
                        ]
                    , p []
                        [ h3 [] [ text "Posts" ]
                        , table []
                            [ tr []
                                [ td []
                                    [ b [ link "Popular Feed:" "https://gab.ai/popular" ] ]
                                , td []
                                    [ button [ onClick GetPopularFeed ]
                                        [ text "Get Posts" ]
                                    ]
                                ]
                            , tr []
                                [ td [ colspan 2 ]
                                    [ b [ text "Before: " ]
                                    , input
                                        [ size 40
                                        , onInput SetPostBefore
                                        , value model.postBefore
                                        ]
                                        []
                                    ]
                                ]
                            , tr []
                                [ td [ colspan 2 ]
                                    [ b [ text " After: " ]
                                    , input
                                        [ size 40
                                        , onInput SetPostAfter
                                        , value model.postAfter
                                        ]
                                        []
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ b
                                        [ text <| nbsp ++ nbsp
                                        , link "Logged-in User:" "https://gab.ai/"
                                        ]
                                    ]
                                , td []
                                    [ button [ onClick GetHomeFeed ]
                                        [ text "Home Feed"
                                        ]
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ b
                                        [ text <| nbsp ++ nbsp
                                        , maybeLink "Username:" <| postUserUrl model
                                        , text " "
                                        ]
                                    , input
                                        [ size 20
                                        , onInput SetPostUser
                                        , value model.postUser
                                        ]
                                        []
                                    ]
                                , td []
                                    [ button
                                        [ onClick GetUserFeed
                                        , disabled <| model.postUser == ""
                                        ]
                                        [ text "User Feed" ]
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ b
                                        [ text "Post Id:"
                                        , text " "
                                        ]
                                    , input
                                        [ size 9
                                        , onInput SetPostId
                                        , value model.postId
                                        ]
                                        []
                                    ]
                                , td []
                                    [ button
                                        [ disabled <| model.postId == ""
                                        , onClick GetPost
                                        ]
                                        [ text "Get" ]
                                    ]
                                ]
                            , tr [] <|
                                let
                                    ( isDisabled, theTitle, unlike, likeText, undislike, dislikeText, unrepost, repostText ) =
                                        case model.post of
                                            Nothing ->
                                                ( True
                                                , "Click \"Get\" to enable."
                                                , True
                                                , "(Un)Like"
                                                , True
                                                , "(Un)Dislike"
                                                , True
                                                , "(Un)Repost"
                                                )

                                            Just post ->
                                                let
                                                    liked =
                                                        post.liked

                                                    disliked =
                                                        post.disliked

                                                    reposted =
                                                        post.repost
                                                in
                                                ( False
                                                , ""
                                                , liked
                                                , if liked then
                                                    "Unlike"
                                                  else
                                                    "Like"
                                                , disliked
                                                , if disliked then
                                                    "Undislike"
                                                  else
                                                    "Dislike"
                                                , reposted
                                                , if reposted then
                                                    "Unrepost"
                                                  else
                                                    "Repost"
                                                )
                                in
                                [ td [ colspan 2 ]
                                    [ text <| String.repeat 4 nbsp
                                    , button
                                        [ disabled isDisabled
                                        , title theTitle
                                        , onClick <|
                                            DoOperation "posts" "like" model.postUser unlike
                                        ]
                                        [ text likeText ]
                                    , text " "
                                    , button
                                        [ disabled isDisabled
                                        , title theTitle
                                        , onClick <|
                                            DoOperation "posts" "dislike" model.postUser undislike
                                        ]
                                        [ text dislikeText ]
                                    , text " "
                                    , button
                                        [ disabled isDisabled
                                        , title theTitle
                                        , onClick <|
                                            DoOperation "posts" "repost" model.postUser unrepost
                                        ]
                                        [ text repostText ]
                                    ]
                                ]
                            ]
                        ]
                    , p []
                        [ input
                            [ type_ "checkbox"
                            , onClick TogglePrettify
                            , checked model.prettify
                            ]
                            []
                        , b [ text " Prettify" ]
                        , text " (easier to read, may no longer be valid JSON)"
                        ]
                    ]
            , case model.request of
                Nothing ->
                    text ""

                Just req ->
                    span []
                        [ b [ text "Request:" ]
                        , br
                        , pre []
                            [ text <|
                                req.method
                                    ++ " "
                                    ++ req.url
                                    ++ "\n"
                                    ++ "\n"
                                    ++ Gab.bodyToString 2 req.body
                            ]
                        ]
            , case ( model.msg, model.reply ) of
                ( Just msg, _ ) ->
                    span []
                        [ b [ text "Error:" ]
                        , br
                        , pre [] [ text <| SE.softWrap 60 <| toString msg ]
                        ]

                ( _, Just reply ) ->
                    span []
                        [ if model.replyThing == nullThing then
                            text ""
                          else
                            span []
                                [ b [ text "Decoded and re-encoded:" ]
                                , pre []
                                    [ text <|
                                        encodeWrap
                                            model.prettify
                                            (decodeEncode model.replyThing)
                                    , br
                                    ]
                                ]
                        , b [ text <| model.replyType ++ ":" ]
                        , pre []
                            [ text <|
                                encodeWrap model.prettify reply
                            ]
                        ]

                _ ->
                    p []
                        [ b [ text "Waiting for request..." ]
                        ]
            ]
        , footerDiv model
        ]


convertJsonNewlines : String -> String
convertJsonNewlines json =
    SE.replace "\\r" "" json
        |> SE.replace "\\n" "\n"


wrapJsonLine : Int -> String -> List String
wrapJsonLine width line =
    let
        body =
            String.trimLeft line

        indentN =
            String.length line - String.length body + 2

        initialIndent =
            String.repeat (indentN - 2) " "

        indent =
            String.repeat indentN " "

        wrapped =
            convertJsonNewlines body
                |> String.split "\n"
                |> List.map (SE.softWrap <| max 20 (width - indentN))
                |> String.join "\n"

        lines =
            String.split "\n" wrapped
    in
    case lines of
        [] ->
            []

        first :: rest ->
            (initialIndent ++ first)
                :: List.map ((++) indent) rest


wrapJsonLines : Int -> String -> String
wrapJsonLines width string =
    String.split "\n" string
        |> List.concatMap (wrapJsonLine width)
        |> String.join "\n"


encodeWrap : Bool -> Value -> String
encodeWrap prettify value =
    JE.encode 2 value
        |> (if prettify then
                wrapJsonLines 80
            else
                identity
           )


b : List (Html a) -> Html a
b body =
    Html.b [] body


br : Html a
br =
    Html.br [] []


sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
    img
        [ src url
        , title name
        , alt name
        , width size
        , height size
        ]
        []


logoLink : String -> String -> String -> Int -> Html Msg
logoLink url img name size =
    a [ href url ]
        [ sqrimg ("images/" ++ img) name size ]


mailLink : String -> Html Msg
mailLink email =
    span []
        [ text "<"
        , a [ href ("mailto:" ++ email) ]
            [ text email ]
        , text ">"
        ]


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


nbsp : String
nbsp =
    -- \u00A0
    stringFromCode 160


copyright : String
copyright =
    -- \u00A9
    stringFromCode 169


checkmark : String
checkmark =
    -- \u2714
    stringFromCode 10004


space : Html Msg
space =
    text " "


footerDiv : Model -> Html Msg
footerDiv model =
    div []
        [ text (copyright ++ " 2017 ")
        , a [ href "https://lisplog.org/" ]
            [ text "Bill St. Clair" ]
        , space
        , mailLink "billstclair@gmail.com"
        , br
        , logoLink "https://github.com/billstclair/elm-gab-api"
            "GitHub-Mark-32px.png"
            "GitHub source code"
            32
        , space
        , logoLink "http://elm-lang.org/"
            "elm-logo-125x125.png"
            "Elm inside"
            28
        ]
