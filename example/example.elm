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


module Main exposing (..)

import Dict exposing (Dict)
import Gab
import Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , h2
        , option
        , p
        , pre
        , select
        , text
        )
import Html.Attributes
    exposing
        ( selected
        , style
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Navigation exposing (Location)
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


type alias Model =
    { authorization : Maybe Authorization
    , token : Maybe ResponseToken
    , state : Maybe String
    , msg : Maybe String
    , replyType : String
    , reply : Maybe Value
    , redirectBackUri : String
    , authorization : Maybe Authorization
    , tokenAuthorization : Maybe TokenAuthorization
    }


type Msg
    = ReceiveLocation Location
    | ReceiveAuthorization (Result Http.Error Authorization)
    | Login
    | GetMe
    | ReceiveUser (Result Http.Error Value)


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
    , replyType = "Token"
    , reply =
        case token of
            Nothing ->
                Nothing

            Just tok ->
                Just <| responseTokenEncoder tok
    , redirectBackUri = Debug.log "redirectBackUri" <| locationToRedirectBackUri location
    , authorization = Nothing
    , tokenAuthorization = Nothing
    }
        ! [ Http.send ReceiveAuthorization <|
                getAuthorization False "authorization.json"
          , Navigation.modifyUrl "#"
          ]


getMe : Model -> ( Model, Cmd Msg )
getMe model =
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
                            Gab.me token.token
                    in
                    model ! [ Http.send ReceiveUser req ]

                _ ->
                    { model | msg = Just "No authorization loaded." }
                        ! []


lookupProvider : Model -> Model
lookupProvider model =
    case model.authorization of
        Nothing ->
            model

        Just auth ->
            case List.head <| Dict.toList auth.scopes of
                Nothing ->
                    model

                Just ( _, scope ) ->
                    { model
                        | tokenAuthorization =
                            Just
                                { authorization = auth
                                , scope = [ scope ]
                                , state = Nothing
                                , redirectBackUri = model.redirectBackUri
                                }
                    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveLocation _ ->
            model ! []

        ReceiveAuthorization result ->
            case result of
                Err err ->
                    { model | msg = Just <| toString err }
                        ! []

                Ok authorization ->
                    let
                        ( replyType, reply ) =
                            case ( model.token, model.msg ) of
                                ( Nothing, Nothing ) ->
                                    ( "Authorization"
                                    , Just <|
                                        authorizationEncoder
                                            { authorization
                                                | clientId = "not telling"
                                                , redirectUri = "don't ask"
                                            }
                                    )

                                _ ->
                                    ( model.replyType, model.reply )
                    in
                    lookupProvider
                        { model
                            | authorization = Just authorization
                            , replyType = replyType
                            , reply = reply
                        }
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

        ReceiveUser result ->
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
                        , msg = Nothing
                    }
                        ! []


view : Model -> Html Msg
view model =
    div
        [ style [ ( "margin-left", "3em" ) ]
        ]
        [ h2 [] [ text "OAuthMiddleware Example" ]
        , p []
            [ button [ onClick Login ]
                [ text "Login" ]
            ]
        , if model.token == Nothing then
            text ""
          else
            p []
                [ button [ onClick GetMe ]
                    [ text "Get logged-in user profile" ]
                ]
        , pre []
            [ case ( model.msg, model.reply ) of
                ( Just msg, _ ) ->
                    text <| toString msg

                ( _, Just reply ) ->
                    text <| model.replyType ++ ":\n" ++ JE.encode 2 reply

                _ ->
                    text "Nothing to report"
            ]
        ]
