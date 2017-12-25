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

import Char
import Dict exposing (Dict)
import Gab
import Gab.EncodeDecode as ED
import Gab.Types exposing (RequestParts)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h2
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
    = UserThing Value


type alias Model =
    { authorization : Maybe Authorization
    , token : Maybe ResponseToken
    , state : Maybe String
    , msg : Maybe String
    , request : Maybe (RequestParts JD.Value)
    , replyType : String
    , replyThing : Thing
    , reply : Maybe Value
    , redirectBackUri : String
    , authorization : Maybe Authorization
    , tokenAuthorization : Maybe TokenAuthorization
    , prettify : Bool
    , username : String
    }


type Msg
    = ReceiveLocation Location
    | ReceiveAuthorization (Result Http.Error Authorization)
    | Login
    | GetMe
    | GetUserProfile
    | ReceiveUser (Result Http.Error Value)
    | SetUsername String
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
    , replyType = "Token"
    , replyThing = UserThing JE.null
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
    , username = ""
    }
        ! [ Http.send ReceiveAuthorization <|
                getAuthorization False "authorization.json"
          , Navigation.modifyUrl "#"
          ]


get : Model -> (OAuth.Token -> RequestParts Value) -> ( Model, Cmd Msg )
get model makeRequest =
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
                    { model | request = Just req }
                        ! [ Http.send ReceiveUser <| Gab.request req ]

                _ ->
                    { model | msg = Just "No authorization loaded." }
                        ! []


getMe : Model -> ( Model, Cmd Msg )
getMe model =
    get model <| \token -> Gab.meParts JD.value token


getUserProfile : Model -> String -> ( Model, Cmd Msg )
getUserProfile model username =
    get model <| \token -> Gab.userProfileParts JD.value token username


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
        SetUsername username ->
            { model | username = username } ! []

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
                            , request = Nothing
                            , replyType = replyType
                            , replyThing = UserThing JE.null
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

        GetUserProfile ->
            getUserProfile model model.username

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
                        , replyThing = UserThing reply
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
        UserThing value ->
            doDecodeEncode ED.userDecoder ED.userEncoder value


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
                    [ p [] [ b [ text "User Information" ] ]
                    , table []
                        [ tr []
                            [ td [] [ b [ text "Logged-in User:" ] ]
                            , td []
                                [ button [ onClick GetMe ]
                                    [ text "Get Profile" ]
                                ]
                            ]
                        , tr []
                            [ td []
                                [ b [ text "Username: " ]
                                , input
                                    [ size 20
                                    , onInput SetUsername
                                    , value model.username
                                    ]
                                    []
                                ]
                            , td []
                                [ button [ onClick GetUserProfile ]
                                    [ text "Get Profile" ]
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
                        ]
                    ]
            , case model.request of
                Nothing ->
                    text ""

                Just req ->
                    pre []
                        [ text <|
                            "Request:\n"
                                ++ req.method
                                ++ " "
                                ++ req.url
                                ++ "\n"
                                ++ "\n"
                                ++ Gab.bodyToString 2 req.body
                        ]
            , pre []
                [ case ( model.msg, model.reply ) of
                    ( Just msg, _ ) ->
                        text <|
                            "Error:\n"
                                ++ (SE.softWrap 60 <| toString msg)

                    ( _, Just reply ) ->
                        text <|
                            model.replyType
                                ++ ":\n"
                                ++ encodeWrap model.prettify reply
                                ++ "\n\nDecoded and re-encoded:\n"
                                ++ encodeWrap
                                    model.prettify
                                    (decodeEncode model.replyThing)

                    _ ->
                        text "Nothing to report"
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
