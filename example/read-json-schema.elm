----------------------------------------------------------------------
--
-- read-json-schema.elm
-- Attempt at reading the OAS Json Schema schema using 1602/json-schema
-- Works, but the result has no use that I can think of.
-- Still have to write a Decoder for the Gab API itself.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Main exposing (..)

import Html
    exposing
        ( Attribute
        , Html
        , br
        , div
        , h2
        , p
        , pre
        , text
        )
import Html.Attributes
    exposing
        ( style
        )
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Json.Schema as JS
import Json.Schema.Definitions exposing (..)
import String.Extra as SE


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { json : Maybe String
    }


type Msg
    = ReceiveJson (Result Http.Error String)


gabOasFile : String
gabOasFile =
    "gab-oas.json"


openapi30File : String
openapi30File =
    "openapi-3.0.json"


init : ( Model, Cmd Msg )
init =
    ( { json = Nothing
      }
    , Http.send ReceiveJson <|
        Http.getString openapi30File
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveJson result ->
            ( { model
                | json =
                    (case result of
                        Err err ->
                            toString err

                        Ok msg ->
                            msg
                    )
                        |> Just
              }
            , Cmd.none
            )


br : Html Msg
br =
    Html.br [] []


b : List (Html Msg) -> Html Msg
b elements =
    Html.b [] elements


btext : String -> Html Msg
btext str =
    b [ text str ]


view : Model -> Html Msg
view model =
    div
        [ style [ ( "margin-left", "3em" ) ]
        ]
        [ h2 [] [ text "Gab OAS Json Decode Test" ]
        , p []
            [ case model.json of
                Nothing ->
                    text "Nothing received yet."

                Just json ->
                    case JS.fromString json of
                        Err msg ->
                            text msg

                        Ok schema ->
                            pre []
                                [ toString schema
                                    |> SE.softBreak 80
                                    |> String.join "\n"
                                    |> text
                                ]
            ]
        ]
