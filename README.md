[![elm-package](https://img.shields.io/badge/elm-1.0.0-blue.svg)](http://package.elm-lang.org/packages/billstclair/elm-gab-api/latest)
[![Build Status](https://travis-ci.org/billstclair/elm-gab-api.svg?branch=master)](https://travis-ci.org/billstclair/elm-gab-api)

The links on the two shields above don't work yet. This a work in progress.

`elm-gab-api` is a pure Elm interface to the [Gab.ai](https://gab.ai/) application programming interface.

Gab uses a REST interface, registered at [app.swaggerhub.com/apis/Gab.ai/Gab.ai](https://app.swaggerhub.com/apis/Gab.ai/Gab.ai/). Developer access is currently limited and by invitation only, but if you ask [@a](https://gab.ai/a) or [@e](https://gab.ai/e) nicely, they may let you in.

The JSON defining the Gab API is stored in this repo at [json/Gab.ai_Gab.ai_1.0.0-oas3_swagger.json](json/Gab.ai_Gab.ai_1.0.0-oas3_swagger.json). The specification for the Swagger JSON format is at [swagger.io/specification](https://swagger.io/specification/).

I found a JSON Schema definition for the Open API Specification, version 3 (OAS3) [here](https://github.com/googleapis/gnostic/blob/master/OpenAPIv3/openapi-3.0.json). It's in [json/openapi-3.0.json](json/openapi-3.0.json). Don't see much use for it, though.

[the-sett/elm-swagger](https://github.com/the-sett/elm-swagger) is a work in progress to decode, construct, and encode the Swagger API. It has the data model, but so far no encoders or decoders.

I thought about building a general-purpose Swagger to API converter, with generic functions to access the API, and a code generator to make typed functions, but I decided that GraphQL is slowly replacing REST, and rightly so, thus making that mostly wasted effort. Instead, I'll just make a Gab-specific API in this project. There are already a number of Elm packages for talking to GraphQL APIs.

