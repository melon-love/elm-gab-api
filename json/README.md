This directory contains JSON files related to the Gab API.

[openapi-3.0.json](openapi-3.0.json) is a Json Schema for Open API 3.0. I thought it would be useful, but, though I can parse it with [1602/json-schema](http://package.elm-lang.org/packages/1602/json-schema/latest), there is currently no code to DO anything with that parse, except validate the Gab schema. So I'm left with writing my own JSON decoder for the API.

[Gab.ai_Gab.ai_1.0.0-oas3_swagger.json](Gab.ai_Gab.ai_1.0.0-oas3_swagger.json) is the OpenAPI JSON for the Gab API. It is symbolically linked from the `src` directory is `gab-oas.json`, to make it easy to get to from `elm reactor` instances.
