The server-side code runs in [Node.js](https://nodejs.org/en/) with Asger Nielsen's [`elm-http-server`])https://www.npmjs.com/package/elm-http-server) package to hook together Node and Elm.

The regular node client contacts the authorization server, passing the client ID, `<redirectUri>`, and application-specific `<stateUri>`. That sends the customer to the server web site to authorize the OAuth connection. If she successfully logs in, the authorization server redirects to the `redirectUri` with:

    <redirectUri>?code=<long hex string>&state=<stateUri>
    
The code in this directory is for implemenating the server to handle that request.

It passes the code, the client id, and the client secret to the token server, gets the `<token>` in its response, and redirects the browser back to the client at:

    <stateUri>?authorizationToken=<token>

It will use an ampersand instead of a question mark if there's already a question mark in `<stateUri>`.
