This directory provides an example of using the `Gab` module to access the REST API documented at [developers.gab.com](https://developers.gab.com/). It provides pretty good operational documentation of the API, for any programmer, not just Elm programmers using the `Gab` module, including what is sent to which URLs and the returned JSON.

For authentication, it requires an authentication server compatible with [billstclair/elm-oauth-middleware](https://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest/).

You must copy `site/authorization.json.template` into `site/authorization.json`, and change the `apiUri`, `clientId`, and `redirectUri` to match your [Gab application](https://gab.com/settings/clients).

The example will run in `elm reactor`, except for file uploads:

```bash
$ git clone git@github.com:billstclair/elm-gab-api.git
$ cd elm-gab-api/example
$ elm reactor
```

Then aim your web browser at http://fakedomain.com:8000/src/Main.elm, where `fakedomain.com` is a redirect-back domain known to the authentication server, and mapped to `127.0.0.1` in your `/etc/hosts` file.

To use the example with the JavaScript that defines the `file-listener` custom element to support fetching the contents of a file, so that it can be uploaded to Gab:

```bash
$ cd .../elm-gab-api/example
$ elm make src/Main.elm --output site/elm.js
$ elm reactor
```

And aim your web browser at http://fakedomain.com/site/index.html.

Note that the `file-listener` custom element definition doesn't work in Firefox, unless you set `dom.webcomponents.customelements.enabled` true in `about:config`, and it doesn't work at all in IE or Edge.

To go live, upload the `site` directory to a web server whose domain is a redirect-back domain known the to the authentication server.

This example is live at https://gabdecker.com/api
