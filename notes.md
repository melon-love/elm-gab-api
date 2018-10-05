Notes on debugging the Gab API, using https://gabdecker.com/api

Bugs
====

GET https://api.gab.com/v1.0/me/
  BadStatus: User not found.

POST https://api.gab.com/v1.0/users/imacpr0n/follow
  BadStatus: Sorry, the page you are looking for could not be found. 

Missing Features
================

I see no way to get the premium tiers. Those prices are necessary to make premium posts, so it would be really good to be able to get them. And the text, to display for the user.

There's also no way to get a list of favorited groups, topics, and users. GabDecker will BE that, but it would be nice to be able to seed it with the existing favorites.

There's currently no way to get a group or topic feed. I would like the requests for those to be something like the following:

GET https://api.gab.com/v1.0/groups/62c38c34-0559-4e2e-a382-98b1ba9acf18/feed/?before=
GET https://api.gab.com/v1.0/groups/62c38c34-0559-4e2e-a382-98b1ba9acf18/feed/popular/?before=
GET https://api.gab.com/v1.0/groups/62c38c34-0559-4e2e-a382-98b1ba9acf18/feed/controversial/?before=

GET https://api.gab.com/v1.0/topic/bfb86f72-5613-4ada-bc36-073c9361986d/feed/?before=
GET https://api.gab.com/v1.0/topic/bfb86f72-5613-4ada-bc36-073c9361986d/feed/popular/?before=
GET https://api.gab.com/v1.0/topic/bfb86f72-5613-4ada-bc36-073c9361986d/feed/controversial/?before=

And, of course, it would be nice to have a way to query for topics
