Notes on debugging using https://gabdecker.com/api

GET https://api.gab.com/v1.0/me/
  BadStatus: User not found.

POST https://api.gab.com/v1.0/users/imacpr0n/follow
  BadStatus: Sorry, the page you are looking for could not be found. 

There's currently no way to get a group or topic feed. I assume the requests for those will be something like the following:

GET https://api.gab.com/v1.0/groups/62c38c34-0559-4e2e-a382-98b1ba9acf18/feed/?before=
GET https://api.gab.com/v1.0/groups/62c38c34-0559-4e2e-a382-98b1ba9acf18/feed/popular/?before=
GET https://api.gab.com/v1.0/groups/62c38c34-0559-4e2e-a382-98b1ba9acf18/feed/controversial/?before=

GET https://api.gab.com/v1.0/topic/bfb86f72-5613-4ada-bc36-073c9361986d/feed/?before=
GET https://api.gab.com/v1.0/topic/bfb86f72-5613-4ada-bc36-073c9361986d/feed/popular/?before=
GET https://api.gab.com/v1.0/topic/bfb86f72-5613-4ada-bc36-073c9361986d/feed/controversial/?before=

And, of course, it would be nice to have a way to query for topics
