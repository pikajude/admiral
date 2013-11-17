admiral, the dock manager
=========================
(which isn't really what admirals do)
-------------------------------------

`admiral` is a way to manage your docker images.

`admiral` reads your `Admiralfile` and builds, starts, stops, and destroys containers and images.

An `Admiralfile` might look like this:

```
myapp/web
  myapp/db => db
  myapp/redis => redis

myapp/nginx
  myapp/web => web

myapp/load-balancer
  myapp/web => web
  myapp/nginx => nginx
```
