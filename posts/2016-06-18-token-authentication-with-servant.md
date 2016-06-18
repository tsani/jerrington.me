---
title: Token authentication with Servant
---

Servant 0.5 introduced a `BasicAuth` combinator to support HTTP Basic
authentication. However, Servant is designed to be extensible without needing
to modify the library: the goal of this article is to see how we can implement
a simple token based authentication.

Specifically, we will define a new combinator called `Otoke` and a `HasServer`
instance involving it. Our goal is to be able to write an API type that looks
like this:

```
type API
  = "unprotected" :> Get '[PlainText] String
  :<|> "protected" :> Otoke :> Get '[PlainText] String
```

Routes below the `Otoke` combinator will be protected by the token-based
authentication. Clients will authenticate by using an HTTP `Authorization`
header whose value will have the form `oToke XXX`.

Enough planning. Let's do it!

The combinator
--------------

This is the easy part.

```
data Otoke
```

No constructors are necessary because `Otoke` will only appear at the type
level.

The `HasServer` instance
------------------------

This is the hard part.

Servant uses what's called the universe pattern, in which a type-level EDSL is
used to define a generic representation of types. This generic representation
can be specialized to different concrete types by defining different
type-level interpreters. `HasServer` is the interpreter that computes the
concrete type of a server for our API. What's more is that `HasServer` can also
register certain checks for us that occur during routing.

It's precisely because `HasServer` is a typeclass with an associated type that
we can extend it without needing to modify the `servant-server` library itself.
The type family given by the associated type of each instance is _open_; this
is a huge strength of Servant.

Here's the definition of `HasServer`.

```
class HasServer layout context where
  type ServerT layout (m :: * -> *) :: *

  route
    :: Proxy layout
    -> Context context
    -> Delayed env (Server layout)
    -> Router env
```

The instances of `HasServer` work by induction. The base case is when the
`Verb` primitive is encountered. We don't normally use `Verb` directly, but
instead use convenient type aliases such as `Get` or `Post`.

The step cases occur when we hit `X :> sublayout`, where `X` is a combinator
such as `Header`, `Capture` or in our case `Otoke`, or is a type-level string,
for constant portions of the URL. The instance head in the step cases has the
constraint `HasServer sublayout context`; this is the induction hypothesis.

Thus, the instance declaration for our combinator looks like this:

```
instance HasServer sublayout context
  => HasServer (Otoke :> sublayout) context where
```

The instance body is also defined inductively in these cases. The `ServerT`
associated type is used to compute the type of a handler function for `Otoke :>
sublayout`. For now, we will just check whether the value of the
`Authorization` header is among a list of accepted values. However, in a real
server, we might want to fetch a user context from a database and provide that
context to the handler function. So for the sake of example, we will represent
our user context with `()`.

```
  type ServerT (Otoke :> sublayout) m = () -> ServerT sublayout m
```

Now we can implement `route`. Let's break down its arguments:

  * `Proxy layout`: used to guide type inference.
  * `Context context`: used to pass data around between our routing functions.
    In a real server, we could use this context to pass a pool of database
    connections to our routing function in order to look up the token in a
    database.
  * `Delayed env (Server layout)`: used to register any checks we would like to
    perform on the request and to decide whether to continue routing inside
    the sublayout.

Our implementation of `route` needs to construct a `Router`. The most obvious
way to do this, by blindly following the types, is to use `route` provided to
us by the induction hypothesis. To do so, we need to call `route` with `Proxy
sublayout`. Since the context is unused, we just pass it along unchanged. All
that's left is the third argument, where we need to add our check for the
`Authorization` header.

Luckily for us, there is the function `addAuthCheck :: Delayed env (a -> b) ->
DelayedIO a -> Delayed env b`. Using this function will require that `a -> b`
unify with `() -> ServerT sublayout m`. If we hadn't introduced the dummy user
context representation earlier with `()`, then typechecking would have failed
here.

Here's the implementation of `route`:

```
  route Proxy context subserver =
    route (Proxy :: Proxy sublayout) context (addAuthCheck subserver go) where
      go = withRequest $ \req -> do
        case parseHeaderMaybe =<< lookup "Authorization" (requestHeaders req) of
          Nothing -> delayedFail err401
          Just h -> if h `elem` pws
            then pure ()
            else delayedFail err401

      pws :: [T.Text]
      pws = ("oToke " `T.append`) <$>
        [ "hello"
        , "world"
        ]

      parseHeaderMaybe = eitherMaybe . parseHeader where
        eitherMaybe e = case e of
          Left _ -> Nothing
          Right x -> Just x
```

The `withRequest` helper lets us access the Wai request, from which we extract
the list of headers, look up the `Authorization` header, and parse it. Finally,
we check that the given token is among the list of accepted tokens. If it is,
then we return the dummy user context; else, we fail with `Unauthorized`. We
also fail with `Unauthorized` if there is no `Authorization` header at all.

That concludes the implementation of the `HasServer` instance!

The server
----------

The server is extremely simple to write, since all the hard work is done by
`HasServer` during routing.

```
myServer :: Server API
myServer
  = pure "not secret"
  :<|> const (pure "secret") -- ignore the ()

main :: IO ()
main = run 8081 $ server (Proxy :: Proxy API)
```

Results
-------

Let's confirm that `/unprotected` is accessible without authorization and that
`/protected` works with either of the two tokens we hardcoded.

```
$ curl -v 'http://localhost:8081/unprotected'
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /unprotected HTTP/1.1
> Host: localhost:8081
> User-Agent: curl/7.48.0
> Accept: */*
>
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Sat, 18 Jun 2016 23:08:03 GMT
< Server: Warp/3.2.6
< Content-Type: text/plain;charset=utf-8
<
* Connection #0 to host localhost left intact
not secret
```

```
$ curl -v 'http://localhost:8081/protected'
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /protected HTTP/1.1
> Host: localhost:8081
> User-Agent: curl/7.48.0
> Accept: */*
>
< HTTP/1.1 401 Unauthorized
< Transfer-Encoding: chunked
< Date: Sat, 18 Jun 2016 23:08:15 GMT
< Server: Warp/3.2.6
<
* Connection #0 to host localhost left intact
```

```
$ curl -v -H 'Authorization: oToke hello' 'http://localhost:8081/protected'
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /protected HTTP/1.1
> Host: localhost:8081
> User-Agent: curl/7.48.0
> Accept: */*
> Authorization: oToke hello
>
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Sat, 18 Jun 2016 23:09:35 GMT
< Server: Warp/3.2.6
< Content-Type: text/plain;charset=utf-8
<
* Connection #0 to host localhost left intact
secret
```
```
$ curl -v -H 'Authorization: oToke world' 'http://localhost:8081/protected'
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /protected HTTP/1.1
> Host: localhost:8081
> User-Agent: curl/7.48.0
> Accept: */*
> Authorization: oToke world
>
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Sat, 18 Jun 2016 23:09:41 GMT
< Server: Warp/3.2.6
< Content-Type: text/plain;charset=utf-8
<
* Connection #0 to host localhost left intact
secret
```

Everything seems to be working just as planned!

Conclusion
----------

This was my first experience in adding a combinator to Servant, and I'm sure
there are ways that the technique I used here can be improved. In a future post
I'll extend the work here by using the context to pass in a database connection
pool and use Esqueleto to look up the token in a database.

Full source code is available
[on Github](https://github.com/tsani/servant-otoke/tree/815c0aef69c3a0aac8b0f664a8122c8d2a490182).
