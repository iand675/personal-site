---
title: Introducing Growler, a simple HTTP server toolkit
published: November 13, 2014
description: Sinatra-esque Haskell web apps.
---

For the past few years, I've always used `scotty` as my tool of choice when building
simple servers for Haskell. However, I've come to take issue with some of the design
choices that `scotty` takes, a few of which include:

* rolling its own error mechanism
* The `next` function, which allows you to switch execution to the next handler that might
  handle a request.
* odd choices for data types that involved unnecessary amounts of coersion
* always loading request bodies into memory
* perhaps a wee bit *too* minimalistic

In light of these issues, I set out to build a toolkit that keeps the best parts of
`scotty` while ameliorating some of the drawbacks that frequently caused me grief and
while adding a few features of my own.

## What's gone

Scotty exports a duplicate API for using it with monad transformers (`ScottyT`) and without (`ScottyM`).
I realize that it's largely for historical reasons that both versions exist, but it always suffices for me
to use the transformer version. If I don't have a special base monad, I just use IO, which is what `ScottyT` really
does anyways.

The `next` function is gone, as I mentioned above. It's a really bizarre concept to me that you'd have control flow
leave off in one request-handling function and start off in a different one. I want all of my logic in one place and
to be responsible for crafting my routes in such a way that they don't overlap. Additionally, the `next` function implements
this control flow transfer by throwing an exception, which is not particularly idiomatic Haskell to me.

## What's changed

The `ActionT` monad transformer is the `HandlerT` monad transformer in Growler terminology. I just think the name is more
sensible.

`param` & `params` don't read form parameters, just pattern matches & query param matches in the URL. The short explanation
is that I prefer leaving it up to the library user to determine whether they want to actually consume the request body or not.

Headers aren't coerced into lazy Text values. They are strict ByteStrings in WAI, so using lazy Texts necessitates conversion on the way in and on the way out, which turned out to be relatively expensive for my workloads.

`capture`, `regex`, and `literal` for constructing routes are all implemented in terms of `function` instead of as intermediate data
structures. This provided some modest performance improvements.

## What's new

Restructuring the routing gave some leeeway for some new tricks: `mount` & `handlerHook`.

`mount` lets you avoid redeclaring the same base sections of routes in a nicely composable way. As an example, where you would have needed to write:

``` haskell
get "/users/:username/settings" userSettingsPage
get "/users/:username/comments" userCommentsPage
get "/users/:username"          userInfoPage
...
```

You can now write:

``` haskell
mount "/users/:username" $ do
  get "/settings" userSettingsPage
  get "/comments" userCommentsPage
  get "/"         userInfoPage
```

There! That's a little DRYer.

The other handy addition is `handlerHook`, which lets you declare middleware for handlers that's
aware of all of the context provided by your monad transformer stack.

One way that I currently use this is with `mount` to enforce authentication for subsections of the site:

``` haskell
mount "/admin" $ do
  ... -- admin-specific routes
  handlerHook enforceUserIsAdmin
```

The nice thing here is that the middleware is only run on requests in the mount block, instead of on
every request across the entire site.

## Example (Untested)
``` haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Growler

main = growl id notFound $ do
  get "/" $ text "Hello, world!"
  mount "/:user" $ do
    get "/" (param "user" >>= text)
    get "/settings" $ text "This would be a settings page"
```

## "I want to use it!"
Great! It's still a little rough around the edges, but it's on [Hackage](http://hackage.haskell.org/package/growler).
In particular, the documentation could use some work, but if you're familiar with Scotty's idioms, they should
more or less translate directly.

## "I want to contribute!"
Please do! You can find the source code on [GitHub](https://github.com/iand675/growler).
Feel free to file any issues that you encounter or to submit pull requests.
