---
title: Using React with GHCJS – Part 1
published: December 16, 2014
description: Early explorations in data-driven Haskell applications in the browser.
---

I've been doing frontend development for a while now. It has a lot of aspects to it that are very appealing. The browser, especially with today's development tools is one of the most ubiquitous ways out there to introduce people to programming. Instant gratification without any tricky setup makes for a compelling platform. However, anyone who's ever written code for the browser knows that there are plenty of warts– CSS is a nightmare. JavaScript doesn't play nicely with true concurrency, has bizarre scoping rules, is loosely typed, and more.

In any case, I'm not here to rag on JavaScript and CSS. They're slowly getting some cool features,
they're not going away, and the browser is probably not going to be replaced by dramatically superior technology any time soon. Nevertheless, JavaScript is never gonna be as fun or safe as Haskell is. If I want to use Haskell on the client side, we live in an era where it's possible! GHCJS is one such tool for compiling Haskell to JavaScript, and it works great. However, it's currently kinda sparse in terms of frontend-specific libraries that can be used off the shelf.

So, enter [React.js](http://facebook.github.io/react/)! It provides a fast, declarative way of building UI components without having to do too much of the heavy lifting. Why bother wrapping it at all? Why not just build a similar thing in Haskell? Well, there are a lot of perks to using code built by a massive organization like Facebook. I don't have to really worry about cross-browser compatibility. I don't have to think too hard about the Virtual DOM diffing algorithms under the hood. I don't have to write a Chrome plugin to help inspect the rendered Virtual DOM. Facebook and a bajillion other developers out there are taking care of that for me!

### Prior Art

At [CircleCI](https://circleci.com/), where I currently work, we use [Om](https://github.com/swannodette/om) for our frontend. Briefly, Om builds on top of React.js with ClojureScript– it's partially a straightforward wrapper, partially some unique features. David Nolen has already charted a lot of the territory building Om, so props to him for that. He's shown that it can be done, and has put a lot of work into it. I wouldn't have had the idea to build this without him. It's a great development experience in a number of ways! Nevertheless, I want my type safety, dammit! If I can skip looking at sourcemaps and code that's been transmogrified into JavaScript trying to figure out what the heck is going on, I'm gonna do it. With some careful thought, compile-time checking ought to help cut back on a lot of the null reference exceptions and other silly bugs that tend to crop up in JavaScript.

## Now for a code sample!

It's not too bad for a first pass, but I'm still working on the API. This is just a writeup of my very early results. Some of you who saw my tweet about getting React working with GHCJS might notice that this is a bit uglier than my first example, but I'm having to rejigger some code around to make the interface better in the long term, so this is the mess in-between.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Control.Monad.Trans
import Control.Lens hiding (createClass)
import Data.Aeson.Lens
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import GHCJS.Foreign
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NodeList
import GHCJS.DOM.Types hiding (Text)
import React
import React.Attributes
import React.DOM

helloComponent :: ComponentSpecification IO props state
helloComponent = component render & displayName ?~ "Hello"
  where
    render = do
      ps <- currentProps
      nameRef <- liftIO $ getPropMaybe ("name" :: Text) ps
      let name = fromMaybe "World" $ fmap fromJSString nameRef
      return $ div_ noProps [str_ "Hello ", str_ name, str_ "!"]

main = do
  -- Yes, I'm a terrible person for not having total pattern
  -- matching. But if you don't have access to the DOM, this
  -- isn't a fun example anyways.
  (Just doc) <- currentDocument
  (Just b) <- documentGetBody doc
  runReact $ do
    hello <- createClass helloComponent
    renderOn (castToElement b) $ div_ (props id)
      [ elem_ $
          createElement hello
            (props $ prop "name" ?~ (toJSString ("Ian" :: Text)))
      ]
  return ()

```

## GHCJS is a little raw. It's still amazing though.

I can't stress enough how much GHCJS seems to handle already.
I can pretty much `cabal install` anything that doesn't have
underlying native code dependencies and it just works.

Want to use `lens`, or `pipes`, or `conduit`? How about `aeson`? `stm`? It all seems to work.

Also, shout out to luite (Luite Stegeman)– he's the fellow responsible for building a lot of GHCJS in the first place, and he's been really helpful to me in the IRC channel while I've been figuring things out.

Nevertheless, GHCJS has a few downsides. I have no doubt that most of these are probably going to be ameliorated with time, but I want to build incredible things with Haskell in the browser *now*, so I have to figure out workarounds in the meantime:

### No way to convert a `StablePtr` to a `JSRef`
Workaround: `MVar`s can be converted to `JSRef`s. Use them instead for the time being.

### No built-in way to return a value from a Haskell callback to Javascript.
This is partially by design, since it's possible for the GHCJS runtime to become blocked while lazily evaluating something with a data dependency that isn't available yet. As a concrete example, if a lazy `ByteString` thunk isn't forced yet because the data depends on the result of an AJAX call, what do you do when you want to return that result into JavaScript land? You can't block, since JavaScript doesn't have a mechanism to suspend a computation and come back to it. Best case scenario, you can return a result if you actually have one, and return a fallback value or throw a JavaScript exception otherwise.

Here's what that can look like in practice.

In JavaScript:

``` javascript
function reactWrapCallback (cb) {
  return function () {
    // capture proper this in order to access component api
    var that = this;
    // pass obj in and set the result on it inside of the
    // Haskell code.
    var obj = {};
    var result = cb(that, obj);
    // if the result could be fully evaluated without
    // blocking, you get a null back.
    if (result === null) {
      // take the intended result and
      // return it. Make sure it's set
      // on the Haskell side.
      return obj.result;
    }
    else {
      throw "Operation blocked";
    }
  };
}
```

In Haskell:

``` haskell
foreign import javascript unsafe "reactWrapCallback($1)"
  reactWrapCallback :: JSFun (JSRef b -> JSRef a -> IO ())
                    -> IO (JSFun (IO (JSRef a)))

wrapCallback c = do
  cb <- syncCallback2 AlwaysRetain False $ \this x -> do
    (Element res) <- runReaderT (c ^. render) this
    setProp ("result" :: JSString) res x
  w <- reactWrapCallback cb
  return (w, cb)
```

### Converting Haskell functions to `JSFun`s requires determining a [retention strategy](https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Foreign.hs#L106).

You'll note in the above example the use of `syncCallback2` and the `AlwaysRetain` argument. If you don't retain a callback, weak references & `MVars` aren't kept alive in some circumstances. You can read the docs for more info, but it's a hard choice to make– I'm leaning towards using AlwaysRetain right now because it makes the code more general, but cleaning up is tough. I'm currently experimenting with using the `SafeT` monad from `pipes-safe` to register finalizers that are guaranteed to be run once the react code is done running (and can optionally be run sooner), but I'm still not sure whether it's the way to go.

On the one hand, it's cool to have the flexibility, but on the other hand it's kind of hard to know when to release retained functions. In the case of React, I don't have a good answer yet for when and how to release the functions for event handler props (e.g. `onClick`). Since they can be added & removed between in any of the component specification callbacks (`componentWillMount`, etc.), I'm not sure where to clean them up.

I suspect that I'll have to resort to a reference counting mechanism to release them if there don't appear to be any components currently using them. If anyone has thoughts on this, please comment!

### Converting data types to JSRefs is a rather manual process still

In the magical year 20XX, I bet we'll just be able to call `ToJSRef` on anything without exotic types and it'll just work. If your data type is an instance of `ToJSON` & `FromJSON`, you can pretty much already do this. I don't really see there being any long-term issues here, it's just an area that needs an enterprising individual to write more instances and submit PRs to ghcjs-base. That probably means me, since I'm the one griping about it :)

### The generated code starts off pretty big.

This isn't really a huge problem. While the code starts off as around 1 MB unminified, running it through the Google Closure Compiler and serving it gzipped brings it down to a manageable 170 kB or so, React included. Granted, there's more code to be written, so that's not a hard number, but it's not too bad!

## Roadmap

As it currently stands, I'm not trying to implement Om for Haskell just yet. I'm primarily interested in getting a wrapper built that stays true to React's API while also being idiomatic Haskell. I know that there are a ton of smart people in the Haskell community that could build neat abstractions on top of this, so I don't want to close off any of React's API from use.

## In closing

I'm looking forward to blogging some of the challenges that I work through while building this. Hopefully my solutions
will come in handy for other intrepid souls, or you can suggest better ways of doing things.

In part 2, I'll have some integrated examples and talk about more of the design challenges. Stay tuned!

## Addendum: Help a fellow Haskeller out!

Here's the short list of what I could use help with:

* Figuring out how to clean up event handler functions properly (as mentioned previously)
* I really want to make the syntax for the DOM trees identical to that of Chris Done's Lucid templating library. I don't have the ability to just smash a bunch of `Builder`s together though, so advice on how to implement a visually similar Monad would be appreciated. I've explored a few options, but it's made my head hurt a bit so far.

It's a reasonably big undertaking to make this thing pleasant to use, to make sure that I'm not introducing memory leaks, and to keep things as type-safe as possible while doing so. Given enough man-hours, I can do it by myself, but I'd love for some other people to help dog-food it with me and get it stabilized. Go to GitHub, give the code a try, file some issues, and make some pull requests!

Find the code here: <iframe src="http://ghbtns.com/github-btn.html?user=iand675&repo=ghcjs-react&type=fork&count=true" allowtransparency="true" frameborder="0" scrolling="0" width="62" height="20"></iframe>
