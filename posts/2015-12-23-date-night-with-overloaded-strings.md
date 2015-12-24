---
title: Date night with OverloadedStrings
---

The `OverloadedStrings` extension in Haskell overloads string literals so they
can act as any type that is an instance of the `IsString` type class.  The
`IsString` type class has only one method `fromString :: IsString a => String
-> a`; it's the function that does all the hard work of converting strings into
your _stringy_ type.

The `IsString` type class is available in `Data.String` without any language
extensions, but with `OverloadedStrings` enabled, GHC implicitly inserts a
`fromString` before every string literal in that module. This makes working
with alternative text types like `Text` and `ByteString` very convenient.

I found a pretty nice use for `OverloadedStrings` that isn't exactly for text
types: dates. Using this extension, we can make what's essentially a _date
literal_. The idea is simple: make `LocalTime` from the `time` package an
instance of `IsString` by unsafely extracting the result of a date parser to
get a `fromString` function.

```
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe ( fromMaybe )
import Data.String
import qualified Data.Time as T

instance IsString T.LocalTime where
    fromString = fromMaybe (error "invalid date literal") . parseTime where
        parseTime = T.parseTimeM False T.defaultTimeLocale iso8601
        iso8601 = T.iso8601DateFormat (Just "%Y-%m-%d")

today :: T.LocalTime
today = "2015-12-23"
```

Of course there are some issues with this.

First, you should probably use a `newtype` wrapper around `LocalTime` instead
of defining an orphan instance.  Using `newtype`s also gives you the benefit of
having different date formats for different types, by changing the definition
of `iso8601` for the different wrappers.

Second, if the literal is invalid, then you'll get a _runtime_ exception, and
in Haskell those are Very Bad. With the `IsString` approach given here, the
latter issue can't be resolved without dependent types. There might be enough
dependent type features in Haskell to do it, but I'm not really sure what can
and can't be done in Haskell yet when it comes to dependent types.

An alternative approach could be to use TemplateHaskell and a quasiquoter. I
think this could be made to fail at compile time if the literal is invalid, and
it avoids the gross stringiness of the `IsString` approach.

I guess I should go learn TemplateHaskell.
