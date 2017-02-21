---
title: "Haskell and Pushbullet - a tale of SMS on the command line"
---

This post is divided in two. In the first part, I give the story of the bulk of
my Haskell development in the last 8 months, culminating in some Pushbullet API
bindings and client applications. In the second part, I show how you can
install these client applications and enjoy writing SMS on the command line.

The story
-----

I first discovered Pushbullet a few years ago when a friend recommended it to
me. At the time, it didn't yet support synchronizing SMS, so my uses for it
were limited. I rediscovered Pushbullet over the summer, after that feature
launched; being able to send SMS using their webapp was wonderful. No more
typing out texts with my thumbs on a virtual keyboard!

Around the same time, I started messing around with Servant in more depth; I
published my
[post](https://jerrington.me/2016-06-18-token-authentication-with-servant.html)
on token authentication. The current generalized authentication combinators in
Servant are implemented using a technique similar to the one I developed.

In the fall, I returned to school. Since my first year in university, I've kept
all my school work in a repository hosted
[on Github](https://github.com/tsani/school). Since I write up all my homework
in LaTeX, the files are all plain text and storing them in Git repository makes
sense. When comes the time to print the assignments, I prefer to do this at
school, because I can never be quite sure when the ink cartridges at home run
out. Using the school printers usually costs a few cents per page. However,
students in the faculty of science, like me, have a free printing allowance of
a few hundred pages per semester, provided that printing is done on very
specific computers in the ~~dungeon~~ basement of the math building. This means
I need to transfer the compiled PDFs of my assignments from my computer to
those computers. Of course, this could be done with a USB flash drive, but in
my experience, these are easy to misplace. (Read: I have lost countless flash
drives.)

Two summers ago, I founded a company with some buddies.
We got accepted to a
[startup program](http://mcgillx1accelerator.com/) offered by our school,
and our company got approved for
[AWS Activate](https://aws.amazon.com/activate/).
The company didn't pan out in the end, and we stopped working on it come the
fall when classes resumed. However, I found myself in possession of a few
thousand dollars of AWS credits with nothing really to spend it on. I've been
coasting on those credits since then to run a number of machines on EC2 and
host various servers for myself and my buddies. Among them is just a simple web
server where I can dump files.

Hence rather than use easy-to-lose USB flash drives to copy my compiled
assignments from my computer to the school computers, I just transfer the
relevant PDFs to print to my web server using
[rsync](https://en.wikipedia.org/wiki/Rsync).
Then, I type out the resulting URL into a browser on a free-printing-enabled
school computer.

This process still isn't great. It requires me to have the foresight to copy
the compiled PDF to my server at home, or to make sure to bring my laptop to
school. At the time I was still working at
[OOHLALA Mobile](https://oohlalamobile.com/), so my laptop was my work laptop,
a 13-inch late 2013 Macbook Pro. That machine is somewhat _clunky_, so I tried
to leave it at the office as much as possible.

To solve this, I looked to
[Github webhooks](https://developer.github.com/webhooks/), which cause HTTP
requests to be sent by Github when certain events occur on repositories. I
figured I could write a program that would listen for webhooks indicating
pushes to my school repository, and upon receiving one pull the latest commit
of that repo and run a makefile in the repository root. The makefile would do
the hard work of actually compiling all the assignments in the repository.
Then, this checked out copy of the repository with all assignments compiled
could be made available over HTTP.

After implementing the first iteration of
[the webservice](https://github.com/tsani/school-build) to listen for the
webhooks, I factored out the special logic for handling Github webhooks into
[servant-github-webhook](https://hackage.haskell.org/package/servant-github-webhook).
This was my first ever published Haskell library! This was also my first foray
into more advanced type-level hacking in Haskell.

One day, I pushed some changes from home to my school repository after
finishing up an assignment, went off the school, logged on to one of the
blessed free printing quote computers, and got ready to print. Yet when I typed
the URL into the browser, I got a 404 error!

It turns out that the build had *failed* on my server. (At that moment I
rediscovered how bad things can get when your development and production
environments are different, even if only slightly.)

I needed a way to know whether the build on the server succeeded or not, which
brings us all the way back to the very beginning of this post.

[Pushbullet](https://pushbullet.com) is a pretty cool service. It can be used
as a simple way to add notifications to your applications, to send and receive
SMS from devices other than a phone, and to mirror notifications between
devices, among other things.

I figured that Pushbullet, since it was already installed on my phone at that
point, would be the simplest way to make my webservice notify me of the status
of the build. I wrote up some barebones
[bindings](https://hackage.haskell.org/package/servant-pushbullet-client) using
servant-client to the Pushbullet API. Shortly later, I was writing the commit
to add Pushbullet integration to my school repository's continuous integration
webservice.

In looking at the documentation of the Pushbullet API, I saw that it wouldn't
be much extra work to add bindings for the SMS-related endpoints. I wrote
[tpb](https://github.com/tsani/tpb)
to wrap the API calls. At last I could stop using the clunky and memory hungry
Pushbullet webapp to send and read SMS!

However, I was still missing one piece of the puzzle. The Pushbullet HTTP API
does not provide a *nice* way to determine when new SMS have been received. I
say "nice" because it would be straightforward albeit tedious to poll the SMS
endpoint and keep a running diff of the output to determine when new SMS have
been received. This strategy has the downside of consuming a lot of the API
usage quota to get decent notification delivery speeds.

Luckily, the folks over at Pushbullet thought of this one. To receive realtime
events, it suffices to hook into their realtime event stream, which is
available over a websocket connection. However, it didn't make sense to add
this functionality to tpb itself at this point. tpb is a batch processing
program. It has the structure `main = getInput >>= sanitizeInput >>=
computeStuff`,
([seriously](https://github.com/tsani/tpb/blob/5f160421a1b7778f09afa77723674dd975adbb9d/src/tpb/Main.hs#L38))
which is not the structure of a long-running process to send desktop
notifications.

I instead wrote a separate executable called `pb-notify` with
[some](https://hackage.haskell.org/package/websockets)
[more](https://hackage.haskell.org/package/wuss)
[dependencies](https://hackage.haskell.org/package/libnotify).
This new executable is just
[50 lines of code](https://github.com/tsani/tpb/blob/5f160421a1b7778f09afa77723674dd975adbb9d/src/pb-notify/Main.hs),
almost half of which are just imports!

SMS on the command line
-----

### Installation

I've published [a package](https://aur.archlinux.org/packages/haskell-tpb-git)
to the AUR that builds tpb and pb-notify HEAD from git. If you're running Arch
Linux, I'd say this is the easiest way to install everything.

Otherwise, the safest way to build tpb and pb-notify is using cabal.

```
git clone https://github.com/tsani/tpb
cd tpb
# if you're running a recent version of cabal:
cabal new-build
# if you're running an older version:
cabal sandbox init
cabal install --only-dependencies
cabal build
```

This will create the two binaries `tpb` and `pb-notify` under `dist` or
`dist-newstyle` depending on whether you used `build` or `new-build`,
respectively. Copy these to a directory in your PATH.

Finally, since the command line interface for tpb itself is a bit clunky (lots
of `--long-form` options for even simple things) there is a helper script
`scripts/sms`. Copy this script to a directory in your PATH as well.

### Configuration

For simple usage, tpb and pb-notify are configured via environment variables.

First, you need to generate a Pushbullet API key.
[Log in](https://pushbullet.com) to Pushbullet's web application and go to the
settings tab. Click "Create Access Token" to obtain an API key. In your
`.bash_profile` (or `.zshenv` or whatever) add:

```
export PUSHBULLET_KEY="<API KEY>"
```

Now basic functionality is available in tpb. List your Pushbullet-connected
devices with `tpb --jsv list devices`. Take note of the ID of the device you
would like to use to send SMS. In same file as before, add:

```
export PUSHBULLET_DEVICE="<DEVICE ID>"
```

### Usage

#### pb-notify

With `PUSHBULLET_KEY` set, run `pb-notify`. That's it.

The notifications are sent via libnotify, so you need to have a
libnotify-compatible notification application running. On "big" distros, such
an application is usually built-in. If your distro does not have a built-in
notification delivery application, I recommend
[dunst](https://dunst-project.org/).

##### BONUS: notification sound with dunst

If you do decide to use dunst, you can get it to play a sound specifically for
pb-notify!

Create the script `pb-notify-sfx.sh` somewhere in your path with the following
contents.

```
#!/bin/bash

exec paplay "${XDG_DATA_HOME:-$HOME/.local/share}/pb-notify/notify-sfx.wav"
```

(This assumes you're using Pulseaudio, which most modern Linux systems do.)
You can use any audio file you like. It doesn't necessarily have to be a WAV
file either. I didn't like any of the notification sounds I found, so
[I made](#notify-sfx-license)
[one](https://files.jerrington.me/notify-sfx.wav). Place the audio file in
`$XDG_DATA_HOME/pb-notify` (that variable defaults to `$HOME/.local/share`) and
adjust the file name in `pb-notify-sfx.sh` if you chose a different
notification sound from mine.

In your dunstrc (usually `$HOME/.config/dunst/dunstrc`), add the following
section.

```
[pb-notify]
    appname = pb-notify
    script = pb-notify-sfx.sh
```

Now whenever pb-notify sends a notification, dunst will play a sound!

#### tpb

tpb shouldn't really be used directly, because of its clunky command line
syntax. Instead, it's easiest to use the `sms` bash script bundled with tpb.
Whereas pb-notify only needed the `PUSHBULLET_KEY` environment variable, `sms`
needs both that variable as well as `PUSHBULLET_DEVICE` to work.

Here are the supported commands:

  * `sms` or `sms threads` - list all SMS conversations, most recent last.
  * `sms from NAME` - show the contents of the most recent thread with at least
    one recipient whose name matches `NAME`.
  * `sms to NAME` - send a message to the phone number of the recipient whose
    name matches `NAME` in the most recent thread.

The `NAME` matching just does a case-insensitive substring check.

Of course, this most-recent-first matching is undesirable when you have SMS
threads with different people having the same name. I don't have ~~any
friends~~ that problem, so I didn't implement a feature to select further
matches.

There is a bug/feature with this name matching. If you just run `sms from` it
will print the contents of the thread with the most recent activity, since the
empty string is trivially a substring of anything.

<a id="notify-sfx-license" rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Sound" property="dct:title" rel="dct:type">pb-notify SFX</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://files.jerrington.me/notify-sfx.wav" property="cc:attributionName" rel="cc:attributionURL">Jacob Thomas Errington</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
