---
title: Roll your own reminders with dunst
---

[Dunst](https://dunst-project.org/) is a very simple notification daemon. It
displays notifications sent by whatever programs on Linux that use libnotify,
which is most programs that have some form of notification. Libnotify also comes
with a script `notify-send` that allows sending notifications from the command
line.

Using `notify-send`, `at`, and a bit of configuration for `dunst`, we can make a
very simple reminder script that displays a notification after a certain time
has elapsed. We can make the reminder notification a bit more obtrusive than
ordinary notifications, so there's no chance of missing it.

First, here's the script that I call `remindme`. Place this somewhere in your `$PATH`.

```
#!/bin/bash

time="$1"
text="$2"

echo "notify-send --category reminder 'Reminder' '$text'" | at now + "$time"
```

To use this script: `remindme '10 minutes' 'take out the trash'`

This script isn't specific to dunst, so it will work other notification display
software. However, if you *are* using dunst, then you can add the following to
your `dunstrc` to get the notifications in the `reminder` category to be
displayed specially.

```
[remindme]
  category = reminder
  background = "#333333"
  foreground = "#ff7f7f"
  timeout = 0
```

I picked these colours because they contrast with my usual notification colours,
which are basically black text on a white background. Customize them to match
your colour scheme.

Ordinary notifications have a nonzero timeout that will make dunst hide them
after a short time. That's no good for a reminder! What if you're away making a
coffee while the reminder appears. Then it might disappear before you get
back. I set the timeout for reminders to zero to make sure that they stay
onscreen until dismissed (by clicking).

Adjustments
-----------

I hardcode the `+` syntax in `remindme` since I almost always want a reminder
relative to the current time. But maybe you want to be able to do
`remindme '7am tomorrow' 'take out the trash'`.
In that case, ditch the `+` and if you want a relative time use it explicitly.
