---
title: Managing downloads from the command line
---

I use the shell for (almost) all my file management. The shell excels at
flexibility at the cost of some time spent reading man pages to sometimes get
it to do what you want. There's one thing that I found particularly unpleasant
to do with the shell, and that's managing downloads.

## Listing downloads

When I used to use Windows, my downloads folder would quickly climb into the
gigabytes of disk storage and was simply an utter mess. The first thing I'd do
to keep it organized is to always keep it sorted by date, most recent files
first. Thankfully, listing files by their last modified time (mtime) is easy
with `ls -t`. However, this puts the most recent files *first*, so in large
directories, we can't even see the files. Seems like the coreutils developers
thought of everything though; we can reverse the ordering of `ls`'s listing
with `-r`, giving `ls -tr`. Combining with with `-l` so we get one file per
line with the full information gives a pretty good human-readable listing of a
downloads folder. Of course, we can make a script (or alias) out of `ls -ltr`
followed by the path to our downloads folder in order to conveniently get the
listing of downloads from anywhere.

## Manipulating downloads

But we don't only *look* at downloads. We want to *do* things with them. In
particular, the most common use I have for files I've downloaded is to move
them into some other folder, e.g. for school assignments. Typing out `mv
~/downloads/` followed by a bunch of tab-completes just to get the most recent
file is inelegant and slow -- let's make a script.

### `mvdl`

We know how to get the most recent downloads with `ls -t`. It suffices to just
pull out the first entry and move it. Let's assume for now that we always want
to move files into the current directory.

```
#!/bin/bash

mv -v "$(ls -t "$DOWNLOAD_DIR" | head -n 1)" .
```

Since `mvdl` is meant to be used pretty much always interactively, it seems
fine to bake in a `-v` so we can see what file we moved.

Let's examine the main part more closely.

`ls -t "$DOWNLOAD_DIR" | head -n 1` fetches the most recent file in the
downloads directory. Although `ls` displays entries in a paragraph-filling kind
of way when used interactively, when its output is not a tty (i.e. not
interactive), it displays one entry per line. `head -n 1` then takes just the
first line.

What if we want to move more than one file? It suffices to make our `mvdl`
script process some switches, in this case `-n`, and forward the result to
`head`.

There's a _little_ problem with this script though. `ls` doesn't write out the
full path to the files it lists! It writes out a path that's relative to the
provided path. To fix this, we now have two options.

 1. We can prepend `$DOWNLOAD\_DIR` to each line produced by `ls`.
 2. We can use `find`.

Both strategies will require us to iterate over the list produced by the
command, so it doesn't really matter which we choose. Let's see how we can
implement both.

```
# Getting the full paths with ls
ls -t "$DOWNLOAD_DIR |
while read line ; do
    echo "$DOWNLOAD_DIR/$line"
done

# Getting the full paths with find
find "$DOWNLOAD_DIR" -maxdepth 1 -type f |
while read line ; do
    eval "$(stat -s "$line")"
    echo "$st_mtime $line"
done |
sort -rn |
cut -d " " -f2-
```

The `ls` case is pretty straightforward, but whoa, what happened with `find`
there? The problem with `find` is that there's no built-in way to control the
order of the output entries. We use find only to produce the list of all
files immediately in the downloads directory. For each file listed, we get its
mtime. The stat command with the `-s` switch produces on a single line a string
consisting of words like `X=Y` separated by spaces. This is exactly a
valid sequence of bash variable assignments, so we `eval` it to get access to
the information produced by `stat` in our script. Then we output the mtime of
the file followed by its name. At this stage in the pipeline, we have text of
the form `N FILE` on each line, where `N` is the mtime in seconds-since-epoch
format. A descending numeric sort of this text gives us almost what we're
after. It suffices finally to chop off the mtime from each line, leaving us
with a listing of files, most recently modified first.

Let `lsdl` be a script that generates precisely this listing, using either the
`ls` strategy or the `find` strategy. Then, we can write `mvdl` as follows.

```
#!/bin/bash

mv -v "$(lsdl | head -n 1)" .
```

So simple!

### `rmdl`

Deleting downloads is another big one. This crops up when we want to delete a
document we've finished reading. The script is essentially the same as for
`mvdl`:

```
#!/bin/bash

rm -v "$(lsdl | head -n 1)"
```

Of course, it can be extended with a `-n` switch in the same was as mvdl.

## Other manipulation commands

It would be easy to similarly implement `cpdl`, `rsyncdl`, `ffmpegdl`, etc. but
that's honestly getting out of hand. Instead, we can implement `lastdl` to
print out the path of the last download and use command substitution on the
command line to do these things. Of course, `lastdl` is simply `lsdl | head -n
1`, which we've seen twice already in `mvdl` and `rmdl`.

For example, suppose we've downloaded a wav file. Here's how we can reencode it
as an mp3, rsync it to another machine, and clean up.

```
lastdl="$(lastdl)"
ffmpeg -i "${lastdl}" "${lastdl/%.wav/.mp3}" &&
rsync "$(lastdl)" my_server:music &&
rmdl -n 2
```

First we fetch the last download's path and store it in a variable. This allows
us to use bash's built-in variable substitution syntax to compute the new path
of the file with the extension changed. This will create a new file in the
downloads directory that is newer than the old one, so when we come to use
`rsync`, we'll need to use `$(lastdl)` rather than `${lastdl}`. Finally, we
delete the top two items from the downloads directory.

# Conclusion

Good luck doing stuff like this as flexibly and quickly with a GUI -- 'nuff
said.
