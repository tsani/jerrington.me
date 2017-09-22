---
title: Cooperative terminal reparenting
---

TODO URL
Reptyr is an incredible piece of software.

Reptyr hooks into a process identified by a PID and uses `ptrace` to inject
some code into it. This code makes the process open up a PTY pair that reptyr
has provisioned. The process then receives its standard input from this PTY,
and sends its standard output and error to this PTY. What reptyr does on the
other end is forward its local terminal to this PTY, so that anything typed
into reptyr goes to the target process, and anything coming from the target
process appears in reptyr's local terminal. The effect is that reptyr _becomes_
the target process, for most intents and purposes.

Reptyr _reparents an existing process's terminal_.

TODO lookup sysctl flag name
There are some downsides to reptyr, namely that it uses `ptrace`.
(Using `ptrace` requires the privilege to debug the target application. On most
linux systems, a program can debug any of its subprocess or any process that
belongs to the same user. The latter point isn't true on certain systems, e.g.
Ubuntu, where a particular sysctl parameter is set.)
There's no way around this, however. Reptyr's purpose is to reparent another
process's terminal _even if that process isn't built for that purpose._

But what if we're building application who's purpose _is to be reparented?_

As a concrete example, suppose we're writing a server application that uses
forking to service individual clients. For debugging purposes, it might be
interesting for each fork to be placed into its own `screen` session. However,
a fork inherits its parent's file descriptors, which includes the standard
descriptors presumably attached to a terminal.

Terminal reparenting is what allows the forks to be effectively be placed into
a new terminal.

Before we dig into the high-level overview of what needs to be done, we should
dispense with the terminology.

Terminal emulators and pseudoterminals
--------------------------------------

* The _terminal emulator_ is an application that houses an inner application.
  The terminal receives input from the user and sends it to the inner
  application. The terminal receives the output of the inner application and
  displays it to the user. Essentially, the terminal emulator is the interface
  between the user and a text-mode application such as a shell. It's the
  terminal emulator's job to process escape sequences sent by the inner
  application to produced varied sounds and colours as well as reposition the
  cursor and draw all the text written by the inner application.

* A _pseudoterminal (PTY)_ is essentially a local socket with superpowers.
  And by superpowers I mean loaded with weird capabilities inherited from the
  times of physical terminals. For example, the pseudoterminal can be
  instructed to transform carriage return characters into newline characters,
  or strip off the eight bit of every octet. Weird.
  Another distinguishing feature of the PTY is that it has a _size_, i.e. a
  width and height measured in _characters_.

  PTYs are represented in the filesystem, much like a named pipe or a UNIX
  socket. However, unlike FIFOs and sockets, which can be placed wherever one
  pleases in the filesystem, PTYs are typically housed under `/dev/pts` and are
  given numeric names by the kernel.

  That was a slight simplification. PTYs come in _pairs_.
  When a process allocates a new PTY from the system, it obtains a file
  descriptor referring to the _master end_ of the PTY pair. Once any setup of
  the PTY is complete, the allocating process uses the `grantpt` function to
  construct the _slave end_ of the PTY and the `unlockpt` function to allow
  other processes to open the slave end. The `ptsname` function is used to
  obtain the path, under `/dev/pts`, of the slave end. The master end is not
  reflected in the filesystem.

When a terminal emulator starts up, before it launches the user application
such as the shell, it creates a new PTY pair and obtains the path to the slave
end. To launch the user application, the terminal emulator `fork`s, and `open`s
the 
