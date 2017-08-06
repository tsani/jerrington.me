---
title: "systemd-nspawn: disabling link-local addressing"
---

I recently migrated my various bespoke web services from a bunch of AWS EC2
instances to a single DigitalOcean droplet. The reason is simple: I ran out of
AWS credits. In an effort to expedite any future migrations, I decided to
containerize all my services. Rather than go with a full-blown container
solution like Docker or LXC, I opted to roll my own using `systemd-nspawn`.

Everything was set up and working well, until I noticed that it would take my
blog a full _three seconds_ to load, whereas it should be pretty much
instantaneous. Let's debug this.

Measuring connection times
--------------------------

Accessing the blog works like this: a local machine connects to the nginx
instance running on `jerrington.me` which performs TLS termination and
reverse-proxies the request into a container named `blog`. This machine has its
own dead-simple nginx instance running to serve up the static blog files.

First, let's check the speed of a no-frills TCP connection from my local
host to the remote host. The easy, quick-and-dirty was to accomplish this is
with `netcat`.

    netcat -lp 5000 # on the remote host
    time netcat -z jerrington.me 5000 # on the local host

The `-z` switch sets netcat into the "zero I/O" mode, which makes it
immediately shut down the connection after establishing it. This can useful (as
it is here) for simple network diagnostics.

This took about 20 milliseconds, so the problem isn't with the connection to
the remote host.

Next let's try the connection from the remote host to the container. Using the
same measurement technique, I found that this took about three seconds. There's
the problem. But why?

What's stranger is that some containers can be connected to instantly, but the
blog container has this flat three second penalty on any new connection.

I should mention at this point that I'm using the NSS mymachines plugin which
enables glibc's `gethostbyname` to resolve container names to the IP addresses
assigned to them internally. So to connect to the blog container, I just needed
to write `blog` rather than an IP address that changes on every reboot due to
DHCP.

I was at a loss about where this three second penalty could possibly be coming
from, until I tried `telnet`. In the container I set up another simple server
with netcat, and on the host I connected to it with telnet. Telnet tells you
something very very useful when it's establishing a connection, and that's the
order in which it tries IP addresses for the host you're trying to connect to.
Here's what I saw:

    $ telnet blog 5000
    Trying 169.254.0.134...
    Trying 10.0.0.99...
    Connected to blog.
    Escape character is '^]'.

And guess what? Trying the link-local address took about three seconds!

For comparison with the containers that don't have the mysterious three second
penalty, if I telnet into those, then I see that the private IP address is
tried first, which immediately succeeds. That's why those containers could be
connected to in a reasonable time.

At this point, rather than continue my investigation of why it takes three
seconds for the client to decide that the link-local address is no good, I
simply decided to eliminate link-local addressing from my containers.

Disabling link-local addressing
-------------------------------

The beautiful thing about systemd-nspawn containers is that the networking is
almost like magic. All you need to do is use `machinectl start blog` and tadaa!
The container is now behind NAT. If you want to forward ports from the host
directly into a container, it's easy to do so by specifying `Port=2233:22` for
instance in the `blog.nspawn` configuration file for the container.

There's one problem with magic though, and that's that nobody actually knows
how it works. All this stuff is taken care of by systemd-networkd and it's
unclear how exactly it does these things.

Luckily I stumbed onto a GitHub issue that made mention of the files
`/lib/systemd/network/80-container-ve.network` and
`/lib/systemd/network/80-container-host.network`.
These files respectively configure the host and guest systemd-networkd
instances to work together to provide DHCP.
And guess what I found in these files? `LinkLocalAddressing=yes`.

It suffices to copy these files into `80-container-ve.network` into
`/etc/systemd/network/` on the host and `80-container-host.network` into
`/etc/systemd/network/` on the guest and set `LinkLocalAddressing=no` in both
to completely eliminate link-local addressing from the network setup.

At last, connection times were normal again, and everybody lived happily every
after.
