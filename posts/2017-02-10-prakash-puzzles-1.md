---
title: "Prakash puzzles: a hundred prisoners and a lightbulb"
---

> There is a prison with 100 inmates. Each day, a prisoner is selected
> arbitrarily by the warden and brought to a room with a lightbulb. The
> prisoner observes the lightbulb and has the option, but not the obligation,
> to adjust the state of the lightbulb as they please, so that when they leave
> the room, it is either on or off. The warden guarantees to the inmates that
> each of them will go to the lightbulb room infinitely many times. The warden
> makes the following promise to the inmates: if an inmate asserts ``we have
> each been to the lightbulb room at least once'', then if they are correct,
> all the inmates are freed; else, all the inmates are executed. The inmates
> are all in solitary cells and cannot communicate, with the exception of one
> mass gathering at the beginning of the game where they may establish a
> protocol. Note that initially, the lightbulb is off.

Given this setup, can the prisoners devise a protocol that will free them?

I invite you to try to come up with the solution for yourself. If you need a
hint, read the following paragraph.

It's informative to consider a simpler case with only two prisoners. In this
case, the solution is simple. When one of them enters the room, if they see
that the lightbulb is off, then they turn it on; else, they see that the
lightbulb is on, so they infer that the other prisoner has been, so they assert
``we have all been to the room.''

Notice that an equally valid solution for the two prisoner case is for them to
establish the following protocol. If prisoner one discovers that the light is
off, then they turn it on. Else they discover that it is on and leave it as
such. If prisoner two discovers that the light is on, then they assert that all
prisoners have been to the room. Else, they discover that the light is off, and
they do nothing. Because each prisoner is prisoner is guaranteed to go
infinitely many times, this protocol is in fact a valid solution.

The next paragraph gives the solution, so read no further if you want to find
the solution for yourself.

With three prisoners, this first solution for two prisoners doesn't work.
Suppose a prisoner enters the room and sees that the light is on. They can
infer that at least *one* of the other prisoners has been, but they cannot be
sure that both have been.

The second solution for two prisoners can be extended to a solution for three,
and in fact any number of prisoners. The crux of the solution is this: one
prisoner is elected as a *leader*. When the leader sees that the light is on,
they turn it off. The leader *counts* the number of times that they turn off
the light. Every non-leader prisoner turns on the light if they see that it is
off, but they only turn on the light *once each*. When the leader has turned
off the light twice (or $n - 1$ times in the general case), they assert ``we
have all been to the lightbulb room once''; they are correct.

My professor Prakash Panangaden gave us this riddle after giving us the proof
that
[Generalized Büchi automata](https://en.wikipedia.org/wiki/Generalized_B%C3%BCchi_automaton)
are equivalent to ordinary Büchi automata. I have yet to see *exactly* what the
connection is.

Also, what is it with riddle-makers wanting to make prisoners solve tricky math
problems?
