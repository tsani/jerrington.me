---
title: About
---

I've always been fascinated by magic.

In the modern age, the closest thing we have to magic is programming.
You have to learn the secret languages, passed down through generations, some
more arcane than others, each with its philosophy and its idioms.
Hence, I'm fascinated by programming.

I grew up in Montreal and developed an interest in computers at a pretty
young age. I remember being in the sixth grade when a friend of mine told me
that he figured out how to make video games, and from that point onwards I was
hooked.
(It was only much later that I realized that game development is way too
complicated for me, and requires a lot more than just programming!)

I first taught myself some basic programming in C and in C++, and
later in high school, I was given the opportunity to take part in the
Be a Computer Scientist for a Week day camp offered at McGill, where we
programmed [Boe-Bots](https://en.wikipedia.org/wiki/Boe-Bot) in BASIC to
navigate a maze.
In [CEGEP](https://en.wikipedia.org/wiki/CEGEP), I took a course on numerical
methods for physical applications, which involved a programming project.
Rather than do a physical simulation, my team created a viewer and builder
for 3D cellular automata similar to
[Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).
Around this time, I started to get interested in functional programming.

In 2014, I started a B.Sc. degree in Computer Science & Math at McGill.
I worked as a research assistant for [Simon
Gravel](http://simongravel.lab.mcgill.ca/) till 2015.
While there, I developed visualizations for genetic data as well as an
improvement for a tool called Tracts used to model ancestry information.
In 2016, I started work at Oohlala Mobile (now [Ready Education](https://www.readyeducation.com/)).
I did Python backend work for their mobile applications, which are whitelabel
products sold to universities. I was in charge of developing integrations
with university systems (SIS, LMS) to pull in information such as student
calendars, event calendars, course lists, etc.
For the compilers course I took in early 2016, my teammate Frédéric Lafrance and
I wrote our compiler for a subset of Go in Haskell.
We got top marks in the class and I was given an honorary A+ grade by the
instructor, Prof. Laurie Hendren -- may she rest in peace.
(McGill does not actually have an A+ grade.)

In early 2017, I left Oohlala, and in the summer I interned for 10 weeks at
[1010data](https://1010data.com/).
My project's goal was to improve the speed of the core platform login. One
aspect of the login that made it slow was that a new process was spawned
for each user session and this process needed to load a huge number of
libraries. The idea to improve the speed was to spawn a partially initialized
session process, with all the libraries loaded, and to fork it when a user
needed to log in. Then the child process can complete the usually quick
user-specific initialization tasks. The challenge was to get the
[K](https://en.wikipedia.org/wiki/K_(programming_language))
interpreter, which runs the user session process, to fork at all.
The interpreter itself could not be directly extended as it is
closed-source. Instead, I leveraged K's ability to run extensions in C, and had
to figure out how to get the interpreter to not crash after forking due to
issues with inherited file descriptors.
Yes, this is a horrible hack, but it did work extremely well.

In spring 2017, I met my partner Eric Mayhew, who was at the time enrolled in the B.Ed. program at
McGill. Although I had always had an interest in teaching, Eric showed me how wonderful
student-centered teaching can be. I have several fond memories of teaching him CS over coffee in
the morning, directed mainly by his questions. Of course, I wasn't very good at teaching back then.
I thought that the best way to start would be to throw him completely into the deep end: we
installed Arch Linux in a virtual machine, and I showed him Haskell by way of implementing a
recursive regular expression recognizer -- all this in vim with the arrow keys disabled!

In fall 2017, Eric and I taught the first iteration of an informal course on the fundamentals of
computer science, (unimaginatively) called [Computing Workshop](https://computing-workshop.com/).
Since then we have taught this course many times, gradually refining it and its focus. Now, it is a
five-lesson introduction to the fundamentals of machine learning in Python.

In late 2017, in my last term of undergrad, I did a research project with
[Brigitte Pientka](http://www.cs.mcgill.ca/~bpientka/) that sought to understand
how we could use [copatterns](https://dl.acm.org/doi/10.1145/2480359.2429075) in
a language with an explicit model of time (see
e.g. [here](https://dl.acm.org/doi/10.1145/2535838.2535881)).
Through the course of this project, my interest in research really blossomed, so I refused the
return offer I received from 1010data to instead do a masters under the supervision of Prof.
Pientka. (Plus, 1010data wasn't paying enough for me to want to move to and live in New York City.)

In 2018, I started my masters degree in Computer Science at McGill.
My advisor was Brigitte Pientka. At first I thought about extending the work I
had done in my undergraduate research project, but this eventually shifted to
working on proof search techniques with the eventual hope to integrate some of
these into [Beluga](http://complogic.cs.mcgill.ca/beluga/).
We realized that to directly generate Beluga programs might be unnecessarily
tricky, so we designed an intermediate language called the "proof script
language". This is at once more high level and more verbose than the traditional
Beluga functional programming language.
The focus of the project shifted once more as I began development on Beluga's
interactive mode.
Ultimately, I developed with the help of Clare Jang and Marcel Goh a brand new
interactive mode for Beluga called Harpoon.
The interactions in this system are grounded in Contextual Modal Type Theory,
and I gave an explicit elaboration from interactive actions into proof script
fragments.
Then I showed that the process of stitching these fragments together into a
larger proof script preserves types.
Finally, I developed a type-preserving translation from the proof script
language into the traditional Beluga programming language.
This demonstrates that interactive proofs are sound, and furthermore that any
proof constructed interactively for a given theorem is indeed a real proof of
that theorem.

In fall 2019, Brigitte was scheduled to teach two sections of the course COMP 302, on functional
programming, so she offered to co-teach the course with me. I taught one of the two sections and
was thrilled by the experience. This solidified for me an interest in teaching in higher education.

I finished the masters program in summer 2020, and my thesis is titled
"Mechanizing Metatheory Interactively". I presented this work at at LFMTP 2020
and later at CADE 2021. You can read the paper [here][harpoon-cade] and all the
gory details in [my thesis][thesis].
As my time in the masters program was concluding, I decided that a career in research seemed a
little too stressful and cutthroat -- the career trajectory for young academics appeared far too
grueling for me. I chose to go to the industry instead, to get some more experience as a
professional software developer, in particular in a larger organization.
I reached out to a company that I had previously interviewed at both for internship and for
full-time positions, and got the job.

In summer 2020, I started working in a trading company called DRW. (In hindsight, what doesn't make
a lot of sense that that decision is that I rejected academia for being grueling and cutthroat, but
then went to work in fintech!)
As this was at the height of the covid19 pandemic, I worked fully remotely.
Ultimately, I decided that such a job in the industry didn't suit me. In summer 2021, I realized
that my favourite part of my job was showing my coworkers new and different ways of doing things.
So with significant help from my partner, I pivoted careers into teaching. In fall 2021, I
interviewed to be a faculty lecturer at McGill University, and got the job, so I left DRW in spring
2022.

Since summer 2022, I have been a faculty lecturer at McGill, and so far I'm
enjoying it much more than professional software engineering. I've primarily taught COMP 302, the
topic of which I'm very passionate about. It's been a pleasure to refine the material and my
teaching style over the semesters that I've taught the course.

[harpoon-cade]: /pdf/harpoon.pdf
[thesis]: /pdf/thesis.pdf
