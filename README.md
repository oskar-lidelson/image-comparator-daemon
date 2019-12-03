1. My Thought Process
=====================

1.1 Similarity Measure
======================

First, naturally, the mathematician in me went straight to the image
similarity algorithm, since that's the most interesting part.

I had a rough idea for a simple similarity test, but then I realized
that 'similar' is pretty subjective, so I'll need multiple sorts of
tests.

Since I had less than a week to write the code, I decided to stop at
two, and to stick to simple measures. There would be time to add more
later.

I did actually look into a more interesting measure involving the
cross-correlation function, but found that applying it to 2D functions
results in horribly complex sums, and on top of that, finding a
similarity score involves calculating the maximum value of this
function, but..in which direction, and from which starting point? So I
shelved this one as something to think about later.

So I came up with these:

1.1.1 The Smudge Test
===================

For this test, I take a disc integral centered on each pixel in the
image, and store that value. I do this for both images. The result is
a pair of 'smudged' images where minor details (like single-pixel
errors) are washed away by the combined values of all the nearby
pixels.

From here, I can take the average of the absolute value of the deltas
between pixels. This test is pretty good at measuring how 'similar'
two images would look if you took off your glasses.

As a bonus, you can tune it by specifying how large the disks should
be. I set that as an optional 'fuzziness-value' parameter.

1.1.2 The Color Space Test
========================

For this test, I break the color space into partitions, and count the
number of pixels for each image that fall into each. The similarity
score at the end is the mean of the absolute value of the deltas for
each partition.

This test is pretty good for detecting whether two images have similar
colors, but pretty terrible at detecting much more than that.

As before, the number of partitions you split the space into is
configurable, and that's an optional 'fuzziness' parameter.

1.2 Architecture
================

Next, I thought about how I would want the system to work if I were
Bjorn. 

First of all, Bjorn wants to work on both MacOS and Windows, and
cross-compiling is a nightmare (and my deadline is tiny), so a pure
binary executable is right out.

Secondly, I had no way of knowing how many images poor Bjorn needs to
operate on, or how much power his at-home processor could muster. I
know my home computer isn't all that great right now.

So I decided that the system should be hosted at the office
(maximizing its compute potential), and that Bjorn could access it
remotely from anywhere (solving the cross-platform problem).

The problem is that the system is supposed to read image files off the
disk. Now, I'm supposing that Bjorn spends more time at work than at
home, and so I can suppose that his work machine is NFS-mounted on the
image-comparator system (or the other way around). That way, it can
read his files. I'm also going to assume that when Bjorn is at home,
that he can push files to the disk over Samba. This solves the disk
access problem, and solves the disk space problem, too. There's no way
to tell if Bjorn's home system has a large disk. Now the office system
can store his mega-sized images for him.

1.2.1 The Trivial Solution
==========================

It's always good, I think, to start with the trivial solution.

The trivial solution (to me, anyway) is to have a daemon listening on
a Samba+NFS-mounted directory, reading in a config file that it finds
there, then operating on each image pair in sequence before dropping
the results into an output directory. Simple. Since it's just disk
based it can be a pure shell script.

1.2.2 Bjorn Is Insane
=====================

Next, I considered, what if Bjorn wanted to compare fifty billion
images? Why? Well, I don't know. Who knows what he's thinking.

So I thought it best that I split the processing from the config
files. Since I'm already going to be using a network mount for the
disk, there's no harm in adding more machines to the cluster if
necessary. So I came up with the idea of worker systems.

1.2.2.1 MicroService Worker
===========================

The Worker Systems are microservices that sit on the network-mounted
directory and accept tasks from the master over the network.

I could have gone purely with more directory-based dropping of config
files, but I wanted to use endpoints here because they're faster (no
need to poll the filesystem) and they generate better logs for
debugging.

Now all I have to do is figure out how to return the task result (the
time taken, and the similarity score).

1.2.2.2 Master Endpoint for Task Reporting
==========================================

The master process, I decided, would generate a unique task ID for
each task it sends out, recording it in a little database (Sqlite,
it's fastest to develop for), and have an HTTP endpoint for the worker
processes to report back their similarity score to.

I thought this was a great place to put a 'check to see if all tasks
are finished' line, too. Just a simple database count query.

..but now I need to design the database.

1.2.2.3 Database Structure
==========================

At the highest level the image comparator reads in a new CSV file and
starts working on it. I'll call this a 'workload'.

Workloads consist of arbitrarily many tasks, each of which is
performed on a different worker.

So it's clear that I need at least two tables: Workloads, which has a
row for each csv file read in, and Tasks, which associates tasks to
Workloads.

And now I know what the 'workload complete' query looks like. When a
task is reported as finished, I look up its corresponding workload,
then query the Tasks table to get me a list of all Tasks associated
with that same workload which have their status set to 'in
progress'. If there are none left, the task is finished, and I can
write those task results to disk before cleaning that workload and
task from the database.

1.3 Language
============

I actually started writing the code in Lisp. It's the fastest language
I have for developing in, and I figure that as long as I comment it
enough to make it readable, it is better for me to submit a
feature-complete program in a 'strange language', than a half-finished
one in a language you'd recognize.

After all, from the commented code, I can then rewrite it quickly in
the common language. (This is my standard coding procedure. Prototype
in Lisp, rewrite in C++ for performance).

1.4 Support Structure
=====================

Since it's a daemon, I figured I'll write a systemd unit file for
it. Straightforward stuff. That should all be in the setup
instructions.

