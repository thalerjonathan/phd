# PhD
Stuff of my PhD I want to share publicly: all code, texts of my papers, reports, research-notes &amp; research-diary

My main research interest is the benefits and drawbacks of using pure functional programming for implementing Agent-Based Simulations (ABS). This is a novel and highly interdisciplinary approach, bridging the gap between functional programming and Agent-Based Simulation for which I am working closely together with the Functional Programming Laboratory [url]

In the first half of my PhD I did lots of experimenting and prototyping where I investigated how to do ABS in pure functional programming, which resulted in the [Pure Functional Epidemics] paper, which is submitted to Haskell Symposium 2018 with feedback pending at the moment. I also wrote [The Art Of Iterating: Update-Strategies in Agent-Based Simulation] paper, which looked at foundational issues, from a programming language agnostic perspective when implementing ABS.

The second half of my PhD is dedicated to researching why using pure functional programming for ABS is of benefit. The main argument is that with pure functional programming we have more and stronger guarantees of the correctness of a programm already at compile time. Additionally I want to bring in dependent types, which have never been investigated in ABS, thus making this my unique contribution to knowledge of my PhD. I hypothesise that by adding dependent types, certain approaches like test-driven development become obsolete, it allows to get rid of run time errors completely and dramatically increases the correctness of the implementation. Currently I am developing techniques how to use dependent types in ABS using Idris, which will result in my 3rd paper (yet to be written and released), to be targeted at a journal, probably ICFP.

A fourth paper termed [Towards pure functional Agent-Based Simulation] will be written and aimed at a journal, after the main research of the PhD has finished around December 2018. It will explain all the concepts and implications of the pure functional and dependently typed approach to the ABS community from a very high level perspective.

The writing process of the final thesis will start in April 2019.

http://severn.cs.nott.ac.uk/~psxjat/
