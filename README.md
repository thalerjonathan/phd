# phd
Stuff of my PhD I want to share publicly: all code, texts of my papers, reports, research-notes &amp; research-diary

In my Ph.D. in computer science at the University of Nottingham I am exploring the benefits and drawbacks of using the functional programming paradigm in Haskell for implementing Agent-Based Simulations.

To give a short overview of the ideas and research directions the abstract of my Research Proposal is quoted here:

Agent-Based Modelling and Simulation (ABS) is still a young discipline and the dominant implementation paradigm in it is traditional object-oriented programming. This thesis goes into the opposite direction and asks how ABS can be mapped to and implemented using the pure functional paradigm in Haskell and what one gains from doing so. To the best knowledge of the author, so far no proper treatment of ABS in this field exists but a few papers which only scratch the surface. The author argues that approaching ABS from a pure functional direction offers a wealth of new powerful tools and methods. The most obvious one is that when using pure functional computation reasoning about the correctness and about total and partial correctness of the simulation becomes possible. Also pure functional approaches allow the design of an embedded domain specific language (EDSL) in which then the models can be formulated by domain-experts. The strongest point in using EDSL is that ideally the distinction between specification and implementation disappears: the model specification is then already the code of the simulation-program. This allows to rule out a serious class of errors where specification and implementation does not match, which is especially a big problem in scientific computing.

In the first year I did lots of prototyping which lead to the use of FRP using the library Yampa. This resulted in a first approach to functional ABS which I termed Functional Reactive ABS (FrABS, as opposed to OoABS). I am in the process of implementing a library for FrABS which is also in this repository which forms the tool of research for my approach. So far it only exists as an unfinished prototype which is subject to constant refactorings and refinement but at some point is is planed to give it a proper name and put it on Hackage.

Future work in the 3rd year will look into reasoning, verification, reproducibility, correctness, testing: how does FrABS help with these?

Jonathan Thaler, 27th November 2017
