# phd
Stuff of my PhD I want to share publicly: all code, texts of my papers, reports, research-notes &amp; research-diary

In my Ph.D. in computer science at the University of Nottingham I am looking into exploring the benefits and drawbacks of using the functional programming paradigm for implementing Agent-Based Simulations. 

To give a short overview of the ideas and research directions the abstract of my Research Proposal is quoted here:

Agent-Based Modelling and Simulation (ABMS) is still a young discipline and the dominant approach to it is object-oriented computation. This thesis goes into the opposite direction and asks how ABM/S can be mapped to and implemented using pure functional computation and what one gains from doing so. To the best knowledge of the author, so far no proper treatment of ABMS in this field exists but a few papers which only scratch the surface. The author argues that approaching ABM/S from a pure functional direction offers a wealth of new powerful tools and methods. The most obvious one is that when using pure functional computation reasoning about the correctness and about total and partial correctness of the simulation becomes possible. Also pure functional approaches allow the design of an embedded domain specific language (EDSL) in which then the models can be formulated by domain-experts. The strongest point in using EDSL is that ideally the distinction between specification and implementation disappears: the model specification is then already the code of the simulation-program. This allows to rule out a serious class of errors where specification and implementation does not match, which is especially a big problem in scientific computing.

Jonathan Thaler, 27th July 2017
