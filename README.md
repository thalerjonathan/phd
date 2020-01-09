# PhD
Stuff of my PhD I want to share publicly: all code, texts of my papers, reports, research-notes &amp; research-diary

My main research interest is the benefits and drawbacks of using pure functional programming for implementing Agent-Based Simulations (ABS). This is a novel and highly interdisciplinary approach, bridging the gap between functional programming and Agent-Based Simulation for which I am working closely together with the Functional Programming Laboratory. As a summary of what I did in my PhD, the abstract of my thesis is added below.

http://severn.cs.nott.ac.uk/~psxjat/

Implementations of Agent-Based Simulations (ABS) primarily use object-oriented programming techniques, as in Python, Java and C++, due to the established opinion that agents map naturally to objects. These techniques have seen tremendous success throughout the last two decades in general and were able to provide the ABS community with useful simulation tools in particular.

However, the verification process of ensuring the correctness of the implementation with respect to its specification has never been an easy task with the established object-oriented techniques, due to their inherent reliance on unrestricted side effects like mutable data and aliasing through references or pointers.
Further, with the shift towards multicore CPUs in recent years, it became clear that the unrestricted side effects of the established object-oriented techniques can pose serious difficulties in arriving at a correct parallel or concurrent solution.

ABS is almost always used in the context of scientific computation, to test hypotheses, explore dynamics, support scientific theories and to make informed decisions about policies, potentially influencing many peoples lives. Therefore, the aforementioned difficulty of the verification process is a serious issue. Because of their importance, the results of ABS need to be free of programming mistakes and reproducible given the same initial starting conditions.

This thesis investigates the use of pure functional programming with the language Haskell as a potential solution to overcome these difficulties. The central theme of this thesis is to do with purity, which identifies the lack of unrestricted side effects and referential transparency. Fundamentally, a pure computation does not depend on its context within the system, but will produce the same result when run repeatedly with similar inputs. This thesis explores if and how purity and the resulting concepts help to overcome the issues of the established object-oriented techniques, increasing the confidence in the correctness of an implementation, by answering the following research questions:

1. How can ABS be implemented purely functional? What are the benefits and drawbacks in doing so?
2. How can pure functional programming be used for robust parallel and concurrent ABS implementations? 
3. How can pure functional programming be used in automated code testing of purely functional ABS implementations?

Thematically, the research presented in this thesis is split into two parts. The first part deals with the approach to a pure functional ABS implementation (answering research question 1) and the second part with exploring benefits enabled through pure functional programming (answering research questions 2 and 3).
First, the thesis explores how to implement ABS purely functional, discussing both a time and event-driven approach. In each case Arrowized Functional Reactive Programming is used to derive fundamental abstractions and concepts. As use cases the well known agent-based SIR and the Sugarscape models are used. Additionally, the thesis focuses on why it is beneficial to implement ABS purely functional. For this research topic, we explore both robust parallel and concurrent programming, where the main focus is on how to speed up the simulation while keeping it as pure as possible. In the parallel part, we rely on built-in language features and are able to speed the simulation up while retaining purity. In the concurrency part, Software Transactional Memory is used, sacrificing purity but still retaining certain guarantees about reproducibility. Finally, the thesis explores automated code testing of ABS implementations using property-based testing to show how to encode agent specifications and model invariants and perform model verification and hypothesis testing.

The contribution of this thesis is threefold:
1. Development of pure functional implementation techniques for ABS through the use of Arrowized Functional Reactive Programming.
2. Development of techniques using Software Transactional Memory to implement robust concurrent ABS.
3. Development of a new testing approach to ABS using randomised property-based testing for declarative and stochastic code testing.

The results of the respective contributions support the view that pure functional programming indeed has its place in ABS. First, a pure functional approach leads to implementations which are more likely to be valid due to the focus on purity by avoiding computations with unrestricted side effects. Secondly, pure parallel computation and Software Transactional Memory (lock-free) based concurrency make it possible to gain substantial speedup, with the latter one dramatically outperforming traditional lock-based approaches. While pure parallel computation fully retains static guarantees, Software Transactional Memory is not pure, but is still able to retain certain guarantees regarding reproducibility. Finally, property-based testing is shown to be extremely useful, as it naturally maps to the stochastic nature of ABS and is therefore suitable to be integrated into the development process as an additional tool for testing specifications and hypotheses.

Overall, we have fulfilled the aim and supported both the functional programming and ABS communities by giving them new technologies and application areas. We hope that this research will lead to an increased interest and higher acceptance for the use of the functional programming paradigm for the purpose of ABS.
