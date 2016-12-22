# PureAgents: A Library for pure Agent-Based Simulation & Modelling (ABM/S) in Haskell

This is one of the major outputs of my PhD I strive for: a compact library for pure ABM/S in Haskell as there does not exist such a thing yet. For now only very basic things exist and lots of work is to be done:

- Develop Agent-Monad for easier handling and chaining of actions
- run inside Dunai/Yampa for leverage of the power of EDSL and SFs
- run STM-Version in parallel (does loose its purity then!)

The driving force behind this Library are two main dreams I want to pursue 
1. Developing an EDSL which allows both model-specification AND implementation: no more distinction between Model and Implementation!
2. Reasoning about properties and dynamics of a Simulation based upon this EDSL.

These two points are also the most powerful features of pure functional programming in Haskell and in combination they just blow you away! Time will tell if I succeed or not :)

Maybe I will write a paper about its implementation when it is finished (Title "Pure By Nature: A Library for Agent-Based Simulation & Modelling in Haskell") but I don't have time for this yet.

Jonathan Thaler, 22th December 2016
