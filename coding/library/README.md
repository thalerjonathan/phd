# PureAgents: A Library for pure Agent-Based Simulation & Modelling in Haskell

This is one of the major outputs of my PhD I strive for: a compact library for pure ABM/S in Haskell as there does not exist such a thing yet. For now only very basic things exist and lots of work is to be done:

- Develop Agent-Monad for easier handling and chaining of actions
- run inside Dunai/Yampa for leverage of the power of EDSL and SFs
- run STM-Version in parallel (does loose its purity then!)

Maybe I will write a paper about its implementation when it is finished (Title "Pure By Nature: A Library for Agent-Based Simulation & Modelling in Haskell") but I don't have time for this yet.

Jonathan Thaler, 22th December 2016
