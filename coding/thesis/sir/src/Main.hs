module Main where

{- TODO agent-model
- need neighbouring of agents (network)

- pattern match on Agent s: sender
- pattern match on Message m: message

- want to send to single random agent
- want to send to single agent
- want to broadcast to all agents
- want to broadcast to all neighbours

- QUESTION: is it possible to send lambdas with data enclosed in the STD?

- need some kind of environment: is a central agent with whom all can interact: can be queried about other agents, infrastructure,... note: it is NOT another domain-specific agent, which is known to ALL existing agents, this has to be implemented in the domain-specific model if sucha thing is required

- want to have proactivity, but how? e.g. calling regularly a function after having polled the message-box?

- is it possible to use deterministic parallelism instead of classic concurrency e.g. transparent parallel execution and communication of data? would resemble more a pure functional style and allows to reason about programs

3 approaches: 1. no parallelism/concurrency, 2. deterministic parallelism, 3. indeterministic concurrency

what about ignoring parallelism and concurrency for now and just iterate round robin through agents?

experiment with basic forkIO, MVar and STM: two threads communicating. install threadscope and look at execution

instead of function to forkIO: use lambda which accesses data from outside forkio

- but in all cases: use IVar, MVar or STM to "communicate" among agents: need some way of sharing data with each other. question: what would be the alternative?

- WANT TO RETAIN ABILITY TO REASON ABOUT PROGRAMM DESPITE USING PARALLELISM AND CONCURRENCY
- ALL HAS TO BE TYPESAFE (but this is ensured by Haskell anyway)
-}

----------------------------------------------------------------------------------------------------
