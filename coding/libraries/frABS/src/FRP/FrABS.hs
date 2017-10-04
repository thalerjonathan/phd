module FRP.FrABS (
    AgentId,
    AgentMessage,
    AgentBehaviour,

    AgentConversationReply,
    AgentConversationReceiver,
    AgentConversationSender,
    
    AgentDef (..),
    AgentIn (..),
    AgentOut (..),

    AgentObservable,
    
    agentIdM,
    createAgent,
    kill,
    isDead,

    agentOutFromIn,

    sendMessage,
    sendMessageTo,
    sendMessages,
    broadcastMessage,
    hasMessage,
    onMessage,
    onFilterMessage,
    onMessageFrom,
    onMessageType,

    hasConversation,
    conversation,
    conversationEnd,

    updateDomainState,
    setDomainState,

    nextAgentId,

    onStart,
    onEvent,

    recInitAllowed,
    allowsRecOthers,
    recursive,
    unrecursive,
    isRecursive,

    agentPure,
    agentPureReadEnv,
    agentPureIgnoreEnv,
    AgentPureBehaviour,
    AgentPureBehaviourReadEnv,
    AgentPureBehaviourNoEnv,

    createAgentM,
    killM,
    isDeadM,

    sendMessageM,
    sendMessageToM,
    sendMessagesM,
    broadcastMessageM,
    onMessageM,
    onMessageMState,

    conversationM,
    conversationEndM,
    conversationReplyMonadicRunner,
    conversationIgnoreEnvReplyMonadicRunner,
    conversationIgnoreReplyMonadicRunner,
    conversationIgnoreReplyMonadicRunner',
    
    bypassEnvironment,
    
    updateDomainStateM,
    getDomainStateM,
    setDomainStateM,
    domainStateFieldM,

    agentMonadic,
    agentMonadicReadEnv,
    agentMonadicIgnoreEnv,
    AgentMonadicBehaviour,
    AgentMonadicBehaviourReadEnv,
    AgentMonadicBehaviourNoEnv,
    
    drain,
    
    ignoreEnv,
    
    EventSource,
    MessageSource,

    ReactiveBehaviourIgnoreEnv,

    doOnce,
    doNothing,
    
    sendMessageOccasionallySrc,
    sendMessageOccasionally,

    constMsgReceiverSource,
    constMsgSource,
    randomNeighbourNodeMsgSource,
    randomNeighbourCellMsgSource,
    
    transitionAfter,
    transitionAfterExp,
    transitionWithUniProb,
    transitionWithExpProb,
    transitionOnEvent,
    transitionOnMessage,
    transitionOnEventWithGuard,
    transitionOnBoolState,
    
    messageEventSource,
    
    ifThenElse,
    ifThenElseM,

    agentRandom,
    agentRandomM,
    
    agentRandomRange,
    agentRandomRanges,
    agentRandomBoolProb,
    agentRandomBoolProbM,
    agentRandomSplit,
    agentRandomPick,
    agentRandomPickM,
    agentRandomPicks,
    agentRandomPicksM,
    agentRandomShuffle,
    agentRandomShuffleM,

    randomBool,
    randomBoolM,
    randomExp,
    randomExpM,
    randomShuffle,
    
    avoid,

    randomNeighbourNode,
    randomNeighbourCell,

    agentRandomNeighbourNode,
    
    EnvironmentBehaviour,
    EnvironmentMonadicBehaviour,
    EnvironmentCollapsing,

    EnvironmentWrapping (..),

    environmentMonadic,
    
    NetworkType (..),
    Network (..), -- TODO: hide data-constructor

    createNetwork,
    createEmptyNetwork,
    createNetworkWithGraph,
    constEdgeLabeler,
    unitEdgeLabeler,

    nodesOfNetwork,
    networkDegrees,
    neighbourNodes,
    neighbourEdges,
    neighbourAgentIds,
    neighbourAgentIdsM,
    neighbourLinks,
    directLinkBetween,
    directLinkBetweenM,

    randomNeighbourNode,

    Discrete2dDimension,
    Discrete2dCoord,
    Discrete2dNeighbourhood,
    Discrete2dCell,
    
    Discrete2d (..), -- TODO: hide data-constructor

    SingleOccupantCell,
    SingleOccupantDiscrete2d,
    MultiOccupantCell,
    MultiOccupantDiscrete2d,

    createDiscrete2d,
 
    envDimsDisc2dM,
    
    allCellsWithCoords,
    updateCells,
    updateCellsM,
    updateCellsWithCoords,
    updateCellsWithCoordsM,
    updateCellAt,
    changeCellAt,
    changeCellAtM,
    cellsAroundRadius,
    cellsAroundRadiusM,
    cellsAroundRect,
    cellsAt,
    cellAt,
    cellAtM,
    randomCell,
    randomCellWithinRect,

    neighbours,
    neighboursM,
    neighbourCells,
    neighbourCellsM,

    neighboursInNeumannDistance,
    neighboursInNeumannDistanceM,
    neighboursCellsInNeumannDistance,
    neighboursCellsInNeumannDistanceM,

    distanceManhattanDisc2d,
    distanceEuclideanDisc2d,
    neighbourhoodOf,
    neighbourhoodScale,
    wrapCells,
    neumann,
    moore,
    wrapNeighbourhood,
    wrapDisc2d,
    wrapDisc2dEnv,
    
    randomNeighbourCell,
    randomNeighbour,
    
    occupied,
    unoccupy,
    occupy,
    occupier,
    addOccupant,
    removeOccupant,
    hasOccupiers,
    occupiers,

    Continuous2dDimension,
    Continuous2dCoord,

    Continuous2d (..), -- TODO: hide data-constructor
    
    Continuous2dEmpty,
    
    createContinuous2d,
    
    stepTo,
    stepRandom,

    distanceManhattanCont2d,
    distanceEuclideanCont2d,

    wrapCont2d,
    wrapCont2dEnv,
    
    multCoord,
    addCoord,
    subCoord,
    vecFromCoord,
    vecLen,
    vecNorm,

    simulateAndRender,
    simulateStepsAndRender,
    
    debugAndRender,
    
    StepCallback,
    RenderFrame,

    initSimulation,
    initSimNoEnv,
    newAgentId,

    AgentDefReplicator,
    EnvironmentReplicator,

    ReplicationConfig (..),

    defaultEnvReplicator,
    defaultAgentReplicator,

    runReplications,

    UpdateStrategy (..),
    EnvironmentCollapsing,
    SimulationParams (..),

    processIOInit,
    processSteps,
    processDebug,
    
    AgentRendererDisc2d,
    AgentCellColorerDisc2d,
    AgentCoordDisc2d,
    EnvRendererDisc2d,
    EnvCellColorerDisc2d,

    renderFrameDisc2d,

    defaultEnvRendererDisc2d,
    defaultEnvColorerDisc2d,
    voidEnvRendererDisc2d,

    defaultAgentRendererDisc2d,
    defaultAgentColorerDisc2d,
    voidAgentRendererDisc2d,

    AgentRendererCont2d,
    AgentColorerCont2d,
    AgentCoordCont2d,
    EnvRendererCont2d,

    renderFrameCont2d,

    defaultEnvRendererCont2d,
    voidEnvRendererCont2d,

    defaultAgentRendererCont2d,
    defaultAgentColorerCont2d,
    voidAgentRendererCont2d,
    transformToWindow,
    
    AgentRendererNetwork,
    AgentColorerNetwork,

    renderFrameNetwork,

    defaultAgentRendererNetwork,
    defaultAgentColorerNetwork,

    cont2dToDisc2d,
    disc2dToCont2d,

    cont2dTransDisc2d,
    disc2dTransCont2d,

    Stock,
    Flow,
    SDObservable,
    SDDef,
    
    runSD,

    createStock,
    createFlow,

    flowInFrom,
    stockInFrom,
    flowOutTo,
    stockOutTo
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Utils
import FRP.FrABS.Agent.Random
import FRP.FrABS.Agent.Monad
import FRP.FrABS.Agent.Reactive
import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Environment.Continuous
import FRP.FrABS.Environment.Network
import FRP.FrABS.Environment.Definitions
import FRP.FrABS.Environment.Spatial
import FRP.FrABS.Environment.Utils
import FRP.FrABS.Simulation.Simulation
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Replication
import FRP.FrABS.Simulation.ParIteration      
import FRP.FrABS.Simulation.SeqIteration 
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Rendering.Discrete2d
import FRP.FrABS.Rendering.Continuous2d
import FRP.FrABS.Rendering.Network
import FRP.FrABS.Rendering.GlossSimulator
import FRP.FrABS.SD.Definitions

{-
------------------------------------------------------------------------------------------------------------------------
-- TODOs
------------------------------------------------------------------------------------------------------------------------
- adding a scheduling language in AgentDef?
    e.g. allows to schedule in case of specific events like the watchee construct of repast. 
    benefit: statically typed. but how to implement? dont want too tight coupling between environments, 
    should be as generic as possible. functions as parameters could solve this. only in case if sequential 
    update strategy. does this imply that an agent is then scheduled more often? yes. zombies would specify: 
    randomorder with a given dt, humans: event-driven

    simparams: introduce different scheduling orderings: e.g. ascending/descebdibg by e.g. agentid or 
        random or in order or reverse order

     but arent we then working towards a DES? does it still allow SD and continuous ABS with time-semantics? 
        if yes then we have a combination of all

- implement Graph-Renderer
- run all rendering-stuff in IO?
- AgentRenderer: Circle/Rectangle as shapes
- GlossSimulator: pass Background-color as additional parameters (use currying)

- can we fix environment the same way as e.g. dt?

- write a agentBehaviour SF which can 'freeze' the state of an agent so we don't have do drag it always in AgentIn/Out around?
    e.g. a new SF implementation: agent: agentBehaviour :: s -> SF (AgentIn, e, s) -> SF (AgentOut, e). allows to get rid of state in agentin. agentout state then simply becomes oberservable state
    -> what happens then in the case of a conversation? the receiving agent cannot change the state? We would need to run the conversation within the original agentbehaviour 
- instead of generic sequential-iteration implement specific for FrABS: this can prevent forwarding environments to all (but is this really expensive? due to haskells lazyness this should never matter)
- just output the State instead of AgentOut in simulation-SF, would save lots of memory?

- allow to be able to stop simulation when iteration.function returns True

- clean-up
    - imports: no unused imports
    - lint: must be clear of warnings
    - warnings: compilation with -w must show no warnings at all
    
- comment haskell code
------------------------------------------------------------------------------------------------------------------------
-}