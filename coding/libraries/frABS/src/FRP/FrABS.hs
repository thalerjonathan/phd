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

    createAgent,
    kill,
    isDead,

    -- createStartingAgentIn,
    agentOutFromIn,
    -- startingAgentInFromAgentDef,

    sendMessage,
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

    -- mergeMessages,

    agentPure,

    agentIdM,
    environmentM,
    environmentPositionM,
    changeEnvironmentPositionM,

    createAgentM,
    killM,
    isDeadM,

    sendMessageM,
    sendMessagesM,
    broadcastMessageM,
    onMessageM,
    onMessageMState,

    conversationM,
    conversationEndM,
    conversationReplyMonadicRunner,
    conversationIgnoreReplyMonadicRunner,

    updateDomainStateM,
    getDomainStateM,
    setDomainStateM,
    domainStateFieldM,

    runEnvironmentM,

    agentMonadic,

    EventSource,
    MessageSource,

    doOnce,
    doNothing,
    
    sendMessageOccasionallySrc,
    sendMessageOccasionally,

    constMsgReceiverSource,
    constMsgSource,
    randomNeighbourNodeMsgSource,
    randomNeighbourCellMsgSource,
    
    transitionAfter,
    transitionWithUniProb,
    transitionWithExpProb,
    transitionOnEvent,
    transitionOnMessage,
    transitionOnEventWithGuard,
    transitionOnBoolState,
    
    messageEventSource,
    
    ifThenElse,
    ifThenElseM,

    runAgentRandom,
    runAgentRandomM,
    
    drawRandomRangeFromAgent,
    drawMultipleRandomRangeFromAgent,
    drawBoolWithProbFromAgent,
    drawBoolWithProbFromAgentM,
    splitRandomFromAgent,
    agentPickRandom,
    agentPickRandomM,
    agentPickRandomMultiple,
    agentPickRandomMultipleM,

    drawRandomBool,
    drawRandomBoolM,
    drawRandomExponential,
    drawRandomExponentialM,

    neighbourCells,
    neighbourCellsWithCoords,

    pickRandomNeighbourNode,
    pickRandomNeighbourNodeM,
    
    pickRandomNeighbourCell,
    pickRandomNeighbourCellM,

    agentRandomMove,
    agentRandomMoveM,

    agentCellOnPos,
    agentCellOnPosM,

    EnvironmentBehaviour,
    EnvCoord,
    EnvLimits,
    EnvNeighbourhood,
    EnvWrapping (..),
    EnvGraph,

    Environment (..),

    createEnvironment,

    neighbourEdges,
    neighbourNodes,
    neighbourNodesM,
    neighbourLinks,
    directLinkBetween,
    directLinkBetweenM,

    allCellsWithCoords,

    updateEnvironmentCells,
    updateEnvironmentCellsWithCoords,
    changeCellAt,
    changeCellAtM,

    distanceManhattan,
    distanceEuclidean,

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
    neighboursDistance,
    neighboursDistanceM,
    neighbourhoodOf,
    neighbourhoodScale,
   
    neumann,
    neumannSelf,
    moore,
    mooreSelf,

    wrapCells,

    NetworkType (..),

    createAgentNetwork,
    nodesOfNetwork,
    networkDegrees,

    RenderCellCoord,
    RenderCellColor,
    RenderCell(..),
    display,
    render2DDiscreteFrame,

    simulateAndRender,
    simulateStepsAndRender,
    
    StepCallback,
    RenderFrame,

    initSimulation,
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
    processSteps
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Utils
import FRP.FrABS.Agent.Random
import FRP.FrABS.Agent.Monad
import FRP.FrABS.Agent.Reactive
import FRP.FrABS.Env.Environment
import FRP.FrABS.Env.Network
import FRP.FrABS.Simulation.Simulation
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Replication
import FRP.FrABS.Simulation.ParIteration      
import FRP.FrABS.Simulation.SeqIteration 
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Rendering.Agents2DDiscrete
import FRP.FrABS.Rendering.GlossSimulator

------------------------------------------------------------------------------------------------------------------------
-- TODOs
------------------------------------------------------------------------------------------------------------------------
-- study arrowized programming (papers): how can dt disappear ? can we ommit arguments which are implicitly there?
-- develop arrowized EDSL for ABS: timeout transitions, rate transitions, sending messages after, repeatedly send message in interval, occasionally send message

-- TODO create project structure according to put it on Hackage in september: tests, comments,...
-- TODO write unit-tests
-- TODO write QuickCheck tests

-- Monadic Conversations

-- Use Dunai to run State Monad

-- TODO: allow to be able to stop simulation when iteration.function returns True

-- TODO: hide AgentIn and AgentOut same way as DTime is hidden, only generic state in/out
-- TODO  can we somehow restrict access to agentin/out? hiding dataconstructors? making them completely opaque?

-- TODO STM FrABS using Dunai?
------------------------------------------------------------------------------------------------------------------------