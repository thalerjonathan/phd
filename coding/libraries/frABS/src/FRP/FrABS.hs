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
    agentPureReadEnv,
    agentPureIgnoreEnv,
    AgentPureBehaviour,
    AgentPureBehaviourReadEnv,
    AgentPureBehaviourNoEnv,
    
    -- agentIdM,
    -- environmentM,
    -- environmentPositionM,
    -- changeEnvironmentPositionM,

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

    -- runEnvironmentM,

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

    pickRandomNeighbourNode,
    
    pickRandomNeighbourCell,

    EnvironmentBehaviour,
    EnvironmentCollapsing,

    EnvironmentWrapping (..),

    NetworkType (..),
    Network (..), -- TODO: hide data-constructor

    createNetwork,
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

    pickRandomNeighbourNode,

        Discrete2dDimension,
    Discrete2dCoord,
    Discrete2dNeighbourhood,

    Discrete2d (..), -- TODO: hide data-constructor

    createDiscrete2d,
 
    allCellsWithCoords,
    updateCells,
    updateCellsWithCoords,
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
    neighbourCellsInDistance,
    neighbourCellsInDistanceM,
    neighbourCells,
    neighbourCellsM,
    neighbourInDistance,
    neighbourInDistanceM,
    neighbours,
    neighboursM,

    distanceManhattanDisc2d,
    distanceEuclideanDisc2d,
    neighbourhoodOf,
    neighbourhoodScale,
    wrapCells,
    neumann,
    neumannSelf,
    moore,
    mooreSelf,
    wrapNeighbourhood,
    wrapDisc2d,

    pickRandomNeighbourCell,
    pickRandomNeighbour,
    
    Continuous2DDimension,
    Continuous2DCoord,

    Continuous2d (..), -- TODO: hide data-constructor
    -- Continuous2dNetwork (..),
    
    createContinuous2d,
    
    environmentDimensionsCont2D,

    distanceManhattanCont2D,
    distanceEuclideanCont2D,

    wrapCont2D,

    simulateAndRender,
    simulateStepsAndRender,
    
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

    AgentRendererDisc2d,
    AgentCellColorer,
    AgentCoordDisc2d,
    EnvDisc2dRenderer,
    EnvDisc2dCellColorer,

    render2dDiscreteFrame,

    defaultEnvironmentRenderer,
    defaultEnvironmentColorer,
    voidEnvironmentRenderer,

    defaultAgentRenderer,
    defaultAgentColorer,
    voidAgentRenderer
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
import FRP.FrABS.Simulation.Simulation
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Replication
import FRP.FrABS.Simulation.ParIteration      
import FRP.FrABS.Simulation.SeqIteration 
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Rendering.Discrete2D
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