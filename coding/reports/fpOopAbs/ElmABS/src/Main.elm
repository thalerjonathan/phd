import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict
import Task
import TimeTravel.Html as TimeTravel
import Random

{--}
main : Program Never Simulation Msg
main = 
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
--}

{--
-- main : Program Never (TimeTravel.Internal.Model.Model Simulation Msg) (TimeTravel.Html.Msg Msg)
main =
    TimeTravel.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
--}

-- MODEL
gridSize : (Int, Int)
gridSize = (15, 15)

halfGridSize : (Int, Int)
halfGridSize = (7, 7)

agentSize : Int
agentSize = 20

illnessDuration : Float
illnessDuration = 15

infectionProb : Float
infectionProb = 0.1

dt : SimTime
dt = 1.0

initialSeed : Int
initialSeed = 42

type alias SimTime = Float

type SIR = Susceptible | Infected | Recovered
type Message = Contact SIR

type alias AgentMessage = (AgentId, Message)

type alias AgentId = Int
type alias Position = (Int, Int)

type alias AgentAction = (SimTime -> AgentIn -> Neighbourhood -> AgentCont)
type alias Continuation = (AgentOut, AgentAction)

type alias Neighbourhood = Dict.Dict AgentId (List AgentId)

type AgentCont = 
    Terminate
    | Continue Continuation

type alias AgentState =
    { aid: AgentId
    , sir: SIR
    , sirTime: SimTime
    , pos: Position
    , seed: Random.Seed }

type alias AgentObservable =
    { sir: SIR
    , pos: Position }

type alias AgentIn =
    { msgs: List AgentMessage }

type alias AgentOut =
    { observable: AgentObservable
    , msgs: List AgentMessage }

type alias Simulation =
    { time : SimTime
    , agents : Dict.Dict AgentId Continuation
    , env : Neighbourhood }

type Msg = 
    Step SimTime
    | AgentsUpdated (List (AgentId, AgentCont))

createSimulation : Simulation
createSimulation = 
    let 
        agentStates = createAgentStates gridSize

        createAgentStates : (Int, Int) -> List AgentState
        createAgentStates (dx, dy) =
            let
                xs = List.range 0 (dx - 1)
                ys = List.range 0 (dy - 1)
                aids = List.range 0 (dx * dy - 1)
                coords = List.foldr (\x acc -> List.append (List.map (\y -> (x, y)) ys) acc) [] xs 

                agents = List.map (\(aid, coord) -> agentState aid Susceptible coord ) (List.map2 (,) aids coords)

                agentState : AgentId -> SIR -> Position -> AgentState
                agentState aid sir coord =
                    let 
                        (sir0, st) = 
                            if coord == halfGridSize then
                                (Infected, illnessDuration)
                                else
                                    (sir, 0)

                        seed = Random.initialSeed (initialSeed + aid)
                    in
                        { aid = aid, sir = sir0, sirTime = st, pos = coord, seed = seed }
            in
                agents

        envFromStates ss = 
            let
                coordsToAid = List.foldr (\s acc -> Dict.insert s.pos s.aid acc) Dict.empty ss

                neighboursOf : Position -> List AgentId
                neighboursOf (x, y) =
                    let
                        neighbourhood = [(0, -1), (1, 0), (0, 1), (-1, 0)]
                        neighbourCoords = List.map (\(nx, ny) -> (x + nx, y + ny)) neighbourhood

                        addNeighbour : Position -> List AgentId -> List AgentId
                        addNeighbour c acc = 
                            let
                                mayAid = Dict.get c coordsToAid
                            in
                                case mayAid of
                                    Nothing -> acc
                                    Just aid -> aid :: acc
                    in
                        List.foldr addNeighbour [] neighbourCoords

            in
                List.foldr (\s acc -> Dict.insert s.aid (neighboursOf s.pos) acc) Dict.empty ss

        agentConts = List.foldr (\s acc -> Dict.insert s.aid (agentStateToCont s) acc) Dict.empty agentStates

        agentStateToCont : AgentState -> Continuation
        agentStateToCont s = ({ observable = agentStateToObservable s, msgs = []}, agentBehaviour s)
    in
        { time = 0.0
        , agents = agentConts
        , env = envFromStates agentStates }

init : (Simulation, Cmd Msg)
init = (createSimulation, nextStep)

agentStateToObservable : AgentState -> AgentObservable
agentStateToObservable s = { sir = s.sir, pos = s.pos }

agentTask : AgentId -> SimTime -> AgentIn -> Neighbourhood -> AgentAction -> Task.Task Never (AgentId, AgentCont)
agentTask aid t ain env af = Task.succeed (aid, af t ain env)

agentOutDefault : AgentState -> AgentOut
agentOutDefault s =
    let 
        obs = agentStateToObservable s
    in
        { observable = obs, msgs = [] }

agentOutUpdateObs : AgentState -> AgentOut -> AgentOut
agentOutUpdateObs s ao = 
    let 
        obs = agentStateToObservable s
    in
        { ao | observable = obs }

agentBehaviour : AgentState -> AgentAction
agentBehaviour s t ain env =
    case s.sir of
        Susceptible -> susceptibleBehaviour s t ain env
        Infected -> infectedBehaviour s t ain env
        Recovered -> recoveredBehaviour s t

susceptibleBehaviour : AgentState -> AgentAction
susceptibleBehaviour s t ain env = 
    let
        ao = agentOutDefault s
        s0 = onMessage (Contact Infected) ain (infect t) s
        (ao0, s1) = contactRandomNeighbour (Contact Susceptible) env ao s0
        ao1 = agentOutUpdateObs s1 ao0
    in
        Continue (ao1, (agentBehaviour s1))

infectedBehaviour : AgentState -> AgentAction
infectedBehaviour s t ain env = 
    let
        ao = agentOutDefault s
        ao0 = replyToMessage (Contact Susceptible) (Contact Infected) ain ao
        (ao1, s0) = contactRandomNeighbour (Contact Infected) env ao0 s

        s1 = 
            if t >= s0.sirTime then
                { s0 | sir = Recovered }
                else
                    s0

        ao2 = agentOutUpdateObs s1 ao1
    in
        Continue (ao2, (agentBehaviour s1))

recoveredBehaviour : AgentState -> SimTime -> AgentCont
recoveredBehaviour s t = 
    let
        ao = agentOutDefault s
    in
        Continue (ao, (agentBehaviour s))

infect : SimTime -> AgentState -> AgentState
infect t s =
    let
        (randProb, seed0) = Random.step (Random.float 0.0 1.0) s.seed
        s0 = { s | seed = seed0 }
    in
        if infectionProb >= randProb then
            { s0 | sir = Infected, sirTime = t + illnessDuration }
            else
                s0

contactRandomNeighbour : Message -> Neighbourhood -> AgentOut -> AgentState -> (AgentOut, AgentState)
contactRandomNeighbour msg env ao s =
    let
        mayNs = Dict.get s.aid env
    in
        case mayNs of
            Nothing -> (ao, s)
            Just ns -> 
                let
                    nsCount = List.length ns
                    (randIdx, seed0) = Random.step (Random.int 0 (nsCount - 1)) s.seed
                    mayReceiverId = List.head (List.drop randIdx ns)
                    s0 = { s | seed = seed0 }  
                in
                    case mayReceiverId of
                        Nothing -> (ao, s0)
                        Just receiverId -> 
                            let
                                ao0 = sendMessage (receiverId, msg) ao
                            in
                                (ao0, s0)

replyToMessage : Message -> Message -> AgentIn -> AgentOut -> AgentOut
replyToMessage incomingMsg replyMsg ain ao = 
    List.foldr (\(senderId, msg) ao0 -> 
        if incomingMsg == msg then 
            sendMessage (senderId, replyMsg) ao
            else
                ao0) ao ain.msgs

sendMessage : AgentMessage -> AgentOut -> AgentOut
sendMessage msg ao = { ao | msgs = msg :: ao.msgs }

onMessage : Message -> AgentIn -> (acc -> acc) -> acc -> acc
onMessage m ain f accInit = 
    List.foldr (\(_, msg) acc0 -> 
        if m == msg then 
            f acc0
            else
                acc0) accInit ain.msgs

-- UPDATE
nextStep : Cmd Msg
nextStep = Task.perform (\_ -> Step dt) (Task.succeed ())

update : Msg -> Simulation -> (Simulation, Cmd Msg)
update msg model =
  case msg of
    Step dt ->
        let
            model0 = { model | time = model.time + dt }
        in
            (model0, executeAgentsSequential model0)

    AgentsUpdated acs ->
        let
            model0 = updateModel acs model
            cmd = 
                if stopCondition model0 then
                    Cmd.none
                    else
                        nextStep
        in
            (model0, cmd)

stopCondition : Simulation -> Bool
stopCondition model = 
    let
        noAgents = Dict.isEmpty model.agents
        noInfected = Dict.foldr (\_ (ao, _) b -> b && (ao.observable.sir /= Infected)) True model.agents
    in
        noAgents || noInfected

updateModel : List (AgentId, AgentCont) -> Simulation -> Simulation
updateModel acs model =
    let 
        agents0 = model.agents
        agents1 = List.foldr updateAgent agents0 acs

        updateAgent : (AgentId, AgentCont) -> Dict.Dict AgentId Continuation -> Dict.Dict AgentId Continuation
        updateAgent (aid, ac) acc =
            case ac of
                Terminate -> Dict.remove aid acc
                Continue cont -> Dict.insert aid cont acc
    in
        { model | agents = agents1 }

executeAgentsSequential : Simulation -> Cmd Msg
executeAgentsSequential model = 
    let
        time = model.time
        ats = List.foldr (\(aid, (_, act)) acc -> (agentTask aid time (agentIn aid model) model.env act) :: acc) [] (Dict.toList model.agents)

        ts = Task.sequence ats

        taskFunc acs = AgentsUpdated acs
    in
        Task.perform taskFunc ts

agentIn : AgentId -> Simulation -> AgentIn
agentIn aid model =
    let
        msgs = List.foldr (\(senderId, sender) acc -> collectFrom senderId (Tuple.first sender) acc) [] (Dict.toList model.agents)

        collectFrom : AgentId -> AgentOut -> List (AgentId, Message) -> List (AgentId, Message)
        collectFrom senderId ao acc = 
            let
                filteredMessages = List.filter (\(receiverId, _) -> receiverId == aid) ao.msgs
                ms = List.map (\(_, m) -> (senderId, m)) filteredMessages
            in
                List.append ms acc
    in
        { msgs = msgs }

-- VIEW
view : Simulation -> Html Msg
view { time, agents, env } =
  let
    agentsSvg = Dict.foldr agentRender [] agents 
    agentRender k cont acc = (renderAgent <| (.observable << Tuple.first) cont) :: acc
  in
    svg [ viewBox "0 0 500 500", width "800px" ] agentsSvg

renderAgent : AgentObservable -> Svg.Svg msg
renderAgent { sir, pos } = 
    let
        xc = agentSize * Tuple.first pos 

        yc = agentSize * Tuple.second pos

        xcTxt = xc

        ycTxt = agentSize + yc

        xPos = 
            toString xc

        yPos = 
            toString yc

        xPosTxt = 
            toString xcTxt

        yPosTxt = 
            toString ycTxt

        w = 
            toString agentSize

        h = 
            toString agentSize

        c = 
            sirColor sir

        txt = "0"

        fs = 
            toString agentSize
    in 
        -- g [] [
        --    rect [ x xPos , y yPos, width w, height h, fill c ] []
        --    , Svg.text_ [ x xPosTxt, y yPosTxt, fontSize fs, fill "white" ] [ Svg.text txt ] ]
        rect [ x xPos , y yPos, width w, height h, fill c ] []

sirColor : SIR -> String
sirColor sir =
    case sir of
        Susceptible -> "blue"
        Infected -> "red"
        Recovered -> "green"