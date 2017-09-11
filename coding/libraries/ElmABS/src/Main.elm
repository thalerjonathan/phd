import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict
import Task
import TimeTravel.Html as TimeTravel

{--}
main : Program Never Simulation Msg
main = 
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
--}

{--
-- main : Program Never (TimeTravel.Internal.Model.Model Simulation Msg) (TimeTravel.Html.Msg Msg)
main =
    TimeTravel.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
--}

-- MODEL
gridSize : (Int, Int)
gridSize = (10, 10)

agentSize : Int
agentSize = 10

dt : SimTime
dt = 1.0

type alias SimTime = Float

type SIR = Susceptible | Infected | Recovered
type Message = Contact SIR

type alias AgentMessage = (AgentId, Message)

type alias AgentId = Int
type alias Position = (Int, Int)

type alias AgentAction = (AgentIn -> Neighbourhood -> AgentCont)
type alias Continuation = (AgentOut, AgentAction)

type alias Neighbourhood = Dict.Dict AgentId (List AgentId)

type AgentCont = 
    Terminate
    | Continue Continuation

type alias AgentState =
    { aid: AgentId
    , sir: SIR
    , pos: Position }

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

                agents = List.map (\(aid, coord) -> { aid = aid, sir = Susceptible, pos = coord}) (List.map2 (,) aids coords)
            in
                agents

        agentConts = List.foldr (\s acc -> Dict.insert s.aid (agentStateToCont s) acc) Dict.empty agentStates

        agentStateToCont : AgentState -> Continuation
        agentStateToCont s = ({ observable = agentStateToObservable s, msgs = []}, agentBehaviour s)
    in
        { time = 0.0
        , agents = agentConts
        , env = Dict.empty }

init : (Simulation, Cmd Msg)
init =
    (createSimulation, nextStep)

agentStateToObservable : AgentState -> AgentObservable
agentStateToObservable s = { sir = s.sir, pos = s.pos }

agentTask : AgentId -> AgentIn -> Neighbourhood -> AgentAction -> Task.Task Never (AgentId, AgentCont)
agentTask aid ain env af = Task.succeed (aid, af ain env)

agentBehaviour : AgentState -> AgentAction
agentBehaviour s ain env =
    case s.sir of
        Susceptible -> susceptibleBehaviour s ain env
        Infected -> susceptibleBehaviour s ain env
        Recovered -> susceptibleBehaviour s ain env

susceptibleBehaviour : AgentState -> AgentAction
susceptibleBehaviour s0 ain env = 
    let
        s1 = onMessage (Contact Infected) ain (\s -> { s | sir = Infected }) s0
        obs = agentStateToObservable s1
        
        ao0 = { observable = obs, msgs = [] }
        ao1 = contactRandomNeighbour env ao0
    in
        Continue (ao1, (agentBehaviour s1))

contactRandomNeighbour : Neighbourhood -> AgentOut -> AgentOut
contactRandomNeighbour env ao = ao

sendMessage : AgentMessage -> AgentOut -> AgentOut
sendMessage msg ao = { ao | msgs = msg :: ao.msgs }

onMessage : Message -> AgentIn -> (AgentState -> AgentState) -> AgentState -> AgentState
onMessage m ain f sInit = 
    let
        msgs = ain.msgs
        s1 = List.foldr (\(_, msg) s0 -> 
            if m == msg then 
                f s0
                else
                    s0) sInit msgs
    in
        s1

-- UPDATE
nextStep : Cmd Msg
nextStep = Task.perform (\_ -> Step dt) (Task.succeed ())

update : Msg -> Simulation -> (Simulation, Cmd Msg)
update msg model =
  case msg of
    Step dt ->
        ({ model | time = model.time + dt }, executeAgentsSequential model)

    AgentsUpdated acs ->
        let
            model0 = updateModel acs model
            cmd = 
                if Dict.isEmpty model0.agents then
                    Cmd.none
                    else
                        nextStep
        in
            (model0, cmd)

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
        ats = List.foldr (\(aid, (_, act)) acc -> (agentTask aid (agentIn aid model) model.env act) :: acc) [] (Dict.toList model.agents)

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

-- SUBSCRIPTIONS
subscriptions : Simulation -> Sub Msg
subscriptions model = Sub.none

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
        g [] [
            rect [ x xPos , y yPos, width w, height h, fill c ] []
            , Svg.text_ [ x xPosTxt, y yPosTxt, fontSize fs, fill "white" ] [ Svg.text txt ] ]

sirColor : SIR -> String
sirColor sir =
    case sir of
        Susceptible -> "blue"
        Infected -> "red"
        Recovered -> "green"