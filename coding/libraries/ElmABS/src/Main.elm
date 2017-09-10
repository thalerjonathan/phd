import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Dict
import Task

main : Program Never Simulation Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type SIR = Susceptible | Infected | Recovered
type Message = Infect

type alias AgentId = Int
type alias Position = (Int, Int)

type alias AgentState =
    { aid: AgentId
    , sir: SIR
    , counter: Int
    , pos: Position }

type alias AgentIn =
    { msgs: List (AgentId, Message) }

type alias AgentOut =
    { observable: AgentState
    , msgs: List (AgentId, Message) }

type alias Simulation =
    { time : Float
    , agents : Dict.Dict AgentId AgentCont
    , neighbourhood : Dict.Dict AgentId (List AgentId) }

createSimulation : Simulation
createSimulation = 
    let 
        a0 = { aid = 0, counter = 0, sir = Susceptible, pos = (0, 0)}
        a1 = { aid = 1, counter = 0, sir = Susceptible, pos = (1, 0)}
        a2 = { aid = 2, counter = 0, sir = Susceptible, pos = (2, 0)}
        a3 = { aid = 3, counter = 0, sir = Susceptible, pos = (0, 1)}
        a4 = { aid = 4, counter = 0, sir = Infected, pos = (1, 1)}
        a5 = { aid = 5, counter = 0, sir = Susceptible, pos = (2, 1)}
        a6 = { aid = 6, counter = 0, sir = Susceptible, pos = (0, 2)}
        a7 = { aid = 7, counter = 0, sir = Susceptible, pos = (1, 2)}
        a8 = { aid = 8, counter = 0, sir = Susceptible, pos = (2, 2)}
        allAgents = [a0, a1, a2, a3, a4, a5, a6, a7, a8]

        agentConts = Dict.empty
    in
        { time = 0.0
        , agents = 
        , neighbourhood = Dict.empty }

init : (Simulation, Cmd Msg)
init =
    (createSimulation, Cmd.none)


type alias AgentAction = (AgentIn -> AgentCont)
type alias Continuation = (AgentOut, AgentAction)

type AgentCont = 
    Terminate
    | Continue Continuation

agentBehaviour : AgentState -> AgentAction
agentBehaviour s0 ain = 
    let 
        s1 = { s0 | counter = s0.counter + 1 }
        ao = { observable = s1, msgs = [] }

    in
        if s1.counter > 42 then 
            Terminate
            else
                Continue (ao, (agentBehaviour s1))

agentTask : AgentIn -> AgentAction -> Task.Task Never AgentCont
agentTask ain af = Task.succeed (af ain)

agentPerform : AgentId -> Task.Task Never AgentCont -> Cmd Msg
agentPerform aid agentTask = 
    let
        contFunc ret = 
            case ret of
                Terminate ->
                    AgentTerminated aid

                Continue cont -> 
                    AgentUpdate (aid, cont)
    
    in
        Task.perform contFunc agentTask

-- UPDATE
type Msg = 
    Tick Time
    | AgentTerminated AgentId
    | AgentUpdate (AgentId, Continuation)

update : Msg -> Simulation -> (Simulation, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)

    AgentTerminated aid ->
        (model, Cmd.none)

    AgentUpdate (aid, cont) ->
        (model, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Simulation -> Sub Msg
subscriptions model =
    Time.every second Tick

-- VIEW
view : Simulation -> Html Msg
view { time, agents, neighbourhood } =
  let
    agentsSvg = List.map renderAgent (Dict.values agents)

  in
    svg [ viewBox "0 0 100 100", width "300px" ] agentsSvg

renderAgent : AgentState -> Svg.Svg msg
renderAgent { sir, counter, pos } = 
    let
        xc = 10 * Tuple.first pos 

        yc = 10 * Tuple.second pos

        xcTxt = xc

        ycTxt = 10 + yc

        xPos = 
            toString xc

        yPos = 
            toString yc

        xPosTxt = 
            toString xcTxt

        yPosTxt = 
            toString ycTxt

        w = "10"

        h = "10"

        c = 
            sirColor sir

        txt =
            toString counter
    in 
        g [] [
            rect [ x xPos , y yPos, width w, height h, fill c ] []
            , Svg.text_ [ x xPosTxt, y yPosTxt, fontSize "10", fill "white" ] [ Svg.text txt ] ]

sirColor : SIR -> String
sirColor sir =
    case sir of
        Susceptible -> "blue"
        Infected -> "red"
        Recovered -> "green"