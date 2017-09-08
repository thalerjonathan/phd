import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Dict

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

type alias AgentId = Int
type alias Position = (Int, Int)

type alias Agent =
    { id: AgentId
    , state: SIR
    , t: Float
    , pos: Position }

type alias Simulation =
    { time : Float
    , agents : List Agent
    , neighbourhood : Dict.Dict AgentId (List AgentId) }

createSimulation : Simulation
createSimulation = 
    { time = 0.0
    , agents = []
    , neighbourhood = Dict.empty }

init : (Simulation, Cmd Msg)
init =
    (createSimulation, Cmd.none)

-- UPDATE
type Msg = Tick Time

update : Msg -> Simulation -> (Simulation, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Simulation -> Sub Msg
subscriptions model =
    Time.every second Tick

-- VIEW
view : Simulation -> Html Msg
view {time, agents, neighbourhood} =
  let
    angle =
      turns (Time.inMinutes time)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    svg [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
      ]