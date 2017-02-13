package SIRS;

import agent.ISimulationObserver;
import utils.Cell;
import SIRS.agent.SIRSAgent;
import SIRS.agent.SIRSMsgType;
import SIRS.gui.SIRSFrontend;
import agent.Agent;
import agent.AgentSimulator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Random;

/**
 * Created by jonathan on 23/01/17.
 */
public class RunSIRS {
    private Random rng;

    private final static long RNGSEED = 42;

    public RunSIRS() {
        this.rng = new Random(RNGSEED);
    }

    public void run() {
        int rows = 200;
        int cols = 200;
        double initialInfection = 0.2;
        double dt = 1.0;
        int maxSteps = 500;

        SIRSFrontend fe = new SIRSFrontend( cols, rows );

        List<SIRSAgent> hacAgents = this.createRandomAgents(cols, rows, initialInfection);
        AgentSimulator simulator = new AgentSimulator();

        simulator.simulateWithObserver(hacAgents, null,
                dt,
                rng,
                new ISimulationObserver() {
                    private int stepCount = 0;

                    @Override
                    public boolean simulationStep(List list) {
                        fe.simulationStep(list);
                        return stepCount++ < maxSteps;
                    }
                });

    }

    private List<SIRSAgent> createRandomAgents(int cols, int rows, double initialInfection) {
        List<SIRSAgent> sirsAgents = new ArrayList<>();
        HashMap<Cell, SIRSAgent> agentsByCell = new HashMap<>();

        // NOTE: need to create them first and then set their enemies and friends because only then all available
        for (int y = 0; y < rows; ++y) {
            for (int x = 0; x < cols; ++x) {
                boolean isInfected = this.rng.nextDouble() <= initialInfection;
                SIRSAgent.SIRSState state = SIRSAgent.SIRSState.Susceptible;
                if ( isInfected)
                    state = SIRSAgent.SIRSState.Infected;

                SIRSAgent a = new SIRSAgent(state, new Cell(x, y));

                sirsAgents.add(a);
                agentsByCell.put( a.getCell(), a);
            }
        }

        for (SIRSAgent a : sirsAgents ) {
            getNeighbours(a, agentsByCell);
        }

        return sirsAgents;
    }

    private void getNeighbours(SIRSAgent a, HashMap<Cell, SIRSAgent> all) {
        List<Agent<SIRSMsgType, Void>> ns = new ArrayList<>();
        List<Cell> neighbourCells = calculateNeighbourhood(a);

        for (Cell nc : neighbourCells) {
            SIRSAgent neighbour = all.get( nc );
            if ( null != neighbour ) {
                ns.add( neighbour );
            }
        }

        a.setNeighbours( ns );
    }

    private List<Cell> calculateNeighbourhood(SIRSAgent a) {
        List<Cell> n = new ArrayList<>();
        int x = a.getCell().getX();
        int y = a.getCell().getY();

        n.add( new Cell( x - 1, y - 1 ) );
        n.add( new Cell( x, y - 1) );
        n.add( new Cell( x + 1, y - 1) );

        n.add( new Cell( x - 1, y ) );
        n.add( new Cell( x + 1, y ) );

        n.add( new Cell( x - 1, y + 1 ) );
        n.add( new Cell( x, y + 1) );
        n.add( new Cell( x + 1, y + 1) );

        return n;
    }
}
