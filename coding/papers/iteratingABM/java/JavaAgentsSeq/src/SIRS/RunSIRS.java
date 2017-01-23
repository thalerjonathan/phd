package SIRS;

import utils.Cell;
import SIRS.agent.SIRSAgent;
import SIRS.agent.SIRSMsgType;
import SIRS.gui.SIRSFrontend;
import agent.Agent;
import agent.AgentSimulator;

import java.util.ArrayList;
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
        double initialInfection = 0.1;
        double dt = 1.0;

        SIRSFrontend fe = new SIRSFrontend( cols, rows );

        List<SIRSAgent> hacAgents = this.createRandomAgents(cols, rows, initialInfection);
        AgentSimulator simulator = new AgentSimulator();

        simulator.simulateWithObserver( hacAgents, null,
                dt,
                rng,
                fe);

    }

    private List<SIRSAgent> createRandomAgents(int cols, int rows, double initialInfection) {
        List<SIRSAgent> sirsAgents = new ArrayList<>();

        // NOTE: need to create them first and then set their enemies and friends because only then all available
        for (int y = 0; y < rows; ++y) {
            for (int x = 0; x < cols; ++x) {
                boolean isInfected = this.rng.nextDouble() <= initialInfection;
                SIRSAgent.SIRSState state = SIRSAgent.SIRSState.Susceptible;
                if ( isInfected)
                    state = SIRSAgent.SIRSState.Infected;

                SIRSAgent a = new SIRSAgent(state, new Cell(x, y));

                sirsAgents.add(a);
            }
        }

        for (int i = 0; i < sirsAgents.size(); ++i) {
            SIRSAgent a = sirsAgents.get( i );
            List<Agent<SIRSMsgType, Void>> ns = getNeighbours(a, sirsAgents);

            a.setNeighbours( ns );
        }

        return sirsAgents;
    }

    private List<Agent<SIRSMsgType, Void>> getNeighbours(SIRSAgent a, List<SIRSAgent> all) {
        List<Agent<SIRSMsgType, Void>> neighbours = new ArrayList<>();
        List<Cell> nCells = calculateNeighbourhood(a);

        for (SIRSAgent n : all) {
            if ( nCells.contains( n.getCell() ) ) {
                neighbours.add(n);

                if (neighbours.size() == nCells.size()) {
                    break;
                }
            }
        }

        return neighbours;
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
