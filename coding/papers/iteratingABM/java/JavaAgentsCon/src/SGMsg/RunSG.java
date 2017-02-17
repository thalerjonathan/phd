package SGMsg;

import SGMsg.agent.SGAgent;
import SGMsg.agent.SGMsgType;
import SGMsg.gui.SGFrontend;
import SIRS.gui.SIRSFrontend;
import agent.Agent;
import agent.AgentSimulator;
import agent.ISimulationObserver;
import utils.Cell;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;

/**
 * Created by jonathan on 23/01/17.
 */
public class RunSG {
    private Random rng;

    private final static long RNGSEED = 42;

    public RunSG() {
        this.rng = new Random(RNGSEED);
    }

    public void run() throws ExecutionException, InterruptedException, CloneNotSupportedException {
        int rows = 99;
        int cols = 99;
        int sleep = 0;
        double dt = 1.0;
        int steps = 2 * 217;

        SGFrontend fe = new SGFrontend( cols, rows, sleep );

        List<SGAgent> hacAgents = this.createCoopsWithOneDefectorAgents(cols, rows);

        AgentSimulator simulator = new AgentSimulator();

        simulator.simulateWithObserver(hacAgents, null,
                dt,
                new ISimulationObserver() {
                    private int stepCounter = 0;

                    @Override
                    public boolean simulationStep(LinkedHashMap as) {
                        fe.simulationStep(as);
                        return stepCounter++ < steps;
                    }
                });

    }

    private List<SGAgent> createCoopsWithOneDefectorAgents(int cols, int rows) {
        List<SGAgent> sgAgents = new ArrayList<>();

        int halfCols = (int) (cols / 2.0);
        int halfRows = (int) (rows / 2.0);

        // NOTE: need to create them first and then set their enemies and friends because only then all available
        for (int y = 0; y < rows; ++y) {
            for (int x = 0; x < cols; ++x) {
                SGAgent a;

                if ( x == halfCols && y == halfRows)
                    a = new SGAgent(SGAgent.SGState.Defector, new Cell(x, y));
                else
                    a = new SGAgent(SGAgent.SGState.Cooperator, new Cell(x, y));

                sgAgents.add(a);
            }
        }

        for (int i = 0; i < sgAgents.size(); ++i) {
            SGAgent a = sgAgents.get( i );
            List<Agent<SGMsgType, Void>> ns = getNeighbours(a, sgAgents);

            a.setNeighbours( ns );
        }

        return sgAgents;
    }

    private List<Agent<SGMsgType, Void>> getNeighbours(SGAgent a, List<SGAgent> all) {
        List<Agent<SGMsgType, Void>> neighbours = new ArrayList<>();
        List<Cell> nCells = calculateNeighbourhood(a);

        for (SGAgent n : all) {
            if ( nCells.contains( n.getCell() ) ) {
                neighbours.add(n);

                if (neighbours.size() == nCells.size()) {
                    break;
                }
            }
        }

        return neighbours;
    }

    private List<Cell> calculateNeighbourhood(SGAgent a) {
        List<Cell> n = new ArrayList<>();
        int x = a.getCell().getX();
        int y = a.getCell().getY();

        n.add( new Cell( x - 1, y - 1 ) );
        n.add( new Cell( x, y - 1) );
        n.add( new Cell( x + 1, y - 1) );

        n.add( new Cell( x - 1, y ) );
        n.add( new Cell( x , y ) );
        n.add( new Cell( x + 1, y ) );

        n.add( new Cell( x - 1, y + 1 ) );
        n.add( new Cell( x, y + 1) );
        n.add( new Cell( x + 1, y + 1) );

        return n;
    }
}
