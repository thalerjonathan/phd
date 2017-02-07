package SGEnv;

import SGEnv.agent.SGAgent;
import SGEnv.gui.SGFrontend;
import agent.Agent;
import agent.AgentSimulator;
import agent.Message;
import utils.Cell;
import utils.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;

/**
 * Created by jonathan on 23/01/17.
 */
public class RunSGEnv {
    private Random rng;

    private final static long RNGSEED = 42;

    public RunSGEnv() {
        this.rng = new Random(RNGSEED);
    }

    public void run() throws ExecutionException, InterruptedException {
        int rows = 99;
        int cols = 99;
        double dt = 1.0;

        SGFrontend fe = new SGFrontend( cols, rows );

        List<SGAgent> sgAgents = this.createCoopsWithOneDefectorAgents(cols, rows);
        HashMap<Integer, Pair<Double, SGAgent.SGState>> env = this.createEnvironmentFromAgents(sgAgents);

        AgentSimulator simulator = new AgentSimulator();

        simulator.simulateWithObserver( sgAgents,
                env,
                dt,
                fe);

    }

    private HashMap<Integer, Pair<Double, SGAgent.SGState>> createEnvironmentFromAgents(List<SGAgent> sgAgents) {
        HashMap<Integer, Pair<Double, SGAgent.SGState>> env = new HashMap<>();

        for (SGAgent a : sgAgents ) {
            env.put(a.getId(), new Pair<>(0.0, a.getCurrState()));
        }

        return env;
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
            List<Agent<Message.NoMsg, Pair<Double, SGAgent.SGState>>> ns = getNeighbours(a, sgAgents);

            a.setNeighbours( ns );
        }

        return sgAgents;
    }

    private List<Agent<Message.NoMsg, Pair<Double, SGAgent.SGState>>> getNeighbours(SGAgent a, List<SGAgent> all) {
        List<Agent<Message.NoMsg, Pair<Double, SGAgent.SGState>>> neighbours = new ArrayList<>();
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
        n.add( new Cell( x, y ) );
        n.add( new Cell( x + 1, y ) );

        n.add( new Cell( x - 1, y + 1 ) );
        n.add( new Cell( x, y + 1) );
        n.add( new Cell( x + 1, y + 1) );

        return n;
    }
}
