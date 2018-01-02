package HAC;

import HAC.agent.HACAgent;
import HAC.agent.Vector;
import HAC.gui.HACFrontend;
import agent.AgentSimulator;
import agent.ISimulationObserver;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;

/**
 * Created by jonathan on 20/01/17.
 */
public class RunHAC {

    private Random rng;

    private final static long RNGSEED = 42;

    public RunHAC() {
        this.rng = new Random(RNGSEED);
    }

    public void run() throws InterruptedException, CloneNotSupportedException, ExecutionException {
        int agentCount = 50_000;
        double herosDist = 0.25;
        double dt = 0.01;

        HACFrontend fe = new HACFrontend( 3 );

        List<HACAgent> hacAgents = this.createRandomAgents(agentCount, herosDist);
        AgentSimulator simulator = new AgentSimulator();

        simulator.simulateWithObserver( hacAgents, null,
                dt,
                fe);

    }

    public void runStepsAndRender() throws ExecutionException, InterruptedException {
        int agentCount = 100_000;
        double herosDist = 0.25;
        double dt = 0.01;
        int steps = 500;

        HACFrontend fe = new HACFrontend( 3 );

        List<HACAgent> hacAgents = this.createRandomAgents(agentCount, herosDist);
        AgentSimulator simulator = new AgentSimulator();

        ISimulationObserver stepObserver = new ISimulationObserver() {
            private int stepCounter;

            @Override
            public boolean simulationStep(LinkedHashMap dontCare) {
                stepCounter++;
                System.out.println(stepCounter);
                return stepCounter < steps;
            }
        };

        LinkedHashMap<Integer, HACAgent> agentMap = simulator.simulateWithObserver(hacAgents,
                null,
                dt,
                stepObserver );

        fe.render( agentMap );
    }

    private List<HACAgent> createRandomAgents(int count, double herosDist) {
        List<HACAgent> hacAgents = new ArrayList<>();

        // NOTE: need to create them first and then set their enemies and friends because only then all available
        for (int i = 0; i < count; ++i) {
            double x = this.rng.nextDouble();
            double y = this.rng.nextDouble();

            Vector pos = new Vector(x,y);
            boolean hero = this.rng.nextDouble() <= herosDist;

            HACAgent a = new HACAgent( pos, hero );

            hacAgents.add(a);
        }

        for (int i = 0; i < hacAgents.size(); ++i) {
            HACAgent a = hacAgents.get( i );

            HACAgent friend = RunHAC.drawRandomIgnoring(hacAgents, new HACAgent[] { a }, this.rng);
            HACAgent enemy = RunHAC.drawRandomIgnoring(hacAgents, new HACAgent[] { a, friend }, this.rng);

            a.setFriend( friend );
            a.setEnemy( enemy );
        }

        return hacAgents;
    }

    // NOTE: this method will not terminate if there is no solution. Use this one if there are many more ts than ignorings and you know there is a solution
    public static <T extends Comparable> T drawRandomIgnoring(List<T> ts, T[] ignoring, Random r) {
        int randIdx = (int)(r.nextDouble() * ts.size());
        T randElem = ts.get( randIdx );

        for ( T i : ignoring ) {
            if (randElem.equals(i)) {
                return drawRandomIgnoring(ts, ignoring, r);
            }
        }

        return randElem;
    }
}
