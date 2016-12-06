import hac.backend.agent.Agent;
import hac.backend.simulation.Simulator;
import hac.backend.simulation.WorldType;
import hac.gui.HACFrontend;

import java.io.IOException;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class Main {
    public static void main(String[] args) throws InterruptedException, IOException {
        long simulationRandomSeed = 42;

        int agentSize = 2;
        int agentCount = 10000;
        double heroesDistribution = 0.2;

        boolean randomTraversal = false;
        boolean simultaneousUpdates = true;

        WorldType wt = WorldType.BORDER;

        Simulator hac = new Simulator( simulationRandomSeed );
        HACFrontend fe = new HACFrontend( agentSize );
        List<Agent> asInit = hac.createRandomAgents(agentCount, heroesDistribution);

        // NOTE: use this code to calculate a number of steps and then display the final result
        /*
        int steps = 1;
        double dt = 0.5;
        List<List<Agent>> allAsSteps = hac.simulate(randomTraversal,
                simultaneousUpdates, wt, asInit, steps, dt);
        List<Agent> finalIteration = allAsSteps.get(allAsSteps.size() -1);
        fe.simulationStep(finalIteration, wt);
        */

        // NOTE: use this code to view simulation interactively
        hac.simulateWithObserver( randomTraversal, simultaneousUpdates, wt, asInit, fe );
        fe.dispose();

    }
}
